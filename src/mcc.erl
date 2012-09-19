-module(mcc).
-author('jonafree@gmail.com').
-behaviour(gen_server).

-export([start_link/0, start/0, rehash/0, flush/0]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-export([set/3, get/3, list/0, list/1]).
-ifdef(TEST).
-compile(export_all).
-endif.  

-include("mcc_internal.hrl").

start() ->
    lists:foreach(fun(A) ->
			  application:start(A)
		  end, [sasl, eredis, mcc]).

rehash() ->
    gen_server:cast(?MODULE, rehash).

list() ->
    gen_server:call(?MODULE, list).

list(N) ->
    gen_server:call(?MODULE, {list, N}).

flush() ->
    gen_server:cast(?MODULE, flush).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    S0 = init_redis(#mcc_state{
      overlay = ?MCC_OVERLAY_DIR ++ ?MCC_OVERLAY_FILE,
      overlay_every = ?MCC_OVERLAY_EVERY
     }),
    {ok, timer(rehash(rehash_appenv(rehash_osenv(S0))))}.

rehash_appenv(State) ->
    State#mcc_state{app_env = case ?MCC_NAMESPACES of
			      N when is_list(N) ->
				  rehash_appenv(N, []);
			      undefined ->
				  []
			  end}.

rehash_appenv([], Config) ->
    Config;
rehash_appenv([N|T], Config) ->
    case application:get_all_env(N) of
	[] ->
	    rehash_appenv(T, Config);
	PL when is_list(PL) ->
	    case lists:keydelete(included_applications, 1, PL) of
		[] ->
		    rehash_appenv(T, Config);
		PL0 when is_list(PL0) ->
		    rehash_appenv(T, [{N, PL0}|Config])
	    end
    end.

rehash_osenv(State) ->
    State#mcc_state{os_env = case ?MCC_NAMESPACES of
			     N when is_list(N) ->
				 rehash_osenv(N, os:getenv(), []);
			     undefined ->
				 []
			 end}.
rehash_osenv([], _Env, Config) ->
    Config;
rehash_osenv([N|T], Env, Config) ->
    rehash_osenv(T, Env, lists:foldl(rehash_osenv_fun(N), Config, Env)).

rehash_osenv_fun(Namespace) ->
    N = string:to_upper(atom_to_list(Namespace)),
    fun
	(Env, Config) ->
	    case string:tokens(Env, "=") of
		[Entry, Value] ->
		    case string:tokens(Entry, "_") of
			[N| Key] ->
			    mcc_util:cfgset(Namespace, list_to_atom(string:to_lower(string:join(Key, "_"))), mcc_util:autoval(Value), Config);
			_ ->
			    Config
		    end;
		_ ->
		    Config
	    end
    end.

init_redis(#mcc_state{redis = undefined} = State) ->
    case ?MCC_REDIS of
	false ->
	    State;
	true ->
	    {Host, Port} = {?MCC_REDIS_SERVER, ?MCC_REDIS_PORT},
	    {ok, Redis} = eredis:start_link(Host, Port),
	    {ok, RedisSub} = eredis_sub:start_link(Host, Port, ""),
	    ok = eredis_sub:controlling_process(RedisSub),
	    ok = eredis_sub:subscribe(RedisSub, [<<"mcc">>]),
	    State#mcc_state{redis = Redis, redis_sub = RedisSub}
    end.

timer(#mcc_state{tref = undefined, overlay_every = FE} = State) when is_integer(FE) ->
    case timer:send_after(self(), tick, FE * 1000) of
	{ok, TRef} ->
	    State#mcc_state{tref = TRef}
    end;
timer(#mcc_state{tref = undefined, overlay_every = undefined} = State) ->
    State;
timer(#mcc_state{tref = TRef} = State) ->
    case timer:cancel(TRef) of
	ok ->
	    timer(State#mcc_state{tref = undefined})
    end.

handle_info({subscribed, <<"mcc">>, PID}, #mcc_state{redis_sub = PID} = State) ->
    eredis_sub:ack_message(PID),
    {noreply, State};
handle_info({message, <<"mcc">>, Config, PID}, #mcc_state{redis_sub = PID} = State) ->
    eredis_sub:ack_message(PID),
    {noreply, rehash(mcc_store:redis_parse(Config), State)};
handle_info(rehash, State) ->
    {noreply, rehash(State)}.

handle_call({set, Name, Key, Value}, _From, State) ->
    {reply, ok, p_set(Name, Key, Value, State)};
handle_call(list, _From, State) ->
    {reply, p_list(State), State};
handle_call({list, Name}, _From, State) ->
    {reply, p_list(Name, State), State};
handle_call(info, _From, State) ->
    {reply, p_info(State), State}.

handle_cast(flush, #mcc_state{redis = Redis} = State) ->
    mcc_store:redis_set(Redis, []),
    {noreply, State};
handle_cast(rehash, State) ->
    {noreply, p_tick(State)}.

terminate(_, _State) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

p_info(State) ->
    State.

p_tick(State) ->
    timer(rehash(State)).

rehash(#mcc_state{redis = R} = State) ->
    rehash(mcc_store:redis_get(R), #mcc_state{redis = R} = State).
rehash(RC0, #mcc_state{redis = Redis, config = C, os_env = OC, app_env = AC, overlay = F} = State) ->
    C0 = lists:foldl(fun merge_fun/2, AC, mcc_store:overlay_read(F)), % merge overlay
    RC = lists:foldl(fun redis_clean_fun/2, RC0, C0),
    if
	RC /= RC0 ->
	    mcc_store:redis_set(Redis, RC);
	true ->
	    ok
    end,
    C1 = lists:foldl(fun merge_fun/2, C0, RC), % merge redis
    C2 = lists:foldl(fun merge_fun/2, C1, OC), % merge os env
    ok = mcc_store:render(C2),
    lists:foreach(notify_fun(C), C2),
    State#mcc_state{config = C2}.

redis_clean_namespace_fun(N) ->
    fun
	({K, V}, Config) ->
	    case mcc_util:cfgget(N, K, Config, undefined) of
		undefined ->
		    Config;
		V ->
		    mcc_util:cfgdel(N, K, Config);
		_O ->
		    Config
	    end
    end.
redis_clean_fun({N, PL}, Config) ->
    lists:foldl(redis_clean_namespace_fun(N), Config, PL).

merge_namespace_fun(Name) ->
    fun
	({K, V}, Config) ->
	    mcc_util:cfgset(Name, K, V, Config)
    end.
merge_fun({N, PL}, Config) ->
    lists:foldl(merge_namespace_fun(N), Config, PL).

notify_namespace_fun(Name, OldConfig) ->
    fun
	({K, V}) ->
	    case mcc_util:cfgget(Name, K, OldConfig, undefined) of
		V ->
		    ok;
		Other ->
		    mcc_event:notify(#mcc{name = Name, key = K, value = V, old_value = Other})
	    end
    end.

notify_fun(OldConfig) ->
    fun
	({Name, PL}) ->
	    lists:foreach(notify_namespace_fun(Name, OldConfig), PL)
    end.

get(Name, Key, Default) when is_atom(Name), is_atom(Key) ->
    case catch mcc_terms:Name(Key) of
	{ok, Value} ->
	    Value;
	{'EXIT', {function_clause, _}} ->
	    Default;
	{'EXIT', {undef, _}} ->
	    Default
    end.

set(Name, Key, Value) when is_atom(Name), is_atom(Key) ->
    case ?MCC_REDIS of
	false ->
	    {error, badarg};
	true ->
	    ok = gen_server:call(?MODULE, {set, Name, Key, Value})
    end.

p_set(Name, Key, Value, #mcc_state{config = Config, redis = Redis} = State) ->
    case mcc_util:cfgget(Name, Key, Config, undefined) of
	Value ->
	    State;
	_Other ->
	    ok = mcc_store:redis_set(Redis, mcc_util:cfgset(Name, Key, Value, mcc_store:redis_get(Redis))),
	    rehash(State)
    end.

p_list(#mcc_state{config = C}) ->
    F = fun
	    ({N, _PL}) ->
		N
	end,
    lists:map(F, C).

p_list(Name, #mcc_state{config = C}) ->
    F = fun
	    ({K, _V}) ->
		K
	end,
    case lists:keysearch(Name, 1, C) of
	false ->
	    {error, badarg};
	 {value, {Name, PL}} ->
	    lists:map(F, PL)
    end.
