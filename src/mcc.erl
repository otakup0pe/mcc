-module(mcc).
-behaviour(gen_server).

-export([start_link/0, start/0]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-export([set/3, get/3, list/0, list/1]).

-include("mcc.hrl").

-record(state, {os_env=[], app_env=[], config=[], redis, redis_sub, overlay, overlay_every, overlay_last, tref}).

start() ->
    lists:foreach(fun(A) ->
			  application:start(A)
		  end, [sasl, eredis, mcc]).

list() ->
    gen_server:call(?MODULE, list).

list(N) ->
    gen_server:call(?MODULE, {list, N}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    S0 = init_redis(#state{
      overlay = mcc_util:app_env(mcc, overlay_file, code:root_dir() ++ "/etc/mcc_overlay.config"),
      overlay_every = mcc_util:app_env(mcc, overlay_every, undefined)
     }),
    {ok, timer(rehash(rehash_appenv(rehash_osenv(S0))))}.

namespaces() ->
    mcc_util:app_env(mcc, namespaces, undefined).

rehash_appenv(State) ->
    State#state{app_env = case namespaces() of
			      N when is_list(N) ->
				  rehash_appenv(N, []);
			      undefined ->
				  []
			  end}.

rehash_appenv([], Config) ->
    Config;
rehash_appenv([N|T], Config) ->
    case application:get_all_env(N) of
	{ok, PL} ->
	    rehash_appenv(T, [{N, PL}|Config]);
	undefined ->
	    rehash_appenv(T, Config)
    end.

rehash_osenv(State) ->
    State#state{os_env = case namespaces() of
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
	    case string:split(Env, "=") of
		[Entry, Value] ->
		    case string:split(Entry, "_") of
			[N, Key] ->
			    mcc_util:cfgset(Namespace, list_to_atom(Key), mcc_util:autovalue(Value), Config);
			_ ->
			    Config
		    end;
		_ ->
		    Config
	    end
    end.

init_redis(#state{redis = undefined} = State) ->
    case mcc_util:app_env(mcc, redis, false) of
	false ->
	    State;
	true ->
	    {Host, Port} = {mcc_util:app_env(mcc, redis_server, "127.0.0.1"), mcc_util:app_env(mcc, redis_port, 6379)},
	    {ok, Redis} = eredis:start_link(Host, Port),
	    {ok, RedisSub} = eredis_sub:start_link(Host, Port, ""),
	    ok = eredis_sub:subscribe(self(), [<<"mcc">>]),
	    State#state{redis = Redis, redis_sub = RedisSub}
    end.

timer(#state{tref = undefined, overlay_every = FE} = State) when is_integer(FE) ->
    case timer:send_after(self(), tick, FE * 1000) of
	{ok, TRef} ->
	    State#state{tref = TRef}
    end;
timer(#state{tref = undefined, overlay_every = undefined} = State) ->
    State;
timer(#state{tref = TRef} = State) ->
    case timer:cancel(TRef) of
	ok ->
	    timer(State#state{tref = undefined})
    end.

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

handle_cast({subscribe, PID, [<<"mcc">>]}, #state{} = State) when PID == self() ->
    {noreply, State};
handle_cast({pmessage, <<"mcc">>, <<"mcc">>, Config, PID}, #state{} = State) when PID == self() ->
    {noreply, rehash(mcc_store:redis_parse(Config), State)};
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

rehash(#state{redis = R} = State) ->
    rehash(mcc_store:redis_get(R), #state{redis = R} = State).
rehash(RC0, #state{redis = Redis, config = C, os_env = OC, app_env = AC, overlay = F} = State) ->
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
    State#state{config = C2}.

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
    case mcc_util:app_env(mcc, redis, false) of
	false ->
	    {error, badarg};
	true ->
	    ok = gen_server:call(?MODULE, {set, Name, Key, Value})
    end.

p_set(Name, Key, Value, #state{config = Config, redis = Redis} = State) ->
    case mcc_util:cfgget(Name, Key, Config, undefined) of
	undefined ->
	    State;
	Value ->
	    State;
	_Other ->
	    ok = mcc_store:redis_set(Redis, mcc_util:cfgset(Name, Key, Value, mcc_store:redis_get(Redis))),
	    rehash(State)
    end.

p_list(#state{config = C}) ->
    F = fun
	    ({N, _PL}) ->
		N
	end,
    lists:map(F, C).

p_list(Name, #state{config = C}) ->
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
