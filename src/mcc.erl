-module(mcc).
-author('jonafree@gmail.com').
-behaviour(gen_server).

-export([start_link/0, start/0, rehash/0, flush/0]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-export([set/3, get/3, list/0, list/1, all/0, info/0]).
-ifdef(TEST).
-compile(export_all).
-endif.

-include("mcc_internal.hrl").

start() ->
    lists:foreach(fun(A) ->
                          application:start(A)
                  end, [sasl, yamerl, mcc]).

info() ->
    {ok, R} = gen_server:call(?MODULE, info),
    R.

rehash() ->
    gen_server:cast(?MODULE, rehash).

list() ->
    gen_server:call(?MODULE, list).

list(N) ->
    gen_server:call(?MODULE, {list, N}).

flush() ->
    gen_server:cast(?MODULE, flush).

all() ->
    all(list(), []).
all([], R) ->
    R;
all([N|T], R) ->
    all(T, [{N, lists:map(fun(K) ->
                                  {K, get(N, K, undefined)}
                          end, list(N))}|R]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    S0 = #mcc_state{
            overlay = p_overlay_file("MCC_OVERLAY_FILE", ?MCC_OVERLAY_FILE),
            yaml_overlay = p_overlay_file("MCC_YAML", ?MCC_YAML_FILE),
            overlay_every = ?MCC_OVERLAY_EVERY
           },
    {ok, timer(rehash(S0))}.

p_overlay_file(Env, File) ->
    case os:getenv(Env) of
        false ->
            File;
        F when is_list(F) ->
            F
    end.

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
    fun
        (Env, Config) ->
	    case string:tokens(Env, "=") of
		[Entry, Value] ->
		    case string:find(Entry, string:to_upper(atom_to_list(Namespace))) of
			Entry ->
			    mcc_util:cfgset(Namespace, list_to_atom(string:prefix(string:to_lower(Entry), atom_to_list(Namespace) ++ "_")), mcc_util:autoval(Value), Config);
			_ ->
			    Config
		    end;
		_ ->
		    Config
	    end
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

handle_cast(rehash, State) ->
    {noreply, p_tick(State)}.

terminate(_, _State) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

p_info(State) ->
    {ok, [
          {overlay, State#mcc_state.overlay},
          {yaml, State#mcc_state.yaml_overlay}
         ]}.

p_tick(State) ->
    timer(rehash(State)).

rehash(#mcc_state{overlay = F,
                  yaml_overlay = YF,
                  override = OVC
} = State) ->
    S0 = rehash_appenv(State),
    S1 = rehash_osenv(S0),
    #mcc_state{app_env = AC} = S1,
    MF = fun(Layer, Config) ->
                 C1 =lists:foldl(fun merge_fun/2, Config, Layer),
                 C1
         end,
    C0 = lists:foldl(MF, S1#mcc_state.config, [
					   S1#mcc_state.os_env, AC,
					   mcc_store:overlay_read(F),
					   mcc_store:yaml_read(YF),
					   OVC
					  ]),
    ok = mcc_store:render(C0),
    lists:foreach(notify_fun(S1#mcc_state.config), C0),
    S1#mcc_state{config = C0}.


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
    ok = gen_server:call(?MODULE, {set, Name, Key, Value}).

p_set(Name, Key, Value, #mcc_state{override = Override} = State) ->
    OC = if Value == undefined ->
                 mcc_util:cfgdel(Name, Key, Override);
            true ->
                 mcc_util:cfgset(Name, Key, Value, Override)
         end,
    rehash(State#mcc_state{override = OC}).

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
