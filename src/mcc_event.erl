-module(mcc_event).
-behaviour(gen_event).
-export([start_link/0,behaviour_info/1]).
-export([add_handler/2,delete_handler/1,notify/1]).
-export([init/1, terminate/2, handle_event/2, handle_call/2, handle_info/2, code_change/3]).

-include("mcc.hrl").

-record(state, {mod, state}).

behaviour_info(callbacks) ->
    [
     {init,1},
     {terminate,1},
     {update,2}
    ].

start_link() ->
    gen_event:start_link({local, ?MODULE}).

add_handler(Mod, Args) ->
    gen_event:add_sup_handler(?MODULE, {?MODULE, Mod}, [Mod | Args]).

delete_handler(Mod) ->
    gen_event:delete_handler(?MODULE, {?MODULE, Mod}).

notify(#mcc{} = MCC) ->
    ok = gen_event:notify(?MODULE, MCC).

init([Mod|Args]) ->
    case Mod:init(Args) of
	{ok, State} ->
	    {ok, #state{mod = Mod, state = State}}
    end.

terminate(_R, #state{mod = Mod, state = CBState}) ->
    Mod:terminate(CBState),
    ok.

handle_call(_, State) ->
    {ok, State}.

handle_info(_, State) ->
    {ok, State}.

handle_event({mcc, M}, State) ->
    {ok, handle_mcc_event(M, State)}.

code_change(_, State, _) ->
    {ok, State}.

handle_mcc_event(#mcc{} = MCC, #state{mod = M, state = CBState} = State) ->
    case M:update(MCC, CBState) of
	{ok, NewCBState} ->
	    State#state{state = NewCBState}
    end.
