%% @author Jonathan Freedman
%% @copyright (c) 2012-2026 Jonathan Freedman
-module(mcc_app).
-author('jonafree@gmail.com').
-behaviour(application).

-include("mcc_internal.hrl").

-export([start/2, stop/1]).

start(_Type, _Args) ->
    Then = erlang:monotonic_time(microsecond),
    {ok, PID} = mcc_sup:start_link(),
    Elapsed = (erlang:monotonic_time(microsecond) - Then) / 1000,
    ?info("mcc started in ~pms", [Elapsed]),
    {ok, PID}.

stop(_State) ->
    ok.
