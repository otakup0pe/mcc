-module(mcc_app).
-author('jonafree@gmail.com').
-behaviour(application).

-include("mcc_internal.hrl").

-export([start/2, stop/1]).

start(_Type, _Args) ->
    Then = now(),
    {ok, PID} = mcc_sup:start_link(),
    ?info("mcc started in ~pms", [timer:now_diff(now(), Then) / 1000]),
    {ok, PID}.

stop(_State) ->
    ok.
