-module(mcc_sup).
-behviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_all, 2, 60},
	  [
	   {mcc_event, {mcc_event, start_link, []}, permanent, brutal_kill, worker, dynamic},
	   {mcc, {mcc, start_link, []}, permanent, brutal_kill, worker, [mcc]}
	  ]}}.
