-module(mcc_shell).
-behaviour(shellbeam).

-export([commands/0]).
-export([cfglist/0, cfglist/1, cfgget/2, cfgset/3, dbgon/1, dbgoff/1]).

commands() ->
    [
     {["list"], "List configuration namespaces", fun cfglist/0},
     {["list", {"namespace", atom}], "List configuration items in a namespace", fun cfglist/1},
     {["get", {"namespace", atom}, {"key", atom}], "Retrieve a configuration item", fun cfgget/2},
     {["set", {"namespace", atom}, {"key", atom}, {"value", string}], "Set a configuration item", fun cfgset/3},
     {["dbgon", {"tag", atom}], "Enable a debug tag", fun dbgon/1},
     {["dbgoff", {"tag", atom}], "Disable a debug tag", fun dbgoff/1}
    ].

cfglist() ->
    {ok, "Config namespaces~n" ++ begin {_, S} = lists:foldl(fun(Name, {I, A}) ->
								      {I + 1, A ++ "(" ++ integer_to_list(I) ++ ") " ++ atom_to_list(Name) ++ "~n"} end, {1, ""}, mcc:list()), S end}.

cfglist(Name) ->
    F = fun({K, V}, {I, A}) ->
		
                {I + 1, A ++ "(" ++ integer_to_list(I) ++ ") " ++ atom_to_list(K) ++ " : " ++ io_lib:format("~p", [V]) ++ "~n"}
        end,
        {ok, "Config items under " ++ atom_to_list(Name) ++ "~n" ++ begin {_, S} = lists:foldl(F, {1, ""}, mcc:list(Name)), S end}.

cfgget(Name, Key) ->
    {ok, "Config ~p:~p : ~p", [Name, Key, mcc:get(Name, Key, undefined)]}.
cfgset(Name, Key, Value) ->
    ok = mcc:set(Name, Key, Value),
        {ok, "Set ~p:~p : ~p", [Name, Key, Value]}.

dbgon(Name) ->
    n(Name, true),
        {ok, "Tag ~p set", [Name]}.

dbgoff(Name) ->
    
    nomura_config:debug_set(Name, false),
        {ok, "Tag ~p unset", [Name]}.