%% @author Jonathan Freedman
%% @copyright (c) 2012-2026 Jonathan Freedman
-module(mcc_shell).
-author('jonafree@gmail.com').
-behaviour(shellbeam).

-export([commands/0, prefix/0]).
-export([cfglist/0, cfglist/1, cfgget/2, cfgset/3, cfgflush/0]).
-export([coerce_value/2]).

prefix() -> ["config"].

commands() ->
    [
     {["list"], "List configuration namespaces", fun cfglist/0},
     {["list", {"namespace", atom}], "List configuration items in a namespace", fun cfglist/1},
     {["get", {"namespace", atom}, {"key", atom}], "Retrieve a configuration item", fun cfgget/2},
     {["set", {"namespace", atom}, {"key", atom}, {"value", auto}], "Set a configuration item", fun cfgset/3},
     {["flush"], "Flush Hot (redis) Config", fun cfgflush/0}
    ].

cfgflush() ->
    mcc:flush(),
    {ok, "Flushed.", []}.

cfglist() ->
    {ok, "Config namespaces~n" ++ begin {_, S} = lists:foldl(fun(Name, {I, A}) ->
								      {I + 1, A ++ "(" ++ integer_to_list(I) ++ ") " ++ atom_to_list(Name) ++ "~n"} end, {1, ""}, mcc:list()), S end}.

cfglist(Name) ->
    F = fun(K, {I, A}) ->
		V = mcc:get(Name, K, undefined),
                {I + 1, A ++ "(" ++ integer_to_list(I) ++ ") " ++ atom_to_list(K) ++ " : " ++ io_lib:format("~p", [V]) ++ "~n"}
        end,
    case lists:member(Name, mcc:list()) of
	true ->
	    {ok, "Config items under " ++ atom_to_list(Name) ++ "~n" ++ begin {_, S} = lists:foldl(F, {1, ""}, mcc:list(Name)), S end};
	false ->
	    {error, "Unknown namespace", []}
    end.

cfgget(Name, Key) ->
    {ok, "Config ~p:~p : ~p", [Name, Key, mcc:get(Name, Key, undefined)]}.
cfgset(Name, Key, Value) ->
    Coerced = coerce_value(Value, mcc:get(Name, Key, undefined)),
    ok = mcc:set(Name, Key, Coerced),
    {ok, "Set ~p:~p : ~p", [Name, Key, Coerced]}.

coerce_value(Value, Existing) when is_binary(Existing) ->
    to_binary(Value);
coerce_value(Value, Existing) when is_float(Existing) ->
    to_float(Value);
coerce_value(Value, Existing) when is_integer(Existing) ->
    to_integer(Value);
coerce_value(Value, _) ->
    Value.

to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(V) when is_float(V) -> float_to_binary(V, [{decimals, 10}, compact]);
to_binary(V) -> iolist_to_binary(io_lib:format("~p", [V])).

to_float(V) when is_float(V) -> V;
to_float(V) when is_integer(V) -> float(V);
to_float(V) when is_list(V) ->
    case catch list_to_float(V) of
        F when is_float(F) -> F;
        _ -> case catch list_to_integer(V) of
                 I when is_integer(I) -> float(I);
                 _ -> V
             end
    end;
to_float(V) when is_atom(V) ->
    to_float(atom_to_list(V));
to_float(V) -> V.

to_integer(V) when is_integer(V) -> V;
to_integer(V) when is_float(V) -> round(V);
to_integer(V) when is_list(V) ->
    case catch list_to_integer(V) of
        I when is_integer(I) -> I;
        _ -> V
    end;
to_integer(V) when is_atom(V) ->
    to_integer(atom_to_list(V));
to_integer(V) -> V.
