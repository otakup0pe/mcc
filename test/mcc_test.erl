-module(mcc_test).

-include_lib("eunit/include/eunit.hrl").
-include("mcc.hrl").

%%--------------------------------------------------------------------
%% merge_fun/2 — config layer merging
%%--------------------------------------------------------------------

merge_fun_new_namespace_test() ->
    Config = [{app1, [{key1, val1}]}],
    Result = mcc:merge_fun({app2, [{key2, val2}]}, Config),
    ?assertEqual([{app2, [{key2, val2}]}, {app1, [{key1, val1}]}], Result).

merge_fun_add_key_test() ->
    Config = [{app1, [{key1, val1}]}],
    Result = mcc:merge_fun({app1, [{key2, val2}]}, Config),
    ?assertEqual([{app1, [{key1, val1}, {key2, val2}]}], Result).

merge_fun_override_test() ->
    Config = [{app1, [{key1, val1}]}],
    Result = mcc:merge_fun({app1, [{key1, new_val}]}, Config),
    ?assertEqual([{app1, [{key1, new_val}]}], Result).

merge_fun_empty_config_test() ->
    Result = mcc:merge_fun({app1, [{key1, val1}]}, []),
    ?assertEqual([{app1, [{key1, val1}]}], Result).

merge_fun_multiple_keys_test() ->
    Config = [{app1, [{key1, val1}]}],
    Result = mcc:merge_fun({app1, [{key2, val2}, {key3, val3}]}, Config),
    ?assertEqual([{app1, [{key1, val1}, {key2, val2}, {key3, val3}]}], Result).

%%--------------------------------------------------------------------
%% config layering — later layers override earlier ones
%%--------------------------------------------------------------------

merge_layering_test() ->
    AppEnv = [{myapp, [{host, "default.local"}, {port, 80}]}],
    Overlay = [{myapp, [{port, 8080}]}],
    Override = [{myapp, [{host, "override.local"}]}],
    MF = fun(Layer, Config) ->
        lists:foldl(fun mcc:merge_fun/2, Config, Layer)
    end,
    Result = lists:foldl(MF, [], [AppEnv, Overlay, Override]),
    ?assertEqual("override.local", mcc_util:cfgget(myapp, host, Result, undefined)),
    ?assertEqual(8080, mcc_util:cfgget(myapp, port, Result, undefined)).

merge_layering_preserves_unoverridden_test() ->
    Base = [{app1, [{a, 1}, {b, 2}]}, {app2, [{c, 3}]}],
    Layer = [{app1, [{b, 20}]}],
    MF = fun(L, C) -> lists:foldl(fun mcc:merge_fun/2, C, L) end,
    Result = lists:foldl(MF, [], [Base, Layer]),
    ?assertEqual(1, mcc_util:cfgget(app1, a, Result, undefined)),
    ?assertEqual(20, mcc_util:cfgget(app1, b, Result, undefined)),
    ?assertEqual(3, mcc_util:cfgget(app2, c, Result, undefined)).

%%--------------------------------------------------------------------
%% rehash_osenv_fun/1 — OS environment variable parsing
%%--------------------------------------------------------------------

rehash_osenv_fun_match_test() ->
    F = mcc:rehash_osenv_fun(myapp),
    Result = F("MYAPP_PORT=8080", []),
    ?assertEqual([{myapp, [{port, 8080}]}], Result).

rehash_osenv_fun_autoval_test() ->
    F = mcc:rehash_osenv_fun(myapp),
    Result = F("MYAPP_DEBUG=true", []),
    ?assertEqual([{myapp, [{debug, true}]}], Result).

rehash_osenv_fun_no_match_test() ->
    F = mcc:rehash_osenv_fun(myapp),
    Result = F("OTHER_PORT=8080", []),
    ?assertEqual([], Result).

rehash_osenv_fun_no_equals_test() ->
    F = mcc:rehash_osenv_fun(myapp),
    Result = F("MALFORMED", []),
    ?assertEqual([], Result).

rehash_osenv_fun_accumulates_test() ->
    F = mcc:rehash_osenv_fun(myapp),
    R1 = F("MYAPP_HOST=localhost", []),
    R2 = F("MYAPP_PORT=3000", R1),
    ?assertEqual("localhost", mcc_util:cfgget(myapp, host, R2, undefined)),
    ?assertEqual(3000, mcc_util:cfgget(myapp, port, R2, undefined)).
