-module(mcc_util_test).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% cfgget/4
%%--------------------------------------------------------------------

cfgget_test() ->
    ?assertEqual(baz, mcc_util:cfgget(foo, bar, [{foo, [{bar, baz}]}], undef)),
    ?assertEqual(undef, mcc_util:cfgget(foo, bar, [], undef)).

cfgget_missing_key_test() ->
    ?assertEqual(default, mcc_util:cfgget(foo, missing, [{foo, [{bar, baz}]}], default)).

cfgget_missing_namespace_test() ->
    ?assertEqual(default, mcc_util:cfgget(missing, bar, [{foo, [{bar, baz}]}], default)).

%%--------------------------------------------------------------------
%% cfgset/4
%%--------------------------------------------------------------------

cfgset_test() ->
    ?assertEqual([{foo, [{bar, baz}]}], mcc_util:cfgset(foo, bar, baz, [])),
    ?assertEqual([{foo, [{bar, baz}, {bam, ban}]}], mcc_util:cfgset(foo, bam, ban, [{foo, [{bar, baz}, {bam, ban}]}])),
    ?assertEqual([{foo, [{bar, baz}]}, {fop, [{bam, ban}]}], mcc_util:cfgset(foo, bar, baz, [{fop, [{bam, ban}]}])).

cfgset_overwrite_test() ->
    Config = [{foo, [{bar, old}]}],
    Result = mcc_util:cfgset(foo, bar, new, Config),
    ?assertEqual(new, mcc_util:cfgget(foo, bar, Result, undefined)).

%%--------------------------------------------------------------------
%% cfgdel/3
%%--------------------------------------------------------------------

cfgdel_basic_test() ->
    ?assertEqual([{foo, [{bar, baz}]}], mcc_util:cfgdel(foo, bam, [{foo, [{bar, baz}, {bam, ban}]}])),
    ?assertEqual([{fap, [{bam, ban}]}], mcc_util:cfgdel(foo, bar, [{foo, [{bar, baz}]}, {fap, [{bam, ban}]}])).

cfgdel_removes_empty_namespace_test() ->
    Config = [{foo, [{bar, baz}]}, {other, [{key, val}]}],
    Result = mcc_util:cfgdel(foo, bar, Config),
    ?assertEqual([{other, [{key, val}]}], Result).

cfgdel_nonexistent_namespace_test() ->
    Config = [{foo, [{bar, baz}]}],
    ?assertEqual(Config, mcc_util:cfgdel(missing, bar, Config)).

cfgdel_nonexistent_key_test() ->
    Config = [{foo, [{bar, baz}]}],
    ?assertEqual(Config, mcc_util:cfgdel(foo, missing, Config)).

%%--------------------------------------------------------------------
%% autoval/1
%%--------------------------------------------------------------------

autoval_test() ->
    ?assertEqual(1.5, mcc_util:autoval("1.5")),
    ?assertEqual(1, mcc_util:autoval("1")),
    ?assertEqual("hgluaghlagh", mcc_util:autoval("hgluaghlagh")),
    ?assertEqual(mcc, mcc_util:autoval("mcc")).

autoval_negative_integer_test() ->
    ?assertEqual(-42, mcc_util:autoval("-42")).

autoval_negative_float_test() ->
    ?assertEqual(-1.5, mcc_util:autoval("-1.5")).
