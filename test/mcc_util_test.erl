-module(mcc_util_test).

-include_lib("eunit/include/eunit.hrl").

cfgget_test() ->
    ?assert(baz == mcc_util:cfgget(foo, bar, [{foo, [{bar, baz}]}], undef)),
    ?assert(undef == mcc_util:cfgget(foo, bar, [], undef)).

cfgset_test() ->
    ?assert([{foo, [{bar, baz}]}] == mcc_util:cfgset(foo, bar, baz, [])),
    ?assert([{foo, [{bar, baz}, {bam, ban}]}] == mcc_util:cfgset(foo, bam, ban, [{foo, [{bar, baz}, {bam, ban}]}])),
    ?assert([{foo, [{bar, baz}]}, {fop, [{bam, ban}]}] == mcc_util:cfgset(foo, bar, baz, [{fop, [{bam, ban}]}])).

cfgdel_basic_test() ->
    ?assert([{foo, [{bar, baz}]}] == mcc_util:cfgdel(foo, bam, [{foo, [{bar, baz}, {bam, ban}]}])),
    ?assert([{fap, [{bam, ban}]}] == mcc_util:cfgdel(foo, bar, [{foo, [{bar, baz}]}, {fap, [{bam, ban}]}])).

autoval_test() ->
    ?assert(1.5 == mcc_util:autoval("1.5")),
    ?assert(1 == mcc_util:autoval("1")),
    ?assert("hgluaghlagh" == mcc_util:autoval("hgluaghlagh")),
    ?assert(mcc == mcc_util:autoval("mcc")).
