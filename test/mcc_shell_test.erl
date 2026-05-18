-module(mcc_shell_test).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% prefix/0
%%--------------------------------------------------------------------

prefix_returns_config_test() ->
    ?assertEqual(["config"], mcc_shell:prefix()).

%%--------------------------------------------------------------------
%% coerce_value/2 — type-match against existing value
%%--------------------------------------------------------------------

coerce_float_from_integer_test() ->
    ?assertEqual(8.0, mcc_shell:coerce_value(8, 0.4)).

coerce_float_from_atom_test() ->
    Result = mcc_shell:coerce_value('0.8', 0.4),
    ?assert(is_float(Result)),
    ?assert(abs(Result - 0.8) < 0.001).

coerce_float_from_string_test() ->
    Result = mcc_shell:coerce_value("0.8", 0.4),
    ?assert(is_float(Result)),
    ?assert(abs(Result - 0.8) < 0.001).

coerce_float_passthrough_test() ->
    ?assertEqual(0.8, mcc_shell:coerce_value(0.8, 0.4)).

coerce_binary_from_string_test() ->
    ?assertEqual(<<"hello">>, mcc_shell:coerce_value("hello", <<"world">>)).

coerce_binary_from_atom_test() ->
    ?assertEqual(<<"true">>, mcc_shell:coerce_value(true, <<"false">>)).

coerce_binary_from_binary_test() ->
    ?assertEqual(<<"foo">>, mcc_shell:coerce_value(<<"foo">>, <<"bar">>)).

coerce_integer_from_float_test() ->
    ?assertEqual(8, mcc_shell:coerce_value(8.2, 4)).

coerce_integer_from_string_test() ->
    ?assertEqual(42, mcc_shell:coerce_value("42", 10)).

coerce_integer_from_atom_test() ->
    ?assertEqual(42, mcc_shell:coerce_value('42', 10)).

coerce_integer_passthrough_test() ->
    ?assertEqual(5, mcc_shell:coerce_value(5, 10)).

coerce_no_existing_value_test() ->
    ?assertEqual("hello", mcc_shell:coerce_value("hello", undefined)).

coerce_atom_passthrough_test() ->
    ?assertEqual(hello, mcc_shell:coerce_value(hello, undefined)).
