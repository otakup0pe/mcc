-module(mcc_store_test).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% render/2 — AST generation
%%--------------------------------------------------------------------

render_basic_test() ->
    Terms = [{foo, [{bar, baz}]}],
    Forms = [
	     {attribute, 1, module, test},
	     {attribute, 3, export, [{foo, 1}]},
	     {function, 5, foo, 1, [{clause, 6, [{atom, 6, bar}], [], [{tuple, 6, [{atom, 6, ok}, {atom, 6, baz}]}]}]},
	     {eof, 7}
	    ],
    Res = mcc_store:render(test, Terms),
    ?assertEqual(Forms, Res).

%%--------------------------------------------------------------------
%% render_value/2 — value type rendering
%%--------------------------------------------------------------------

render_value_atom_test() ->
    ?assertEqual({atom, 1, foo}, mcc_store:render_value(1, foo)).

render_value_integer_test() ->
    ?assertEqual({integer, 1, 2}, mcc_store:render_value(1, 2)).

render_value_float_test() ->
    ?assertEqual({float, 1, 2.3}, mcc_store:render_value(1, 2.3)).

render_value_string_test() ->
    ?assertEqual({string, 1, "test"}, mcc_store:render_value(1, "test")).

render_value_tuple_test() ->
    ?assertEqual({tuple, 1, [{atom, 1, foo}, {integer, 1, 2}]}, mcc_store:render_value(1, {foo, 2})).

render_value_empty_list_test() ->
    ?assertEqual({nil, 1}, mcc_store:render_value(1, [])).

render_value_non_printable_list_test() ->
    Expected = {cons, 1,
        {integer, 1, 1},
        {cons, 1,
            {integer, 1, 2},
            {cons, 1,
                {integer, 1, 3},
                {nil, 1}}}},
    ?assertEqual(Expected, mcc_store:render_value(1, [1, 2, 3])).

render_value_binary_test() ->
    ?assertEqual(
        {bin, 1, [
            {bin_element, 1, {integer, 1, 104}, default, default},
            {bin_element, 1, {integer, 1, 105}, default, default}
        ]},
        mcc_store:render_value(1, <<"hi">>)).

render_value_map_test() ->
    Result = mcc_store:render_value(1, #{foo => 42}),
    ?assertMatch({map, 1, [{map_field_assoc, 1, {atom, 1, foo}, {integer, 1, 42}}]}, Result).

%%--------------------------------------------------------------------
%% render/1 + compile — full roundtrip
%%--------------------------------------------------------------------

render_compile_roundtrip_test() ->
    Terms = [{myns, [{mykey, myval}, {num, 42}]}],
    ok = mcc_store:render(Terms),
    ?assertEqual({ok, myval}, mcc_terms:myns(mykey)),
    ?assertEqual({ok, 42}, mcc_terms:myns(num)).

render_compile_roundtrip_types_test() ->
    Terms = [{app1, [{name, "hello"}, {count, 10}, {ratio, 3.14}, {flag, true}]}],
    ok = mcc_store:render(Terms),
    ?assertEqual({ok, "hello"}, mcc_terms:app1(name)),
    ?assertEqual({ok, 10}, mcc_terms:app1(count)),
    ?assertEqual({ok, 3.14}, mcc_terms:app1(ratio)),
    ?assertEqual({ok, true}, mcc_terms:app1(flag)).

render_compile_roundtrip_multi_namespace_test() ->
    Terms = [{ns1, [{k1, v1}]}, {ns2, [{k2, v2}]}],
    ok = mcc_store:render(Terms),
    ?assertEqual({ok, v1}, mcc_terms:ns1(k1)),
    ?assertEqual({ok, v2}, mcc_terms:ns2(k2)).

render_compile_roundtrip_map_test() ->
    Terms = [{app1, [{config, #{host => "localhost", port => 8080}}]}],
    ok = mcc_store:render(Terms),
    ?assertEqual({ok, #{host => "localhost", port => 8080}}, mcc_terms:app1(config)).

render_compile_roundtrip_binary_test() ->
    Terms = [{app1, [{token, <<"abc123">>}]}],
    ok = mcc_store:render(Terms),
    ?assertEqual({ok, <<"abc123">>}, mcc_terms:app1(token)).
