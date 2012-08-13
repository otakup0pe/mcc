-module(mcc_store_test).

-include_lib("eunit/include/eunit.hrl").

render_basic_test() ->
    Terms = [{foo, [{bar, baz}]}],
    Forms = [
	     {attribute, 1, module, test},
	     {attribute, 3, export, [{foo, 1}]},
	     {function, 5, foo, 1, [{clause, 6, [{atom, 6, bar}], [], [{tuple, 6, [{atom, 6, ok}, {atom, 6, baz}]}]}]},
	     {eof, 7}
	    ],
    Res = mcc_store:render(test, Terms),
    io:format("AAA ~p", [Res]),
    ?assert(Res == Forms).

render_value_atom_test() ->
    ?assert(mcc_store:render_value(1, foo) == {atom, 1, foo}).

render_value_integer_test() ->
    ?assert(mcc_store:render_value(1, 2) == {integer, 1, 2}).

render_value_float_test() ->
    ?assert(mcc_store:render_value(1, 2.3) == {float, 1, 2.3}).

render_value_string_test() ->
    ?assert(mcc_store:render_value(1, "test") == {string, 1, "test"}).

render_value_tuple_test() ->
    ?assert(mcc_store:render_value(1, {foo, 2}) == {tuple, 1, [{atom, 1, foo}, {integer, 1, 2}]}).
