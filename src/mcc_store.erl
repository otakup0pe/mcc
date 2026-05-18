%% @author Jonathan Freedman
%% @copyright (c) 2012-2026 Jonathan Freedman
-module(mcc_store).
-author('jonafree@gmail.com').

-include("mcc_internal.hrl").
-include_lib("yamerl/include/yamerl_errors.hrl").

-export([render/1]).
-export([overlay_read/1, yaml_read/1]).
-ifdef(TEST).
-compile(export_all).
-endif.

yaml_read(undefined) ->
    [];
yaml_read(File) when is_list(File) ->
    try yamerl_constr:file(File) of
        [L] when is_list(L) ->
            F = fun({K, V}) when is_list(K) ->
                         {list_to_atom(K), V}
                 end,
            UF = fun({K, V}) when is_list(K), is_list(V) ->
                         {list_to_atom(K), lists:map(F, V)}
                 end,
            lists:map(UF, L)
    catch
        throw:#yamerl_exception{errors=[#yamerl_parsing_error{name = file_open_failure}]} ->
            []
    end.

overlay_read(undefined) ->
    [];
overlay_read(File) ->
    case file:consult(File) of
        {ok, Overlay} ->
            Overlay;
        {error, enoent} ->
            [];
        {error, {Line, erl_parse, M}} ->
            ?error("unable to read overlay line ~p : ~s", [Line, lists:flatten(M)]),
            []
    end.

render(Terms) ->
    compile(render(mcc_terms, Terms)).
render(Mod, Terms) ->
    render(Terms, lists:reverse(header(Mod, Terms)), 5).
render([], Forms, Line) ->
    lists:reverse([{eof, a(Line + 1)} | Forms]);
render([{Name, Terms}|Tail], Forms, Line) ->
    render(Tail, [{function, a(Line), Name, 1, render_namespace(Name, Terms, [], Line + 1)} | Forms], Line + 1).

render_namespace(_Name, [], Clauses, _Line) ->
    lists:reverse(Clauses);
render_namespace(Name, [{Key, Value} | Terms], Clauses, Line) ->
    render_namespace(Name, Terms, [{clause, a(Line), [{atom, a(Line), Key}], [], [{tuple, a(Line), [{atom, a(Line), ok}, render_value(Line, Value)]}]}|Clauses], Line + 1).

render_value(Line, Value) when is_atom(Value) ->
    {atom, a(Line), Value};
render_value(Line, Value) when is_integer(Value) ->
    {integer, a(Line), Value};
render_value(Line, Value) when is_float(Value) ->
    {float, a(Line), Value};
render_value(Line, []) ->
    {nil, a(Line)};
render_value(Line, Value) when is_list(Value) ->
    case io_lib:printable_unicode_list(Value) of
	true ->
	    {string, a(Line), Value};
	false ->
	    render_list(Line, lists:reverse(Value))
    end;
render_value(Line, Value) when is_tuple(Value) ->
    {tuple, a(Line), lists:map(fun(A) ->
				    render_value(Line, A)
			    end, tuple_to_list(Value))};
render_value(Line, Value) when is_binary(Value) ->
    {bin, a(Line), lists:map(fun(E) ->
				  {bin_element, a(Line), render_value(Line, E), default, default}
			  end, binary_to_list(Value))};
render_value(Line, Value) when is_map(Value) ->
    Pairs = maps:to_list(Value),
    {map, a(Line), lists:map(fun({K, V}) ->
        {map_field_assoc, a(Line), render_value(Line, K), render_value(Line, V)}
    end, Pairs)}.

render_list(Line, [H|T]) ->
    render_list(Line, T, {cons, a(Line), render_value(Line, H), {nil, a(Line)}}).
render_list(_Line, [], Result) ->
    Result;
render_list(Line, [H|T], Result) ->
    render_list(Line, T, {cons, a(Line), render_value(Line, H), Result}).

%% extract from mod
%% {_,_,Bin}=compile:file(Mod,[debug_info,export_all,binary]), {ok,{_,[{abstract_code,{_,R}}]}} = beam_lib:chunks(Bin, [abstract_code]),
%%                  io:format("~p~n",[R]).

header(Mod, Terms) ->
    Namespaces = lists:map(fun({Name, _}) -> {Name, 1} end, Terms),
    [
     {attribute, a(1), module, Mod},
     {attribute, a(3), export, Namespaces}
    ].

compile(Forms) ->
    case compile:forms(Forms) of
	{ok, Mod, Code} ->
	    code:purge(Mod),
	    code:delete(Mod),
	    {module, Mod} = code:load_binary(Mod, preloaded, Code),
	    ok
    end.

a(Line) -> erl_anno:new(Line).
