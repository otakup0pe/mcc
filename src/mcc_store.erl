-module(mcc_store).
-author('jonafree@gmail.com').

-include("mcc_internal.hrl").

-export([render/1]).
-export([redis_get/1, redis_set/2, redis_parse/1, overlay_read/1]).
-ifdef(TEST).
-compile(export_all).
-endif.  
-define(REDIS_VSN, 1).

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

redis_set(PID, Config) when is_pid(PID), is_list(Config) ->
    Blob = term_to_binary({mcc, ?REDIS_VSN, Config}),
    case eredis:q(PID, ["SET", "mcc", Blob]) of
	{ok, <<"OK">>} ->
	    case eredis:q(PID, ["PUBLISH", "mcc", Blob]) of
		{ok, B} ->
		    case list_to_integer(binary_to_list(B)) of
			I when is_integer(I) ->
			    ok
		    end
	    end
    end.

redis_get(undefined) ->
    [];
redis_get(PID) when is_pid(PID) ->
    case eredis:q(PID, ["GET", "mcc"]) of
	{ok, Bin} when is_binary(Bin) ->
	    redis_parse(Bin);
	{ok, undefined} ->
	    []
    end.

redis_parse(Bin) when is_binary(Bin) ->
    case binary_to_term(Bin) of
	{mcc, ?REDIS_VSN, PL} when is_list(PL) ->
	    case mcc_util:verify(PL) of
		true ->
		    PL
	    end
    end.

render(Terms) ->
%%    io:format("TERMS ~p~n", [Terms]),
    compile(render(mcc_terms, Terms)).
render(Mod, Terms) ->
    render(Terms, lists:reverse(header(Mod, Terms)), 5).
render([], Forms, Line) ->
    lists:reverse([{eof, Line + 1} | Forms]);
render([{Name, Terms}|Tail], Forms, Line) ->
    render(Tail, [{function, Line, Name, 1, render_namespace(Name, Terms, [], Line + 1)} | Forms], Line + 1).

render_namespace(_Name, [], Clauses, _Line) ->
    lists:reverse(Clauses);
render_namespace(Name, [{Key, Value} | Terms], Clauses, Line) ->
    render_namespace(Name, Terms, [{clause, Line, [{atom, Line, Key}], [], [{tuple, Line, [{atom, Line, ok}, render_value(Line, Value)]}]}|Clauses], Line + 1).

render_value(Line, Value) when is_atom(Value) ->
    io:format("render_atom ~p~n", [Value]),
    {atom, Line, Value};
render_value(Line, Value) when is_integer(Value) ->
    io:format("render_integer ~p~n", [Value]),
    {integer, Line, Value};
render_value(Line, Value) when is_float(Value) ->
    io:format("render_float ~p~n", [Value]),
    {float, Line, Value};
render_value(Line, []) ->
    io:format("render_nil~n"),
    {nil, Line};
render_value(Line, Value) when is_list(Value) ->
    io:format("render_list ~p~n", [Value]),
    case io_lib:printable_unicode_list(Value) of
	true ->
	    {string, Line, Value};
	false ->
	    render_list(Line, lists:reverse(Value))
    end;
render_value(Line, Value) when is_tuple(Value) ->
    io:format("render_tuple ~p~n", [Value]),
    {tuple, Line, lists:map(fun(A) ->
				    render_value(Line, A)
			    end, tuple_to_list(Value))};
render_value(Line, Value) when is_binary(Value) ->
    io:format("render_binary ~p~n", [Value]),
    {bin, Line, lists:map(fun(E) ->
				  {bin_element, Line, render_value(Line, E), default, default}
			  end, binary_to_list(Value))}.

render_list(Line, [H|T]) ->
    render_list(Line, T, {cons, Line, render_value(Line, H), {nil, Line}}).
render_list(_Line, [], Result) ->
    Result;
render_list(Line, [H|T], Result) ->
    render_list(Line, T, {cons, Line, render_value(Line, H), Result}).

%% extract from mod
%% {_,_,Bin}=compile:file(Mod,[debug_info,export_all,binary]), {ok,{_,[{abstract_code,{_,R}}]}} = beam_lib:chunks(Bin, [abstract_code]), 
%%                  io:format("~p~n",[R]).

header(Mod, Terms) ->
    Namespaces = lists:map(fun({Name, _}) -> {Name, 1} end, Terms),
    [
     {attribute, 1, module, Mod},
     {attribute, 3, export, Namespaces}
    ].

compile(Forms) ->
%%    io:format("FORMS ~p~n", [Forms]),
    case compile:forms(Forms) of
	{ok, Mod, Code} ->
	    code:purge(Mod),
	    code:delete(Mod),
	    {module, Mod} = code:load_binary(Mod, preloaded, Code),
	    ok
    end.
