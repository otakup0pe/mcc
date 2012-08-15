-module(mcc_store).

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
	    []
    end.

redis_set(PID, Config) when is_pid(PID), is_list(Config) ->
    case eredis:q(PID, ["SET", "mcc", term_to_binary({mcc, ?REDIS_VSN, Config})]) of
	{ok, <<"OK">>} ->
	    ok
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
    {atom, Line, Value};
render_value(Line, Value) when is_integer(Value) ->
    {integer, Line, Value};
render_value(Line, Value) when is_float(Value) ->
    {float, Line, Value};
render_value(Line, []) ->
    {nil, Line};
render_value(Line, Value) when is_list(Value) ->
    case io_lib:printable_unicode_list(Value) of
	true ->
	    {string, Line, Value};
	false ->
	    render_list(Line, lists:reverse(Value))
    end;
render_value(Line, Value) when is_tuple(Value) ->
    {tuple, Line, lists:map(fun(A) ->
				    render_value(Line, A)
			    end, tuple_to_list(Value))}.

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
    io:format("FORMS ~p~n", [Forms]),
    case compile:forms(Forms) of
	{ok, Mod, Code} ->
	    code:purge(Mod),
	    code:delete(Mod),
	    {module, Mod} = code:load_binary(Mod, preloaded, Code),
	    ok
    end.
