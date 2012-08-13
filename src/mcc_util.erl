-module(mcc_util).

-export([app_env/3, os_env/3, verify/1, cfgget/4, cfgset/4, cfgdel/3, autoval/1]).

cfgget(Name, Key, Terms, Default) when is_atom(Name), is_atom(Key), is_list(Terms) ->
    case lists:keysearch(Name, 1, Terms) of
	false ->
	    Default;
	{value, {Name, PL}} when is_list(PL) ->
	    case lists:keysearch(Key, 1, PL) of
		false ->
		    Default;
		{value, {Key, Value}} ->
		    Value
	    end
    end.

cfgset(Name, Key, Value, Terms) when is_atom(Name), is_atom(Key), is_list(Terms) ->
    case lists:keysearch(Name, 1, Terms) of
	false ->
	    [{Name, [{Key, Value}]} | Terms];
	{value, {Name, PL}} when is_list(PL) ->
	    lists:keystore(Name, 1, Terms, {Name, lists:keystore(Key, 1, PL, {Key, Value})})
    end.

cfgdel(Name, Key, Terms) when is_atom(Name), is_atom(Key) ->
    case lists:keysearch(Name, 1, Terms) of
	false ->
	    Terms;
	{value, {Name, PL}} when is_list(PL) ->
	    NPL = case lists:keysearch(Key, 1, PL) of
		      false ->
			  PL;
		      {value, {Key, _Value}} ->
			  lists:keydelete(Key, 1, PL)
		  end,
	    if 
		length(NPL) == 0 ->
		    lists:keydelete(Name, 1, Terms);
	        true ->
		    lists:keystore(Name, 1, Terms, {Name, NPL})
	    end
    end.

autoval(Value) when is_list(Value) ->
    case catch list_to_integer(Value) of
	I when is_integer(I) ->
	    I;
	{'EXIT', {badarg, _}} ->
	    case catch list_to_float(Value) of
		F when is_float(F) ->
		    F;
		{'EXIT', {badarg, _}} ->
		    case catch list_to_existing_atom(Value) of
			A when is_atom(A) ->
			    A;
			{'EXIT', {badarg, _}} ->
			    Value
		    end
	    end
    end.

app_env(Name, Key, Default) ->
    case application:get_env(Name, Key) of
	undefined ->
	    Default;
	{ok, Value} ->
	    Value
    end.

os_env(Name, Key, Default) ->
    case os:get_env(string:to_upper(atom_to_list(Name) ++ "_" ++ atom_to_list(Key))) of
	undefined ->
	    Default;
	{ok, Value} when is_list(Value) ->
	    case catch list_to_existing_atom(Value) of
		A when is_atom(A) ->
		    A;
		{'EXIT',{badarg,_}} -> 
		    case catch list_to_integer(Value) of
			I when is_integer(I) ->
			    I;
			{'EXIT', {badarg, _}} ->
			    Value
		    end
	    end
    end.

verify([]) ->
    true;
verify([{Name, Terms}|Tail]) when is_atom(Name), is_list(Terms) ->
    case verify_namespace(Terms) of
	false ->
	    false;
	true ->
	    verify(Tail)
    end;
verify(_) ->
    false.

verify_namespace([]) ->
    true;
verify_namespace([{Key, _V}|T]) when is_atom(Key) ->
    verify_namespace(T);
verify_namespace(_) ->
    false.
