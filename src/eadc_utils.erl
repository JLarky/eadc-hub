-module(eadc_utils).
-author('jlarky@gmail.com').

-export([convert/1, quote/1]).
-export([random_base32/1, base32/1]).

-export([code_reload/1]).


convert({string, String}) ->
    convert_string (String);
convert({list, List}) when is_list(List) ->
    convert_list(_Out=[], List).

convert_string(String) ->
    {Bu, Ac} = lists:foldr(fun(Char, {Buf, Acc}) ->
			 case Char of
			     $\  -> {[], [Buf|Acc]};
			     _ -> {[Char | Buf], Acc}
			 end
		 end, {[],[]}, String), {list, [Bu|Ac]}.


convert_list(Out, []) ->
    {string, Out};
convert_list( [], [H|T]) when is_list(H)->
    convert_list(quote(H), T);
convert_list(Out, [H|T]) when is_list(H)->
    convert_list(Out++" "++quote(H), T).

quote(String) ->
    lists:foldl(fun(Char, Acc) -> case Char of
				      $\ ->
					  Acc++"\\s";
				      $\\ ->
					  Acc++"\\\\";
				      _ ->
					  Acc++[Char]
				  end end, [], String).

random_base32(Count) ->
    random_base32(Count, []).
random_base32(0, Output) ->
    Output;
random_base32(Count, Output) ->
    random_base32(Count-1, Output)++base32(random:uniform(32)-1).


base32(V) ->
    if
	V < 0 -> error;
	V < 26 -> [V+65];
	V > 25 -> [V+24]; % V-26+48+2
	true -> V
    end.

code_reload(Module) ->
    error_logger:info_msg("~s", [os:cmd("cd .. && make")]),
    true = code:soft_purge(Module),
    code:load_file(Module).

