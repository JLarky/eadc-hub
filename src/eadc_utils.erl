-module(eadc_utils).
-author('jlarky@gmail.com').

-export([convert/1]).
-export([random_base32/1, base32/1]).

convert({string, String}) ->
    convert_string (_Out=[], _Buf=[], String);
convert({list, List}) when is_list(List)->
    convert_list(_Out=[], List).

convert_string(Out, [], []) ->
    {list, Out};
convert_string(Out, Buf, [] ) ->
    convert_string(Out++[Buf], [], []);
convert_string([], Buf, [32|Tail] ) ->
    convert_string([Buf], [], Tail);
convert_string(Out, Buf, [32|Tail] ) ->
    convert_string(Out++[Buf], [], Tail);
convert_string(Out, Buf, [H|Tail] ) ->
    convert_string(Out, Buf++[H], Tail).

convert_list(Out, []) ->
    {string, Out};
convert_list( [], [H|T]) when is_list(H)->
    convert_list(H, T);
convert_list(Out, [H|T]) when is_list(H)->
    convert_list(Out++" "++H, T).

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