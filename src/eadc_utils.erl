-module(eadc_utils).
-author('jlarky@gmail.com').

-export([parse/2, reverse/1]).
-export([random_base32/1, base32/1]).

parse(simple, String) ->
    parse(simple, [], [], String).


parse(simple, Out, Buf, []) ->
    {simple, reverse([reverse(Buf)|Out])};
parse(simple, Out, Buf, [ Elem | Tail ]) ->
    case {Out,Elem} of
	{[],32} -> parse(simple, [reverse(Buf)], [], Tail);
	{ _,32} -> parse(simple, [reverse(Buf)|Out], [], Tail);
	_       -> parse(simple, Out, [Elem|Buf], Tail)
    end.

reverse(List)             -> reverse([], List).
reverse(Out, [])          -> Out;
reverse(Out, [Elem|Tail]) -> reverse([Elem|Out], Tail).


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
