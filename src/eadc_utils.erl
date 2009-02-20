-module(eadc_utils).
-author('jlarky@gmail.com').

-export([parse/2, reverse/1]).

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
