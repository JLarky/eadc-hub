%%% Solution for http://www.lshift.net/blog/2006/09/06/random-in-erlang
%%%
%%% 2006-09-13  Stefan Scholl 
%%%
%%% Public Domain (sic!)
%%%
%%% Some modifications was performed by JLarky<jlarky@gmail.com>

-module(random_p).
-export([start/0, stop/0, uniform/1, generator/0]).
-author('stesch@no-spoon.de').


%%% Start the random-generator process
start() ->
    Pid = spawn(?MODULE, generator, []),
    register(random_p_generator, Pid),
    random_p_generator ! seed.

%%% Process shutdown
stop() ->
    random_p_generator ! shutdown.

%%% The actual process with the random state
generator() ->
    receive
        seed ->
	    {A1, A2, A3} = now(),
	    random:seed(A1, A2, A3),
	    generator();
        shutdown ->
            true;
        {next_random, Pid, N, Ref} ->
            Pid ! {Ref, random:uniform(N)},
            generator()
    end.

%%% Getting the next uniform random number
uniform(N) ->
    Ref=make_ref(),
    random_p_generator ! {next_random, self(), N, Ref},
    receive
        {Ref, Random} ->
            Random
    end.
