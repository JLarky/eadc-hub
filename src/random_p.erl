%%% Solution for http://www.lshift.net/blog/2006/09/06/random-in-erlang
%%%
%%% 2006-09-13  Stefan Scholl 
%%%
%%% Public Domain (sic!)
%%%

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
        {next_random, Pid, N} ->
            Pid ! random:uniform(N),
            generator()
    end.

%%% Getting the next uniform random number
uniform(N) ->
    random_p_generator ! {next_random, self(), N},
    receive
        Random ->
            Random
    end.
