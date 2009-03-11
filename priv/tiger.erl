-module(tiger).
-export([start/0, stop/0, init/1]).
-export([hash/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% tiger_drv - A Erlang Port_driver to use TIGER hash in Erlang
%%% Copyrught (c) 2009, JLarky <jlarky@gmail.com>
%%% 
%%% This program is free software; you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation; either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%


start() ->
    SharedLib="tiger_drv",
    case erl_ddll:load_driver("../priv", SharedLib) of
        ok ->
	    ok;
	{error, already_loaded} -> ok;
        _ -> exit({error, could_not_load_driver})
    end,
    spawn(?MODULE, init, [SharedLib]).

init(SharedLib) ->
    register(tiger, self()),
    Port = open_port({spawn, SharedLib}, []),
    loop(Port).

stop() ->
    tiger ! stop.

hash(X) when is_list(X) ->
    case lists:flatten(X) == X of
	true ->
	    call_port(X);
	_ ->
	    {bad_string, X}
    end.

call_port(Msg) ->
    tiger ! {call, self(), Msg},
    receive
        {tiger, Result} ->
            Result
    end.

loop(Port) ->
    receive
        {call, Caller, Msg} ->
            Port ! {self(), {command, Msg}},
            receive
                {Port, {data, Data}} ->
                    Caller ! {tiger, Data}
            end,
            loop(Port);
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    exit(normal)
            end;
        {'EXIT', Port, Reason} ->
            io:format("~p ~n", [Reason]),
            exit(port_terminated)
    end.
