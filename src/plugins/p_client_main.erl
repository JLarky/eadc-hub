%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% eadc-hub - ADC hub software written using Erlang/OTP.
%%% Copyright (c) 2010, JLarky <jlarky@gmail.com>
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

%%%-------------------------------------------------------------------
%%% File    : p_client_main.erl
%%% Author  : JLarky <jlarky@gmail.com>
%%% Description : Sup
%%%
%%% Created : 29 Jul 2009 by JLarky <jlarky@gmail.com>
%%%-------------------------------------------------------------------
-module(p_client_main).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    %%supervisor:start_link({local, ?SERVER}, ?MODULE, [listener]).
    io:format("p_client_main:start_link() ~p\n", [self()]),
    supervisor:start_link({local, fnjkdnfjkvndfk}, ?MODULE, [listener]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([listener]) ->
    io:format("p_client_main:init([listener]) ~p\n", [self()]),
    ListenChild = {p_client_listen,{eadc_listener,start_link,[4112, p_client_fsm]},
	      permanent,2000,worker,[p_client_fsm]},
    %%{ok,{{one_for_all,5,60}, [ListenChild]}}.
    A={p_client_listenee,{eadc_listener,start_link,[4112, p]},
              permanent,2000,worker,[]},
    {ok,{{one_for_all,5,60}, [A]}}.

%%====================================================================
%% Internal functions
%%====================================================================
