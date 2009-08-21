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
