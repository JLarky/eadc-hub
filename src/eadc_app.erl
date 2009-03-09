-module(eadc_app).
-author('jlarky@gmail.com').

-behaviour(application).
-behaviour(supervisor).

%% Internal API
-export([start_client/0]).

%% Application and Supervisor callbacks
-export([start/2, stop/1, init/1]).

-include("eadc.hrl").

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).
-define(DEF_PORT,    4111).

%% A startup function for spawning new client connection handling FSM.
%% To be called by the TCP listener process.
start_client() ->
    supervisor:start_child(eadc_client_sup, []).

%%----------------------------------------------------------------------
%% Application behaviour callbacks
%%----------------------------------------------------------------------
start(_Type, _Args) ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    T=client,
    mnesia:wait_for_tables([T], 10000),
    case lists:member(T, mnesia:system_info(tables)) of
	true ->
	    mnesia:clear_table(T);
	false ->
	    mnesia:create_table(T,
				[{attributes, 
				  record_info(fields, client)},
				 {disc_copies, [node()]}])
    end,
    
    error_logger:logfile({open, 'error.log'}),
    error_logger:tty(false),
    eadc_plugin:hook(init, [{pids,[]},{data,[]}]),
    ListenPort = list_to_integer(get_app_env(listen_port, integer_to_list(?DEF_PORT))),
    supervisor:start_link({local, ?MODULE}, ?MODULE, {eadc_sup, ListenPort, eadc_client_fsm}).

stop(_S) ->
    ok.

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
init({eadc_sup, Port, Module}) ->
    {ok,
        {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
	 [
	     %% TCP Listener
	     {   eadc_sup,                                 % Id       = internal id
		 {eadc_listener,start_link,[Port,Module]},% StartFun = {M, F, A}
		 permanent,                               % Restart  = permanent | transient | temporary
		 2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
		 worker,                                  % Type     = worker | supervisor
		 [eadc_listener]                          % Modules  = [Module] | dynamic
		},
	     %% Master module
	     {   eadc_master,                             % Id       = internal id
		 {eadc_master, start_link,[]},            % StartFun = {M, F, A}
		 permanent,                               % Restart  = permanent | transient | temporary
		 2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
		 worker,                                  % Type     = worker | supervisor
		 [eadc_master]                            % Modules  = [Module] | dynamic
		},
	     %% Client instance supervisor
	     {   eadc_client_sup,
		 {supervisor,start_link,[{local, eadc_client_sup}, ?MODULE, [Module]]},
		 permanent,                               % Restart  = permanent | transient | temporary
		 infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
		 supervisor,                              % Type     = worker | supervisor
		 []                                       % Modules  = [Module] | dynamic
		}
            ]
        }
    };

init([Module]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Client
              {   undefined,                               % Id       = internal id
                  {Module,start_link,[]},                  % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    }.

%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------
get_app_env(Opt, Default) ->
    case application:get_env(application:get_application(), Opt) of
	{ok, Val} -> Val;
	_ ->
	    case init:get_argument(Opt) of
		{ok, [[Val | _]]} -> Val;
		error       -> Default
	    end
    end.
