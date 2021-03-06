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

%% @doc Module eadc_app starts all required supervisors and sub-applications like Tiger Hash.
%% @end
-module(eadc_app).
-author('jlarky@gmail.com').

-behaviour(application).
-behaviour(supervisor).

%% Application and Supervisor callbacks
-export([start/2, stop/1, init/1]).

%% Utility for get configure options
-export([get_app_env/2, start_table/3]).

-include("eadc.hrl").

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).
-define(DEF_PORT,    4111).

%%----------------------------------------------------------------------
%% Application behaviour callbacks
%%----------------------------------------------------------------------

%% @spec start(_Type::any(), _Args::any()) -> {ok, Pid} | ignore | {error, Error}
%% @doc Prepares mnesia tables, starts tiger-hash application and starts supervisors
%% @end
start(_Type, _Args) ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    eadc_app:start_table(option, [{attributes,
				   record_info(fields, option)},
				  {disc_copies, [node()]}], []),

    random_p:start(),

    eadc_user:init(),

    error_logger:logfile({open, "error.log"}),error_logger:tty(false),

    eadc_plugin:hook(init, [{pids,[]},{data,[]}]),

    code:add_patha("../deps/tiger/ebin/"),
    spawn(application,start, [tiger]),

    ListenPort = list_to_integer(get_app_env(listen_port, integer_to_list(?DEF_PORT))),
    supervisor:start_link({local, ?MODULE}, ?MODULE, {eadc_sup, ListenPort, eadc_client}).

%% @spec stop(_S::term()) -> ok
%% @doc Application callback
stop(_S) ->
    ok.

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------

%% @spec init(Which_sup::term()) -> Result
%% Args = term()
%% Result = {ok, {{RestartStrategy, MaxR, MaxT}, [ChildSpec]}} | ignore
%% @doc Supervisor callback
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
	     %% Connect state module
	     {   eadc_connect_state,                      % Id       = internal id
		 {eadc_connect_state,start_link,[]},      % StartFun = {M, F, A}
		 temporary,                               % Restart  = permanent | transient | temporary
		 2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
		 worker,                                  % Type     = worker | supervisor
		 [eadc_connect_state]                     % Modules  = [Module] | dynamic
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
                  permanent,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [Module]                                 % Modules  = [Module] | dynamic
              }
            ]
        }
    }.

%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------

%% @spec get_app_env(atom(), term()) -> {Val | Default}
%% @doc Returns option with name 'Opt' from command line option,
%% config file or Default
%% @end
get_app_env(Opt, Default) ->
    case file:consult("eadc.cfg") of
	{ok, Data} ->
	    case lists:keysearch(Opt, 1, Data) of
		{value, {Opt,Val}} ->
		    Val;
		_ ->
		    get_app_env_(Opt, Default)
	    end;
	_ ->
	    get_app_env_(Opt, Default)	  
    end.

get_app_env_(Opt, Default) ->
    case application:get_env(application:get_application(), Opt) of
	{ok, Val} -> Val;
	_ ->
	    case init:get_argument(Opt) of
		{ok, [[Val | _]]} -> Val;
		error       -> Default
	    end
    end.

%% @spec start_table(atom(), MnesiaOptionList, OptionList) -> ok
%% @doc If table <code>TableName</code> exists just do <code>mnesia:wait_for_tables</code>
%% if Options contain {clear, true} than will be run mnesia:clear_table. MnesiaOptions will
%% be passed to <code>mnesia:create_table</code>
start_table(TableName, MnesiaOptions, Options) ->
    case lists:member(TableName, mnesia:system_info(tables)) of
	true ->
	    mnesia:wait_for_tables([TableName], 10000),
	    case eadc_utils:get_val(clear, Options) of
		true ->
		    {atomic, ok}=mnesia:clear_table(TableName),
		    ok;
		_ -> ok
	    end;
	false ->
	    {atomic, ok}=mnesia:create_table(TableName,MnesiaOptions),
	    case eadc_utils:get_val(run, Options) of
                Fun when is_function(Fun) ->
		    Fun();
		_ ->
		    ok
	    end,
	    ok
    end.
