-module(eadc_client_fsm).
-author('jlarky@gmail.com').

-behaviour(gen_fsm).

-export([start_link/0, set_socket/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM States
-export([
    'WAIT_FOR_SOCKET'/2,
    'WAIT_FOR_DATA'/2,
    'IDENTIFY STAGE'/2,
    'PROTOCOL STAGE'/2,
    'NORMAL STAGE'/2
]).

-record(state, {
	  socket,    % client socket
	  addr,      % client address
	  sid       % client's SID
	 }).

-define(TIMEOUT, 120000).
-define(DEBUG(Type, Format, Data), case Type of
				       debug ->
					   error_logger:info_msg(Format, Data)
				   end).

%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @spec (Socket) -> {ok,Pid} | ignore | {error,Error}
%% @doc To be called by the supervisor in order to start the server.
%%      If init/1 fails with Reason, the function returns {error,Reason}.
%%      If init/1 returns {stop,Reason} or ignore, the process is
%%      terminated and the function returns {error,Reason} or ignore,
%%      respectively.
%% @end
%%-------------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% @private
%%-------------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, 'WAIT_FOR_SOCKET', #state{}}.

%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
'WAIT_FOR_SOCKET'({socket_ready, Socket}, State) when is_port(Socket) ->
    % Now we own the socket
    error_logger:info_msg("new socket ~w\n", [{Socket, ok}]),
%%    inet:setopts(Socket, [{active, once}, {packet, 2}, binary]),
    inet:setopts(Socket, [{active, once}, {packet, line}]),
    {ok, {IP, _Port}} = inet:peername(Socket),
    {next_state, 'PROTOCOL STAGE', State#state{socket=Socket, addr=IP}, ?TIMEOUT};
'WAIT_FOR_SOCKET'(Other, State) ->
    error_logger:error_msg("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", [Other]),
    %% Allow to receive async messages
    {next_state, 'WAIT_FOR_SOCKET', State}.

'PROTOCOL STAGE'({data, Data}, #state{socket=Socket} = State) ->
    ?DEBUG(debug, "Data recived ~w~n", [Data]),
    case Data of
	[ $H,$S,$U,$P,$\  | _] ->
	    {A,B,C}=time(),
	    random:seed(A,B,C),
	    ok = gen_tcp:send(Socket, "ISUP ADBASE ADTIGR\n"),
	    ok = gen_tcp:send(Socket, "ISID "++ eadc_utils:random_base32(32) ++"\n"),
	    {next_state, 'IDENTIFY STAGE', State, ?TIMEOUT};
	_ ->
	    ok = gen_tcp:send(Socket, "ISTA 240 Protocol error\n"),
	    {next_state, 'PROTOCOL STAGE', State, ?TIMEOUT}
    end;

'PROTOCOL STAGE'(timeout,  #state{socket=Socket} = State) ->
    ok = gen_tcp:send(Socket, "Protocol Error: connection timed out"),
    error_logger:info_msg("Client '~w' timed out\n", [self()]),
    {stop, normal, State}.

'IDENTIFY STAGE'({data, Data}, #state{socket=Socket} = State) ->
    ?DEBUG(debug, "String recived '~s'~n", [Data]),
    {list, List}=eadc_utils:convert({string, Data}),
    case List of
	[[$B|"INF"]|Tail] ->
	    ?DEBUG(debug, "BINF String recived '~s'~n", [Data]),
	    %% ok = gen_tcp:send(Socket, Data),
	    %% чтобы успел поменять состояние
	    erlang:send_after(10, eadc_master, {self(), {command, {$B, 'INF', Tail}}}), 
	    {next_state, 'NORMAL STAGE', State};
	_ ->
	    ok = gen_tcp:send(Socket, "ISTA 240 Protocol error\n"),
	    {next_state, 'IDENTIFY STAGE', State, ?TIMEOUT}
    end;

'IDENTIFY STAGE'(timeout,  #state{socket=Socket} = State) ->
    ok = gen_tcp:send(Socket, "Protocol Error: connection timed out"),
    error_logger:info_msg("Client '~w' timed out\n", [self()]),
    {stop, normal, State}.


'NORMAL STAGE'({data, Data}, #state{socket=Socket} = State) ->
    ?DEBUG(debug, "DATA recived '~s'~n", [Data]),
    {list, List}=eadc_utils:convert({string, Data}),
    case List of
	[[Header|Command_name]|Tail] ->
	    %%gen_fsm:send_event(self(), {command, {Header, Command_name, Tail}}),
	    eadc_master ! {self(), {command, {list_to_atom([Header]), list_to_atom(Command_name), Tail}}},
	    ?DEBUG(debug, "Command recived '~s'~n", [Data]);
	_ ->
	    ok = gen_tcp:send(Socket, "ISTA 240 Protocol error\n")
    end,
    {next_state, 'NORMAL STAGE', State};

'NORMAL STAGE'({master, {send, Data}}, #state{socket=Socket} = State) ->
    ?DEBUG(debug, "Master says to send '~s' ~n", [Data]),
    ok = gen_tcp:send(Socket, Data),
    {next_state, 'NORMAL STAGE', State};
'NORMAL STAGE'({master, {event, Event}}, State) ->
    master_event(Event, 'NORMAL STAGE', State);

'NORMAL STAGE'(Other,  #state{socket=Socket} = State) ->
    ?DEBUG(debug, "Unknown message '~s' ~n", [Other]),
    {next_state, 'NORMAL STAGE', State}.


%% Notification event coming from client
'WAIT_FOR_DATA'({data, Data}, #state{socket=S} = State) ->
    error_logger:info_msg("rcv data: '~s'\n", [Data]),
    ok = gen_tcp:send(S, Data), %% echo
    {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};

'WAIT_FOR_DATA'(timeout, State) ->
    error_logger:error_msg("~p Client connection timeout - closing.\n", [self()]),
     {stop, normal, State};

'WAIT_FOR_DATA'(Data, State) ->
    io:format("~p Ignoring data: ~p\n", [self(), Data]),
    {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}.

%%-------------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_info({tcp, Socket, Bin}, StateName, #state{socket=Socket} = StateData) ->
    % Flow control: enable forwarding of next TCP message
    inet:setopts(Socket, [{active, once}]),
    Data = binary_to_list(Bin),
    ?MODULE:StateName({data, Data}, StateData);

handle_info({tcp_closed, Socket}, _StateName,
            #state{socket=Socket, addr=Addr} = StateData) ->
    error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Addr]),
    {stop, normal, StateData};

handle_info({master, Data}, StateName, StateData) ->
    ?MODULE:StateName({master, Data}, StateData);

handle_info(_Info, StateName, StateData) ->
    {noreply, StateName, StateData}.


%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, #state{socket=Socket}) ->
    (catch gen_tcp:close(Socket)),
    ok.

%%-------------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.


%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------
master_event(Event, StateName, #state{socket= _Socket} = State) ->
    %%case Event of
	%%{new_user, Pid} ->
	  %%  Pid ! {master, {send, ""
    ?DEBUG(debug, "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ~w~n", [{Event}]),
    {next_state, StateName, State}.
