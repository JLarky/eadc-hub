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
	  sid,       % client's SID
	  binf
	 }).

-export([test/0]).

-define(TIMEOUT, 120000).
-include("eadc.hrl").

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
	    {A,B,C}=time(), random:seed(A,B,C),
	    ok = gen_tcp:send(Socket, "ISUP ADBASE ADTIGR\n"),
	    Sid = get_unical_SID(),
	    ok = gen_tcp:send(Socket, "ISID "++Sid ++ eadc_utils:random_base32(28) ++"\n"),
	    {next_state, 'IDENTIFY STAGE', State, ?TIMEOUT};
	_ ->
	    ok = gen_tcp:send(Socket, "ISTA 240 Protocol error\n"),
	    {next_state, 'PROTOCOL STAGE', State, ?TIMEOUT}
    end;

'PROTOCOL STAGE'(timeout,  #state{socket=Socket} = State) ->
    ok = gen_tcp:send(Socket, "Protocol Error: connection timed out"),
    error_logger:info_msg("Client '~w' timed out\n", [self()]),
    {stop, normal, State}.

'IDENTIFY STAGE'({data, Data}, #state{socket=Socket}=State) ->
    ?DEBUG(debug, "String recived '~s'~n", [Data]),
    case Data of
	[$B, $I, $N, $F, $\ |_Tail] ->
	    ?DEBUG(debug, "BINF String recived '~s'~n", [Data]),
	    My_Pid=self(),
	    {list, List} = eadc_utils:convert({string, Data}),
	    [_BINF, SID | _] = List,
	    Sid = list_to_atom(SID),
	    ets:insert(eadc_clients, #client{pid=My_Pid, sid=Sid}),
	    New_State=State#state{binf=Data, sid=Sid},
	    Childs= supervisor:which_children(eadc_client_sup),
	    lists:foreach(fun({_, Pid, _, _}=_Elem) ->
				  gen_fsm:send_event(Pid, {binf, Data}),
				  if 
				      My_Pid == Pid -> dont_send;
				      true -> gen_fsm:send_event(Pid, {new_client, My_Pid})
				  end
			  end, Childs),
	    {next_state, 'IDENTIFY STAGE', New_State, ?TIMEOUT};
	_ ->
	    ok = gen_tcp:send(Socket, "ISTA 240 Protocol error\n"),
	    {next_state, 'IDENTIFY STAGE', State, ?TIMEOUT}
    end;

'IDENTIFY STAGE'({binf, Data},  State) ->
    'NORMAL STAGE'({binf, Data}, State);

'IDENTIFY STAGE'(timeout,  #state{socket=Socket} = State) ->
    ok = gen_tcp:send(Socket, "Protocol Error: connection timed out"),
    error_logger:info_msg("Client '~w' timed out\n", [self()]),
    {stop, normal, State}.


'NORMAL STAGE'({data, Data}, #state{socket=Socket} = State) ->
    ?DEBUG(debug, "DATA recived '~s'~n", [Data]),
    {list, List}=eadc_utils:convert({string, Data}),
    case List of
	[[Header|Command_name]|Tail] ->
	    client_command(list_to_atom([Header]), list_to_atom(Command_name), Tail),
	    ?DEBUG(debug, "Command recived '~s'~n", [Data]);
	_ ->
	    ok = gen_tcp:send(Socket, "ISTA 240 Protocol error\n")
    end,
    {next_state, 'NORMAL STAGE', State};

'NORMAL STAGE'({binf, Data}, #state{socket=Socket} = State) ->
    ?DEBUG(debug, "BINF event '~s'~n", [Data]),
    ok = gen_tcp:send(Socket, Data),
    {next_state, 'NORMAL STAGE', State};

'NORMAL STAGE'({inf_update, Inf_update}, #state{binf=Inf, sid=Sid} = State) ->
    ?DEBUG(debug, "BINF Update '~w'~n", [Inf_update]),
    ?DEBUG(debug, "Old BINF '~s'~n", [Inf]),
    {list, [_binf, _sid | Inf_list]} = eadc_utils:convert({string, Inf}),
    New_Inf_list=
	lists:foldl(
	  fun(Cur_Inf_Elem, Inf_Acc) ->
		  Updated_Inf_Elem = 
		      lists:foldl(
			fun(Cur_Upd_Elem, Inf_Elem_Acc) -> 
				case lists:prefix(lists:sublist(Cur_Upd_Elem, 2), Inf_Elem_Acc) of
				    true -> Cur_Upd_Elem;
				    false -> Inf_Elem_Acc
				end
			end, Cur_Inf_Elem, Inf_update),
		  [Updated_Inf_Elem|Inf_Acc]
	  end, [], lists:reverse(Inf_list)),
    {string, New_Inf} = eadc_utils:convert({list, ["BINF", atom_to_list(Sid) | New_Inf_list]}),
    ?DEBUG(debug, "New BINF '~s'~n", [New_Inf]),
    {next_state, 'NORMAL STAGE', State#state{binf=New_Inf}};

'NORMAL STAGE'({new_client, Pid}, #state{binf=BINF} = State) ->
    ?DEBUG(debug, "new_client event from ~w \n", [Pid]),
    gen_fsm:send_event(Pid, {binf, BINF}),
    {next_state, 'NORMAL STAGE', State};

'NORMAL STAGE'({master, {event, Event}}, State) ->
    master_event(Event, 'NORMAL STAGE', State);

'NORMAL STAGE'({send_to_socket, Data}, #state{socket=Socket} = State) ->
    ?DEBUG(debug, "send_to_socket event '~s'~n", [Data]),
    ok = gen_tcp:send(Socket, Data),
    {next_state, 'NORMAL STAGE', State};

'NORMAL STAGE'(Other, State) ->
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
terminate(_Reason, _StateName, #state{socket=Socket, sid=Sid}) ->
    ?DEBUG(debug, "TERMINATE ~w", [Sid]),
    (catch ets:delete(eadc_clients, Sid)),
    String_to_send = "IQUI "++ atom_to_list(Sid) ++"\n",
    lists:foreach(fun(Pid) ->
			  gen_fsm:send_event(Pid, {send_to_socket, String_to_send})
		  end, all_pids()),
    (catch gen_tcp:send(Socket, String_to_send)),
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
    ?DEBUG(debug, "Unknown event in fsm !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ~w~n", [{Event}]),
    {next_state, StateName, State}.


client_command(Header, Command, Args) ->
    {string, String}=eadc_utils:convert({list, Args}),
    String_to_send=command(Header, Command)++" "++String,
    Pids = case {Header,Command} of 
	       {'B','MSG'} ->
		   all_pids();
	       {'E', 'MSG'} ->
		   [Sid1, Sid2 | _] = Args, [get_pid_by_sid(Sid1), get_pid_by_sid(Sid2)];
	       {'D', 'RCM'} ->
		   [_Sid1, Sid2 | _] = Args, [get_pid_by_sid(Sid2)];
	       {'D', 'CTM'} ->
		   [_Sid1, Sid2 | _] = Args, [get_pid_by_sid(Sid2)];
	       {'B', 'INF'} ->
		   [_sid | Filds] = Args,
		   gen_fsm:send_event(self(), {inf_update, Filds}),
		   all_pids();
	       {'B', 'SCH'} ->
		   all_pids();
	       {'F', 'SCH'} ->
		   all_pids(); %% надо бы искать по признаку поддержки фичи
	       {'D', 'RES'} ->
		   [_Sid1, Sid2 | _] = Args, [get_pid_by_sid(Sid2)];
	       {place, holder} ->
		   ok
	   end,
    lists:foreach(fun(Pid) ->
			  gen_fsm:send_event(Pid, {send_to_socket, String_to_send})
		  end, Pids).




%%%------------------------------------------------------------------------                                                                                            
%%% Helping functions                                                                                            
%%%------------------------------------------------------------------------

command(Type, Command) ->
    atom_to_list(Type)++atom_to_list(Command).

get_unical_SID() ->
    Sid = eadc_utils:random_base32(4),
    case ets:member(eadc_clients, list_to_atom(Sid)) of
	true -> get_unical_SID();
	_ ->  Sid
    end.

get_pid_by_sid(Sid) when is_atom(Sid) ->
    case ets:lookup(eadc_clients, Sid) of
	[] ->
	    error;
	[Client] ->
	    Client#client.pid
    end;
get_pid_by_sid(Sid) when is_list(Sid)->
    get_pid_by_sid(list_to_atom(Sid)).


get_sid_by_pid(Pid) when is_pid(Pid) ->
    MS=[{{client, '$1','$2'},[{'==','$2',Pid}],['$1']}],
    case ets:select(eadc_clients, MS) of
	[] -> error;
	[Sid] -> Sid
    end.

all_pids() ->
    List=ets:match(eadc_clients, #client{_='_', pid='$1'}),
    lists:map(fun([Pid]) -> Pid end, List).


test() ->
    [Pid | _] =all_pids(),
    gen_fsm:send_event(Pid, {inf_update, ["SS419000", "SF17771"]}).

