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
	 'IDENTIFY STAGE'/2,
	 'PROTOCOL STAGE'/2,
	 'NORMAL STAGE'/2
	]).

%% HELPING FUNCTIONS
-export([all_pids/0]).

%% DEBUG
-export([test/1, get_sid_by_pid/1,get_unical_cid/1]).

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
    {ok, 'WAIT_FOR_SOCKET', #state{buf=[]}}.

%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
'WAIT_FOR_SOCKET'({socket_ready, Socket}, State) when is_port(Socket) ->
    %% Now we own the socket
    error_logger:info_msg("new socket ~w\n", [{Socket, ok}]),

    inet:setopts(Socket, [{active, once}, {packet, line}]),
    {ok, {IP, _Port}} = inet:peername(Socket),
    {next_state, 'PROTOCOL STAGE', State#state{socket=Socket, addr=IP}, ?TIMEOUT};
'WAIT_FOR_SOCKET'(Other, State) ->
    error_logger:error_msg("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", [Other]),
    %% Allow to receive async messages
    {next_state, 'WAIT_FOR_SOCKET', State}.

'PROTOCOL STAGE'({data, Data}, #state{socket=Socket} = State) ->
    ?DEBUG(debug, "Data recived '~s'~n", [Data]),
    case Data of
	[ $H,$S,$U,$P,$\  | _] ->
	    {A,B,C}=time(), random:seed(A,B,C),
	    ok = gen_tcp:send(Socket, "ISUP ADBAS0 ADBASE ADTIGR ADUCM0 ADUCMD\n"),
	    Sid = get_unical_SID(),
	    ok = gen_tcp:send(Socket, "ISID "++Sid ++"\n"),
	    {next_state, 'IDENTIFY STAGE', State#state{sid=list_to_atom(Sid)}, ?TIMEOUT};
	_ ->
	    ok = gen_tcp:send(Socket, "ISTA 240 Protocol error\n"),
	    {next_state, 'PROTOCOL STAGE', State, ?TIMEOUT}
    end;

'PROTOCOL STAGE'(timeout,  #state{socket=Socket} = State) ->
    ok = gen_tcp:send(Socket, "Protocol Error: connection timed out\n"),
    error_logger:info_msg("Client '~w' timed out\n", [self()]),
    {stop, normal, State}.

'IDENTIFY STAGE'({data, Data}, #state{socket=Socket, addr=Addr, sid=Sid}=State) ->
    ?DEBUG(debug, "String recived '~s'~n", [Data]),
    {list, List} = eadc_utils:convert({string, Data}),
    case List of
	["BINF", SID | _] ->
	    case list_to_atom(SID) == Sid of
		false ->
		    gen_tcp:send(Socket,"ISTA 240 "++eadc_utils:quote("SID is not correct")++"\n"),
		    {stop, normal, State};	    
		true ->
		    ?DEBUG(debug, "New client with BINF= '~s'~n", [Data]),
		    My_Pid=self(), Sid = list_to_atom(SID),
		    {I1,I2,I3,I4} = Addr,
		    P_Inf=eadc_utils:parse_inf(Data),
		    Nick=case lists:keysearch('NI', 1, P_Inf) of
			     {value,{'NI', Nick_}} -> Nick_;
			     _ -> "[Unknown]"++eadc_utils:random_base32(5)
			 end,		
		    {value,{'ID', Cid_f}}  = lists:keysearch('ID', 1, P_Inf),
		    Cid=get_unical_cid(Cid_f),

		    Inf=inf_update(Data, [lists:concat(["I4",I1,".",I2,".",I3,".",I4]),"PD","ID"++Cid, "NI"++Nick]),

		    New_State=State#state{inf=Inf, nick=Nick},
		    Other_clients = all_pids(), %% важно, что перед операцией записи
		    Args=[{pids,Other_clients},{data,Inf},{sid,SID},{pid,My_Pid},
			  {nick, Nick}, {inf, Inf}, {state,State}],
		    {Pids_to_inform, Data_to_send}=eadc_plugin:hook(user_login, Args),
		    ets:insert(eadc_clients, #client{pid=My_Pid, sid=Sid, nick=Nick, cid=Cid}),
		    lists:foreach(fun(Pid) ->
					  gen_fsm:send_event(Pid, {new_client, My_Pid})
				  end, Pids_to_inform),
		    eadc_utils:send_to_pids([self()| Pids_to_inform], Data_to_send),
		    %% gen_fsm:send_event(My_Pid, {send_to_socket, "IGPA A\n"})
		    {next_state, 'NORMAL STAGE', New_State, ?TIMEOUT}
	    end;
	_ ->
	    ok = gen_tcp:send(Socket, "ISTA 240 Protocol error\n"),
	    {next_state, 'IDENTIFY STAGE', State, ?TIMEOUT}
    end;

'IDENTIFY STAGE'({send_to_socket, Data}, State) ->
    'NORMAL STAGE'({send_to_socket, Data}, State);

'IDENTIFY STAGE'(timeout,  #state{socket=Socket} = State) ->
    ok = gen_tcp:send(Socket, "ISTA 240 Protocol\\sError:\\sconnection\\stimed\\sout\n"),
    error_logger:info_msg("Client '~w' timed out\n", [self()]),
    {stop, normal, State}.


'NORMAL STAGE'({data, Data}, #state{socket=Socket} = State) ->
    ?DEBUG(debug, "DATA recived '~s'~n", [Data]),
    {list, Message}=eadc_utils:convert({string, Data}),
    case Message of
	[[Header|Command_name]|Tail] ->
	    H = list_to_atom([Header]),Cmd=list_to_atom(Command_name),
	    Res = (catch handle_command(H, Cmd, Tail, Data, State)),
	    case Res of
		{'EXIT', Error} ->
		    Msg_to_send= lists:flatten(io_lib:format("Error: ~w",[Error])),
		    eadc_utils:error_to_pid(self(), Msg_to_send);
		_ -> everything_is_fine
	    end,
	    ?DEBUG(debug, "command result ~w\n", [Res]);    
	[[]] ->
	    keep_alive;
	Other ->
	    ?DEBUG(error, "Unknown message '~w'", [Other]),
	    ok = gen_tcp:send(Socket, "ISTA 240 Protocol\\serror\n")
    end,
    {next_state, 'NORMAL STAGE', State};

'NORMAL STAGE'({inf_update, Inf_update}, #state{inf=Inf} = State) ->
    ?DEBUG(debug, "BINF Update '~w'~n", [Inf_update]),
    ?DEBUG(debug, "Old BINF '~s'~n", [Inf]),
    New_Inf=inf_update(Inf, Inf_update),
    ?DEBUG(debug, "New BINF '~s'~n", [New_Inf]),
    {next_state, 'NORMAL STAGE', State#state{inf=New_Inf}};

'NORMAL STAGE'({new_client, Pid}, #state{inf=BINF} = State) ->
    ?DEBUG(debug, "new_client event from ~w \n", [Pid]),
    gen_fsm:send_event(Pid, {send_to_socket, BINF}),
    {next_state, 'NORMAL STAGE', State};

'NORMAL STAGE'({send_to_socket, Data}, #state{socket=Socket} = State) ->
    ?DEBUG(debug, "send_to_socket event '~s'~n", [Data]),
    ok = gen_tcp:send(Socket, lists:concat([Data, "\n"])),
    {next_state, 'NORMAL STAGE', State};

'NORMAL STAGE'(Other, State) ->
    ?DEBUG(debug, "Unknown message '~s' ~n", [Other]),
    {next_state, 'NORMAL STAGE', State}.


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
handle_sync_event(get_state, From, StateName, StateData) ->
    gen_fsm:reply(From, StateData),
    {next_state, StateName, StateData};
handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_info({tcp, Socket, Bin}, StateName, #state{socket=Socket, buf=Buf} = StateData) ->
    %% Flow control: enable forwarding of next TCP message
    inet:setopts(Socket, [{active, once}]),
    Data = binary_to_list(Bin),
    String = lists:delete($\n, Data),
    ?DEBUG(debug, "tcp string '~s'", [String]), 
    case {Buf, lists:last(Data)} of 
	{[], 10} -> 
	    ?MODULE:StateName({data, String}, StateData);
	{_, 10} ->
	    ?MODULE:StateName({data, lists:concat([Buf,Data])}, StateData#state{buf=[]});
	_ -> 
	    {next_state, StateName, StateData#state{buf=lists:concat([Buf,Data])}}
    end;


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
terminate(_Reason, _StateName, #state{socket=Socket, sid=Sid}=State) ->
    ?DEBUG(debug, "TERMINATE ~w", [Sid]),
    (catch ets:delete(eadc_clients, Sid)),
    String_to_send = "IQUI "++ atom_to_list(Sid) ++"\n",
    lists:foreach(fun(Pid) ->
			  gen_fsm:send_event(Pid, {send_to_socket, String_to_send})
		  end, all_pids()),
    eadc_plugin:hook(user_quit, [{sid, Sid}, {msg, String_to_send},{pids,[]},
				 {data,[]},{state, State}]),
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


%% суть хаба по хедеру понять где взять цели куда нужно
%% посылать сообщение, лишь иногда хабу нужно как-то менять
%% посылаемое сообещение или не посылать его вовсе

%% в итоге: берём хедер, получаем список получаетелей и шлём.
handle_command(H, Cmd, Tail, Data, State) ->
    {Pids, Args} = %% в данном случае мы получаем список получателей
	case H of  %% и параметры для обработчика одним махом =)
	    'B' ->
		[MySid | Par] = Tail, 
		{all_pids(), 
		 [{par, Par}, {my_sid, MySid}]};
	    'I' ->
		{[], 
		 [{par, Tail}]};
	    'H' ->
		{[], 
		 [{par, Tail}]};
	    'D' ->
		[MySid, TSid | Par] = Tail, 
		{[get_pid_by_sid(TSid)], 
		 [{par, Par}, {my_sid, MySid}, {tar_sid, TSid}]};
	    'E' ->
		[MySid, TSid | Par] = Tail, 
		{[get_pid_by_sid(MySid), get_pid_by_sid(TSid)], 
		 [{par, Par}, {my_sid, MySid}, {tar_sid, TSid}]};
	    'F' ->
		[MySid | Par] = Tail,
		{all_pids(), 
		 [{par, Par}, {my_sid, MySid}]}
	end,
    {New_Pids, New_Data} = client_command(H, Cmd, [{data, Data}|Args], Pids, State),
    eadc_utils:send_to_pids(New_Pids, New_Data).

client_command(Header, Command, Args, Pids, State) ->
    Data=get_val(data, Args),
    Res =
	case {Header,Command} of 
	    {'B','MSG'} ->
		[Msg]=get_val(par, Args),
		Nick=State#state.nick,
		Sid=list_to_atom(get_val(my_sid, Args)),
		?DEBUG(debug, "client_command: chat_msg hook", []),
		Params=[{pid,self()},{msg,Msg},{sid,Sid},{nick,Nick},{data, Data},
			{pids,Pids},{state, State}],
		eadc_plugin:hook(chat_msg, Params);
	    {'D','CTM'} ->
		[Pid] = Pids,
		case is_pid(Pid) of
		    true ->
			Args2=get_val(par, Args),
			Sid=list_to_atom(get_val(my_sid, Args)),
			?DEBUG(debug, "client_command: chat_msg hook ~w", [Pids]),
			eadc_plugin:hook(ctm, [{pid,self()},{args,Args2},{sid,Sid},
					       {data,Data},{pids,Pids},{state, State}]);
		    false ->
			eadc_utils:error_to_pid(self(), "Произошла ошибка при поиске юзера с которого вы хотите скачать, такое ощущение что его нет."),
			{[], Data} %% в том смысле что никому ничего мы теперь не пошлём
		end;
	    {_place, _holder} ->
		{Pids, Data}
	end,
    %%?DEBUG(debug, "client_command: ~w", [{Header, Command, Args, Pids, State}]),
    Res.


%%%------------------------------------------------------------------------                                                                                            
%%% Helping functions                                                                                            
%%%------------------------------------------------------------------------

get_val(Key, Args) -> 
    {value,{Key, Val}} = lists:keysearch(Key, 1, Args), Val.

get_unical_SID() ->
    Sid = eadc_utils:random_base32(4),
    case ets:member(eadc_clients, list_to_atom(Sid)) of
	true -> get_unical_SID();
	_    -> Sid
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
	[] ->
     error;
	[Sid] ->
	    Sid
    end.

get_unical_cid(Cid) ->
    List=ets:match(eadc_clients, #client{_='_', cid=Cid}),
    case length(List) of
	0 ->
	    Cid;
	_ ->
	    get_unical_cid("AAAA"++eadc_utils:random_base32(35))
    end.


all_pids() ->
    List=ets:match(eadc_clients, #client{_='_', pid='$1'}),
    lists:map(fun([Pid]) -> Pid end, List).


test(String) ->
    [Pid | _] =all_pids(),
    gen_fsm:send_event(Pid, {send_to_socket, String}).


inf_update(Inf, Inf_update) ->
    {list, [_binf, Sid | Inf_list]} = eadc_utils:convert({string, Inf}),
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
    {string, New_Inf} = eadc_utils:convert({list, ["BINF", Sid | New_Inf_list]}),
    New_Inf.
