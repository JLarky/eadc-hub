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
	 'PROTOCOL STAGE'/2,
	 'IDENTIFY STAGE'/2,
	 'VERIFY STAGE'/2,
	 'NORMAL STAGE'/2
	]).

%% HELPING FUNCTIONS
-export([all_pids/0]).

%% DEBUG
-export([test/1, get_pid_by_sid/1, get_unical_cid/1,get_unical_SID/0]).

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
    ?DEBUG(debug, "Data recived in PROTOCOL '~s'~n", [Data]),
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
    ?DEBUG(debug, "String recived in IDENTIFY '~s'~n", [Data]),
    {list, List} = eadc_utils:convert({string, Data}),
    case List of
	["BINF", SID | _] ->
	    case list_to_atom(SID) == Sid of
		false ->
		    gen_tcp:send(Socket,"ISTA 240 "++eadc_utils:quote("SID is not correct")++"\n"),
		    {stop, normal, State};	    
		true ->
		    ?DEBUG(debug, "New client with BINF= '~s'~n", [Data]),
		    My_Pid=self(), {I1,I2,I3,I4} = Addr,
		    P_Inf=eadc_utils:parse_inf(Data),

		    PID=eadc_utils:get_required_field('PD', P_Inf),
		    Nick=eadc_utils:get_required_field('NI', P_Inf),
		    Cid_f=eadc_utils:get_required_field('ID', P_Inf),
		    Cid=get_unical_cid(Cid_f),

		    case eadc_utils:base32_encode(tiger:hash(eadc_utils:base32_decode(PID))) of
			Cid ->
			    ok;
			WRONGCID ->
			    eadc_utils:broadcast(fun(Pid_to_inform) ->
							 eadc_utils:info_to_pid(Pid_to_inform,
										lists:concat(["User '", Nick, "' has wrong CID (",WRONGCID,"). Be aware"]))
						 end),
			    eadc_utils:info_to_pid(self(), "Your CID isn't corresponding to PID. You are cheater.")
		    end,
		    
		    Inf=inf_update(Data, [lists:concat(["I4",I1,".",I2,".",I3,".",I4]),"PD","ID"++Cid, "NI"++Nick]),
		    New_State=State#state{inf=Inf, nick=Nick, cid=Cid, sid=Sid},

		    Other_clients = all_pids(),
		    Args=[{pids,Other_clients},{data,Inf},{sid,SID},{pid,My_Pid},
			  {nick, Nick}, {inf, Inf}, {state,New_State}],

		    case need_authority(Nick, Cid) of
			true ->
			    Random=tiger:hash(eadc_utils:random_base32(39)),
			    eadc_utils:send_to_pid(self(), {args, ["IGPA", eadc_utils:base32_encode(Random)]}),
			    {next_state, 'VERIFY STAGE', New_State#state{random=Random, triesleft=3,
									 afterverify=fun() -> user_login(Sid,Nick,Cid,Args) end}, ?TIMEOUT};
			false ->
			    user_login(Sid,Nick,Cid,Args),
			    {next_state, 'NORMAL STAGE', New_State, ?TIMEOUT}
		    end
			    
	    end;
	_ ->
	    ok = gen_tcp:send(Socket, "ISTA 240 Protocol error\n"),
	    {next_state, 'IDENTIFY STAGE', State, ?TIMEOUT}
    end;

'IDENTIFY STAGE'({send_to_socket, Data}, State) ->
    send_to_socket(Data, State),
    {next_state, 'IDENTIFY STAGE', State};

'IDENTIFY STAGE'(timeout,  #state{socket=Socket} = State) ->
    ok = gen_tcp:send(Socket, "ISTA 240 Protocol\\sError:\\sconnection\\stimed\\sout\n"),
    error_logger:info_msg("Client '~w' timed out\n", [self()]),
    {stop, normal, State}.

'VERIFY STAGE'({data, Data}, #state{nick=Nick, addr=Addr, cid=Cid, random=Random, sid=_Sid, afterverify=Func, triesleft=Tries_left}=State) ->
    case eadc_utils:convert({string, Data}) of
	{list, ["HPAS", Pass]} ->
	    {login, Login}=eadc_utils:account_get_login(Nick, Cid),
	    Account=eadc_utils:account_get(Login),
	    User_Pass = Account#account.pass,
	    A=User_Pass++Random,
	    B=tiger:hash(A),
	    C=eadc_utils:base32_encode(B),
	    ?DEBUG(debug, "DATA recived in VERIFY pass for ~s(~p) '~p'~n", [Nick,Addr,{Pass,C}]),
	    case C==Pass of
		true ->
		    Func(), %% user_login
		    if (Account#account.class > 2) ->
			    Inf_update=["CT4"];
		       true ->
			    Inf_update=["CT2"]
		    end,
		    New_Inf_full=inf_update(State#state.inf, Inf_update),
		    New_Inf_to_send=inf_update(lists:sublist(New_Inf_full, 9), Inf_update),

		    eadc_utils:broadcast({string, New_Inf_to_send}),
		    set_client_login(Login),
		    {next_state, 'NORMAL STAGE', State#state{inf=New_Inf_full, login=Login}};
		false ->
		    case (catch Tries_left-1) of
			I when is_integer(I) and (I > 0) ->
			    timer:sleep(1000),
			    eadc_utils:send_to_pid(self(), {args, ["ISTA", "123", "Wrong password"]}),
			    New_Random=tiger:hash(eadc_utils:random_base32(39)),
			    eadc_utils:send_to_pid(self(), {args, ["IGPA", eadc_utils:base32_encode(New_Random)]}),
			    {next_state, 'VERIFY STAGE', State#state{random=New_Random, triesleft=I}, ?TIMEOUT};
			_Other ->
			    {stop, normal, State}
		    end
	    end;
	_ ->
	    ?DEBUG(debug, "DATA recived in VERIFY '~s'~n", [Data]),
	    {next_state, 'VERIFY STAGE', State, ?TIMEOUT}
    end;

'VERIFY STAGE'({send_to_socket, Data}, State) ->
    send_to_socket(Data, State),
    {next_state, 'VERIFY STAGE', State};

'VERIFY STAGE'(timeout,  #state{socket=Socket} = State) ->
    ok = gen_tcp:send(Socket, "ISTA 240 Protocol\\sError:\\sconnection\\stimed\\sout\n"),
    error_logger:info_msg("Client '~w' timed out\n", [self()]),
    {stop, normal, State}.


'NORMAL STAGE'({data, Data}, #state{socket=Socket} = State) ->
    ?DEBUG(debug, "DATA recived in NORMAL '~s'~n", [Data]),
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

'NORMAL STAGE'({inf_update, New_Inf}, State) ->
    {next_state, 'NORMAL STAGE', State#state{inf=New_Inf}};

'NORMAL STAGE'({new_client, Pid}, #state{inf=BINF} = State) ->
    ?DEBUG(debug, "new_client event from ~w \n", [Pid]),
    gen_fsm:send_event(Pid, {send_to_socket, BINF}),
    {next_state, 'NORMAL STAGE', State};

'NORMAL STAGE'({send_to_socket, Data}, State) ->
    send_to_socket(Data, State),
    {next_state, 'NORMAL STAGE', State};

'NORMAL STAGE'(kill_your_self, State) ->
    ?DEBUG(error, "~w killed by self", [self()]),
    {stop, normal, State};

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

handle_info({tcp_error,Socket,etimedout}, _StateName,
            #state{socket=Socket, addr=Addr} = StateData) ->
    error_logger:info_msg("~p Client ~p connection timeout.\n", [self(), Addr]),
    {stop, normal, StateData};

handle_info({master, Data}, StateName, StateData) ->
    ?MODULE:StateName({master, Data}, StateData);

handle_info(Info, StateName, StateData) ->
    ?DEBUG(error, "Unknown info ~w\n", [Info]),
    {noreply, StateName, StateData}.


%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, #state{socket=Socket, sid=Sid}=State) ->
    ?DEBUG(debug, "TERMINATE ~w", [Sid]),
    (catch client_delete(Sid)),
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
		Params=[{pid,self()},{msg,eadc_utils:unquote(Msg)},{sid,Sid},{nick,Nick},
			{data, Data},{pids,Pids},{state, State}],
		eadc_plugin:hook(chat_msg, Params);
	    {'B','INF'} ->
		%% user not allow to change his CT or ID
		Inf_update=lists:filter(fun(A) -> 
						not (lists:prefix("CT", A) or
						     lists:prefix("ID", A))
					end, get_val(par, Args)),
		New_Inf_full=inf_update(State#state.inf, Inf_update),
		gen_fsm:send_event(self(), {inf_update, New_Inf_full}),
		New_Inf_to_send=inf_update(lists:sublist(New_Inf_full, 9), Inf_update),

		{Pids, New_Inf_to_send};
	    {'D','CTM'} ->
		[Pid] = Pids,
		case is_pid(Pid) of
		    true ->
			Args2=get_val(par, Args),
			Sid=list_to_atom(get_val(my_sid, Args)),
			?DEBUG(debud, "client_command: ctm hook ~w", [Pids]),
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
    eadc_utils:get_val(Key, Args).

get_unical_SID() ->
    Sid=eadc_utils:random_base32(4),
    MatchHead = #client{sid='$1', _='_'},Guard = [{'==', '$1', Sid}],Result = '$1',
    F = fun() ->
		mnesia:select(client,[{MatchHead, Guard, [Result]}])	
	end,
    case catch mnesia:transaction(F) of
	{atomic, []} -> %% not used SID
	    Sid;
	{atomic, [_|_]} -> %% CID allready in use, generate new
	    get_unical_SID();
	Error -> {error, Error} 
    end.

get_pid_by_sid(Sid) when is_atom(Sid) ->
    MatchHead = #client{sid='$1', pid='$2', _='_'},Guard = [{'==', '$1', Sid}],Result = '$2',
    F = fun() ->
		mnesia:select(client,[{MatchHead, Guard, [Result]}])	
	end,
    case catch mnesia:transaction(F) of
	{atomic, [Pid]} -> %% list must contain one pid
	    Pid;
	Error -> {error, Error} 
    end;

get_pid_by_sid(Sid) when is_list(Sid)->
    get_pid_by_sid(list_to_atom(Sid)).

get_unical_cid(Cid) ->
    MatchHead = #client{cid='$1', _='_'},Guard = [{'==', '$1', Cid}],Result = '$1',
    F = fun() ->
		mnesia:select(client,[{MatchHead, Guard, [Result]}])	
	end,
    case catch mnesia:transaction(F) of
	{atomic, []} -> %% not used CID
	    Cid;
	{atomic, [_|_]} -> %% CID allready in use, generate new
	    get_unical_cid("CIDINUSE"++eadc_utils:random_base32(31));
	Error -> {error, Error} 
    end.


all_pids() ->
    MatchHead = #client{pid='$1', _='_'},Guard = [],Result = '$1',
    F = fun() ->
		mnesia:select(client,[{MatchHead, Guard, [Result]}])	
	end,
    case catch mnesia:transaction(F) of
	{atomic, Pids} -> Pids;
	_Error -> []
    end.

set_client_login(Login) ->
    F=fun() ->
	      [Client]=mnesia:match_object(#client{pid=self(),_='_'}),
	      mnesia:write(Client#client{login=Login})
      end,
    {atomic, ok}=mnesia:transaction(F).

test(String) ->
    set_client_login(String).
    %%[Pid | _] =all_pids(),
    %%gen_fsm:send_event(Pid, {send_to_socket, String}).


inf_update(Inf, Inf_update) ->
    [$B,$I,$N,$F,$\ |Inf_to_parse]=Inf,
    Parsed_Inf=eadc_utils:parse_inf(Inf_to_parse),
    New_Inf=
	lists:foldl(fun([A1,A2|Cur_update], Acc) ->
			    Key=list_to_atom([A1,A2]),
			    case get_val(Key, Parsed_Inf) of
				'NO KEY' ->
				    Acc++[{Key,Cur_update}];
				_ ->
				    case Cur_update of
				    [] -> lists:keydelete(Key, 1, Acc);
					_  -> eadc_utils:set_val(Key, Cur_update, Acc)
				    end
			    end
		    end, Parsed_Inf, Inf_update),
    "BINF"++eadc_utils:deparse_inf(New_Inf).

user_login(Sid,Nick,Cid,Args) ->
    My_Pid=self(),
    {Pids_to_inform, Data_to_send}=eadc_plugin:hook(user_login, Args),
    clients_insert(#client{pid=My_Pid, sid=Sid, nick=Nick, cid=Cid}),
    lists:foreach(fun(Pid) ->
			  gen_fsm:send_event(Pid, {new_client, My_Pid})
		  end, Pids_to_inform),
    eadc_utils:send_to_pids([self()| Pids_to_inform], Data_to_send).

need_authority(Nick, Cid) ->
    MatchHead = #account{cid='$1', nick='$2', _='_'},
    Guard = [{'or',{'==','$2',Nick},{'==','$1',Cid}}], Result = '$2',
    F = fun() ->
		mnesia:select(account,[{MatchHead, Guard, [Result]}])
	end,
    A=(catch mnesia:transaction(F)),
    case A of
	{atomic, [_|_]} -> %% not used SID
	    true;
	_ ->
	    false
    end.


clients_insert(Client) when is_record(Client, client) ->
    F = fun() ->
		mnesia:write(client, Client, write)
	end,
    mnesia:transaction(F).

client_delete(Sid) ->
    mnesia:transaction(fun() ->
			       mnesia:delete({client, Sid})
		       end).

send_to_socket(Data, #state{socket=Socket}) ->
    ?DEBUG(debug, "send_to_socket event '~s'~n", [Data]),
    ok = gen_tcp:send(Socket, lists:concat([Data, "\n"])).
