-module(eadc_client_fsm).
-author('jlarky@gmail.com').

-behaviour(gen_fsm).

-export([start_link/0, set_socket/2, start_client/0]).

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
-export([all_pids/0,inf_update/2,sup_update/2]).
-export([get_pid_by_sid/1, get_unical_cid/0,get_unical_SID/0]).

%% SOCKET
-export([send/2]).

%% CLIENT
-export([client_get/1, client_write/1, client_delete/1,client_all/0]).

-define(TIMEOUT, 30000).
-include("eadc.hrl").

%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).

start_client() ->
    supervisor:start_child(eadc_client_sup, []).

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
    %% hook
    Args=[{state, State}, {ip,IP}, {socket, Socket}],
    _Hooked_Args=eadc_plugin:hook(user_connected, [Args]),
    {next_state, 'PROTOCOL STAGE', State#state{socket=Socket, addr=IP}, ?TIMEOUT};
'WAIT_FOR_SOCKET'(Other, State) ->
    error_logger:error_msg("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", [Other]),
    %% Allow to receive async messages
    {next_state, 'WAIT_FOR_SOCKET', State}.

'PROTOCOL STAGE'({data, Data}, #state{socket=Socket, other=Other} = State) ->
    ?DEBUG(debug, "Data recived in PROTOCOL '~s'~n", [Data]),
    case Data of
	[ $H,$S,$U,$P,$\  | SupLine] ->
	    SupList=string:tokens(SupLine, " "),Sup=sup_update([], SupList),
	    New_Other=eadc_utils:set_val(sup, Sup, Other),
	    ok = send("ISUP ADBAS0 ADBASE ADTIGR ADUCM0 ADUCMD", Socket),
	    Sid = get_unical_SID(),
	    ok = send(["ISID ",eadc_utils:sid_to_s(Sid)], Socket),
	    New_State=State#state{sid=Sid, other=New_Other},
	    Args=[{state, New_State}, {data,Data}, {sup, Sup}],
	    _Hooked_Args=eadc_plugin:hook(user_identify, [Args]),
	    {next_state, 'IDENTIFY STAGE', New_State, ?TIMEOUT};
	_ ->
	    ok = send("ISTA 140 Protocol\\serror", Socket),
 	    {next_state, 'PROTOCOL STAGE', State, ?TIMEOUT}
    end;

'PROTOCOL STAGE'(timeout, State) ->
    Not_set=make_ref(),
    Host=eadc_utils:get_option(hub, host, Not_set),
    Port=eadc_utils:get_option(hub, port, Not_set),
    if %% most likely that timeout is coused by connecting in nmdc mode
	((Host /= Not_set) and (Port /= Not_set)) ->
	    Addr=lists:concat(["adc://", Host, ":", Port]),
	    Msg=["<ADC> This hub doesn't work with NMDC clients, redirect to ADC(",
		 Addr,").|$ForceMove ",Addr,"|"],
	    ok = send(Msg, State);
       true ->
	    ok = send("ISTA 240 Protocol\\sError:\\sconnection\\stimed\\sout", State)
    end,
    error_logger:info_msg("Client '~w' timed out\n", [self()]),
    {stop, normal, State}.

'IDENTIFY STAGE'({data, Data}, #state{socket=Socket, addr=Addr, sid=Sid}=State) ->
    ?DEBUG(debug, "String recived in IDENTIFY '~s'~n", [Data]),
    List = eadc_utils:s2a(Data),
    case List of
	["BINF", SID | _] ->
	    send("IINF CT32 VEEADC NIHub DE", Socket),
	    case SID == eadc_utils:sid_to_s(Sid) of
		false ->
		    send(["ISTA 240 ",eadc_utils:quote("SID is not correct")], Socket),
		    {stop, normal, State};	    
		true ->
		    ?DEBUG(debug, "New client with BINF= '~s'~n", [Data]),
		    ?DEBUG(error, "New client '~w'~n", [Addr]),
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
			    Msg=lists:concat(["User '", Nick, "' has wrong CID (",
					      WRONGCID,"). Be aware"]),
			    eadc_utils:broadcast({info, Msg}),
			    eadc_utils:info_to_pid(self(),"Your CID isn't corresponding to PID. You are cheater.")
		    end,
		    
		    Inf=inf_update(Data, [lists:concat(["I4",I1,".",I2,".",I3,".",I4]),
					  "PD","ID"++Cid, "NI"++Nick]),

		    Login=case eadc_utils:account_get_login(Nick, Cid) of
			      {login, A} -> A;
			      _ -> undefined
			  end,

		    New_State=State#state{login=Login},

		    Other_clients = all_pids(),
		    Args=[{pids,Other_clients},{data,Inf},{sid,SID},{pid,My_Pid},{cid, Cid},
			  {nick, Nick}, {inf, Inf}, {addr,Addr},{state,New_State}],

		    case need_authority(Nick, Cid) of
			true ->
			    Random=tiger:hash(eadc_utils:random_string(24)),
			    eadc_utils:send_to_pid(self(), {args, ["IGPA", eadc_utils:base32_encode(Random)]}),
			    {next_state, 'VERIFY STAGE', New_State#state{random=Random, triesleft=3,afterverify=fun() -> user_login(Sid,Nick,Cid,Args) end}, ?TIMEOUT};
			false ->
			    New_Args=user_login(Sid,Nick,Cid,Args), %% user_login
			    {client, Client}={client, eadc_utils:get_val(client, New_Args)},
			    case Client of %% user_login
				Client when is_record(Client, client) ->
				    New_State2=eadc_utils:get_val(state, New_Args),
				    {next_state, 'NORMAL STAGE', New_State2, ?TIMEOUT};
				{logoff, Logoff_Msg} ->
				    logoff(Logoff_Msg, New_State);
				Why ->
				    {stop, Why, New_State}
			    end
		    end
			    
	    end;
	_ ->
	    ok = send("ISTA 240 Protocol\\serror", Socket),
	    {next_state, 'IDENTIFY STAGE', State, ?TIMEOUT}
    end;

'IDENTIFY STAGE'({send_to_socket, Data}, State) ->
    send_to_socket(Data, State),
    {next_state, 'IDENTIFY STAGE', State};

'IDENTIFY STAGE'(timeout, State) ->
    ok = send("ISTA 240 Protocol\\sError:\\sconnection\\stimed\\sout", State),
    error_logger:info_msg("Client '~w' timed out\n", [self()]),
    {stop, normal, State}.

'VERIFY STAGE'({data, Data}, #state{addr=Addr, login=Login, random=Random, sid=_Sid, afterverify=Func, triesleft=Tries_left}=State) ->
    case eadc_utils:s2a(Data) of
	["HPAS", Pass] ->
	    Account=eadc_utils:account_get(Login),
	    User_Pass = Account#account.pass,
	    A=User_Pass++Random,
	    B=tiger:hash(A),
	    C=eadc_utils:base32_encode(B),
	    ?DEBUG(debug, "DATA recived in VERIFY pass for ~s(~p) '~p'~n", [Login,Addr,{Pass,C}]),
	    case C==Pass of
		true ->
		    New_Args=Func(), %% user_login
		    {client, Client}={client, eadc_utils:get_val(client, New_Args)},

		    {state,New_State}={state,eadc_utils:get_val(state, New_Args)},
		    case Client of
			Client when is_record(Client, client) ->
			    case lists:member(root,Account#account.roles) of
				true ->
				    Inf_update=["CT4"];
				false ->
				    Inf_update=["CT2"]
			    end,
			    New_Inf_full=inf_update(Client#client.inf, Inf_update),
			    New_Inf_to_send=inf_update(lists:sublist(New_Inf_full, 9), Inf_update),
			    client_write(Client#client{inf=New_Inf_full,login=Login}),
			    eadc_utils:broadcast({string, New_Inf_to_send}),
			    {next_state, 'NORMAL STAGE', New_State};
			{logoff, Logoff_Msg} ->
			    logoff(Logoff_Msg, New_State);
			Why ->
			    io:format("kill"),
			    {stop, Why, New_State}
		    end;
		false ->
		    case (catch Tries_left-1) of
			I when is_integer(I) and (I > 0) ->
			    timer:sleep(1000),
			    eadc_utils:send_to_pid(self(), {args, ["ISTA", "123", "Wrong password"]}),
			    New_Random=tiger:hash(eadc_utils:random_string(24)),
			    eadc_utils:send_to_pid(self(), {args, ["IGPA", eadc_utils:base32_encode(New_Random)]}),
			    {next_state, 'VERIFY STAGE', State#state{random=New_Random, triesleft=I}, ?TIMEOUT};
			_Other ->
			    %% TODO: send message to user
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

'VERIFY STAGE'(timeout, State) ->
    ok = send("ISTA 240 Protocol\\sError:\\sconnection\\stimed\\sout", State),
    error_logger:info_msg("Client '~w' timed out\n", [self()]),
    {stop, normal, State}.


'NORMAL STAGE'({data, Data_utf8}, #state{socket=Socket} = State) ->
    Data=utf8:from_utf8(Data_utf8),
    ?DEBUG(debug, "DATA recived in NORMAL '~s'~n", [Data]),
    Message=string:tokens(Data, " "),
    New_State = case Message of
		    [[Header|Command_name]|Tail] ->
			H = [Header],Cmd=Command_name,
			Res = (catch handle_command(H, Cmd, Tail, Data, State)),
			?DEBUG(debug, "command result ~w\n", [Res]),
			case Res of
			    {ok, New_State_} when is_record(New_State_,state) ->
				New_State_;
			    Error ->
				Msg_to_send= lists:flatten(io_lib:format("Error: ~w",[Error])),
				eadc_utils:error_to_pid(self(), Msg_to_send),
				State
			end;
		    [] ->
			keep_alive,
			State;
		    Other ->
			?DEBUG(error, "Unknown message '~w'", [Other]),
			ok = send("ISTA 240 Protocol\\serror", Socket),
			State
		end,
    {next_state, 'NORMAL STAGE', New_State};

'NORMAL STAGE'({send_to_socket, Data}, State) ->
    send_to_socket(Data, State),
    {next_state, 'NORMAL STAGE', State};

'NORMAL STAGE'(kill_yourself, State) ->
    ?DEBUG(error, "~w killed by (him/her/it)self", [self()]),
    {stop, normal, State};

'NORMAL STAGE'({kill, Why}, State) ->
    send_to_socket("ISTA 100 "++eadc_utils:quote(Why), State),
    ?DEBUG(error, "~w killed: ~s~n", [self(), Why]),
    {stop, normal, State};

'NORMAL STAGE'(Other, State) ->
    ?DEBUG(error, "Unknown message '~s' ~n", [Other]),
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

handle_info({'EXIT',Pid, Reason}, StateName, StateData) ->
    ?DEBUG(error, "this guy ~w just die: ~w\n", [Pid, Reason]),
    {next_state, StateName, StateData};    
handle_info({tcp_error,_,Error}, _StateName, StateData) ->
    ?DEBUG(error, "tcp_error ~w\n", [Error]),
    {stop, normal, StateData};    
handle_info(Info, StateName, StateData) ->
    ?DEBUG(error, "Unknown info ~w\n", [Info]),
    {noreply, StateName, StateData}.


%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_,_,#state{sid=Sid}) when not is_integer(Sid) -> ok;
terminate(Reason, _StateName, #state{socket=Socket,sid=Sid}=State) ->
    case Reason of
	String when is_list(String) ->
	    (catch send(["ISTA 123 ",eadc_utils:quote(Reason)], State));
	_ -> ok
    end,
    ?DEBUG(debug, "TERMINATE ~w", [Sid]),
    S_to_send="IQUI "++eadc_utils:sid_to_s(Sid),
    Args=eadc_plugin:hook(user_quit, [{sid, Sid}, {msg, S_to_send},{state, State}]),
    String_to_send=eadc_utils:get_val(msg, Args),
    lists:foreach(fun(Pid) ->
			  case Pid of
			      PID when is_pid(PID) ->
				     gen_fsm:send_event(Pid, {send_to_socket, String_to_send});
			      _not_pid ->
				  ok
			  end
		  end, all_pids()),
    (catch client_delete(Sid)),
    (catch send(String_to_send, Socket)),
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
	    "B" ->
		[MySid | Par] = Tail, 
		{all_pids(), 
		 [{par, Par}, {my_sid, MySid}]};
	    "I" ->
		{[], 
		 [{par, Tail}]};
	    "H" ->
		{[], 
		 [{par, Tail}]};
	    "D" ->
		[MySid, TSid | Par] = Tail, 
		{[get_pid_by_sid(TSid)], 
		 [{par, Par}, {my_sid, MySid}, {tar_sid, TSid}]};
	    "E" ->
		[MySid, TSid | Par] = Tail, 
		{[get_pid_by_sid(MySid), get_pid_by_sid(TSid)], 
		 [{par, Par}, {my_sid, MySid}, {tar_sid, TSid}]};
	    "F" ->
		[MySid | Par] = Tail,
		{all_pids(), 
		 [{par, Par}, {my_sid, MySid}]}
	end,
    {New_Pids, New_Data, New_State} = client_command(H, Cmd, [{data, Data}|Args], Pids, State),
    {eadc_utils:send_to_pids(New_Pids, utf8:to_utf8(New_Data)), New_State}.

client_command(Header, Command, Args, Pids, State) ->
    Data=get_val(data, Args),
    Client=client_get(State#state.sid),
    
    Res =
	case {Header,Command} of 
	    {"B","MSG"} ->
		[Msg|_]=get_val(par, Args),
		Sid=eadc_utils:unbase32(get_val(my_sid, Args)),
		Nick=Client#client.nick,
		?DEBUG(debug, "client_command: chat_msg hook", []),
		Params=[{pid,self()},{msg,eadc_utils:unquote(Msg)},{sid,Sid},{nick,Nick},
			{data, Data},{pids,Pids},{state, State},{client,Client}],
		eadc_plugin:hook(chat_msg, Params);
	    {"E", "MSG"} -> % private
		Msg=get_val(par, Args),
		Sid=eadc_utils:unbase32(get_val(my_sid, Args)),
                Nick=Client#client.nick,
                ?DEBUG(debud, "client_command: priv_msg hook ~p", [Data]),
                Params=[{pid,self()},{msg,Msg},{sid,Sid},{nick,Nick},
                        {data, Data},{pids,Pids},{state, State}],
		%%[{data, Data},{pids,Pids}];
                eadc_plugin:hook(priv_msg, Params);
	    {"B","INF"} ->
		%% user not allow to change his CT or ID
		Inf_update=lists:filter(fun(A) -> 
						not (lists:prefix("CT", A) or
						     lists:prefix("ID", A))
					end, get_val(par, Args)),
		New_Inf_full=inf_update(Client#client.inf, Inf_update),
		client_write(Client#client{inf=New_Inf_full}),
		New_Inf_to_send=inf_update(lists:sublist(New_Inf_full, 9), Inf_update),
		%% no any plugin
		[{pids, Pids}, {data, New_Inf_to_send}, {state, State}];
	    {"D","CTM"} ->
		{pid,[Pid]} = {pid,Pids},
		case is_pid(Pid) of
		    true ->
			Args2=get_val(par, Args),
			Sid=eadc_utils:unbase32(get_val(my_sid, Args)),
			?DEBUG(debug, "client_command: ctm hook ~w", [Pids]),
			eadc_plugin:hook(ctm, [{pid,self()},{args,Args2},{sid,Sid},{client,Client},
					       {data,Data},{pids,Pids},{state, State}]);
		    false ->
			eadc_utils:error_to_pid(self(), "Произошла ошибка при поиске юзера с которого вы хотите скачать, такое ощущение что его нет."),
			[{pids,[]}, {data,Data}, {state, State}] %% в том смысле что никому ничего мы теперь не пошлём
		end;
	    {"H", "SUP"} ->
		Sup_Update=get_val(par, Args),
		Sup=Client#client.sup,
		New_Sup=sup_update(Sup, Sup_Update),
		client_write(Client#client{sup=New_Sup}),
		[{pids,Pids}, {data,Data}, {state, State}];
	    {_place, _holder} ->
		[{pids,Pids}, {data,Data}, {state, State}]
	end,
    %%?DEBUG(debug, "client_command: ~w", [{Header, Command, Args, Pids, State}]),
    {eadc_utils:get_val(pids, Res),
     eadc_utils:get_val(data, Res),
     eadc_utils:get_val(state, Res)}.


%%%------------------------------------------------------------------------
%%% Helping functions
%%%------------------------------------------------------------------------

get_val(Key, Args) -> 
    eadc_utils:get_val(Key, Args).

get_unical_SID() ->
    Sid=eadc_utils:random(1048575), %% 20 bit and > 0 thatswhy can't be AAAA
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

get_pid_by_sid(Sid) when is_integer(Sid) ->
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
    get_pid_by_sid(eadc_utils:unbase32(Sid)).

get_unical_cid() ->
    Cid=eadc_utils:random((1 bsl 192)-1), %% 192 bit
    get_unical_cid(eadc_utils:cid_to_s(Cid)).

get_unical_cid(Cid) ->
    MatchHead = #client{cid='$1', _='_'},Guard = [{'==', '$1', Cid}],Result = '$1',
    F = fun() ->
		mnesia:select(client,[{MatchHead, Guard, [Result]}])	
	end,
    case catch mnesia:transaction(F) of
	{atomic, []} -> %% not used CID
	    Cid;
	{atomic, [_|_]} -> %% CID allready in use, generate new
	    get_unical_cid("CIDINUSE"++lists:sublist(eadc_utils:cid_to_s(eadc_utils:random((1 bsl 192)-1)) ,31));
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


inf_update_cur(Update, [], Acc) ->
    [Update|Acc];            %% adds new field
inf_update_cur([A1,A2], [[A1,A2|_Val]|Tail], Acc) ->
    Tail++Acc;               %% deletes empthy field
inf_update_cur([A1,A2|Val], [[A1,A2|_Val]|Tail], Acc) ->
    [[A1,A2|Val]|Tail++Acc]; %% change field value
inf_update_cur(Cur_Update, [Cur_Inf|Tail], Acc) ->
    inf_update_cur(Cur_Update, Tail, [Cur_Inf|Acc]).


inf_update(Inf, Inf_update) ->
    ["BINF", SID |Parsed_Inf]=string:tokens(Inf, " "),
    Foldl=fun(Cur_Update, Cur_Inf) -> inf_update_cur(Cur_Update, Cur_Inf, []) end,
    New_Inf=lists:foldl(Foldl ,Parsed_Inf, Inf_update), %% call Map to every element of inf string
    string:join(["BINF", SID | New_Inf], " ").

user_login(Sid,Nick,Cid,Args) ->
    My_Pid=self(),Pre_Client=#client{pid=My_Pid, sid=Sid, nick=Nick, cid=Cid, 
				     inf=get_val(inf,Args),login=get_val(login,Args),
				     addr=get_val(addr, Args)},
    Hooked_Args=eadc_plugin:hook(user_login, [{client, Pre_Client}|Args]),
    {pids, Pids_to_inform}={pids,get_val(pids, Hooked_Args)},
    {data, Data_to_send}={data,get_val(data, Hooked_Args)},
    {client, Hooked_Client}={client,get_val(client, Hooked_Args)},
    {state, State} = {state, get_val(state, Hooked_Args)},
    Sup=get_val(sup, State#state.other),
    Client=Hooked_Client#client{sup=Sup},
    New_Args=eadc_utils:set_val(client, Client, Hooked_Args),
    
    case is_record(Client, client) of
	true ->
	    lists:foreach(fun(#client{inf=CInf}) ->
				  eadc_utils:send_to_pid(My_Pid, CInf)
			  end, client_all()),
	    client_write(Client),
	    eadc_utils:send_to_pids([self()| Pids_to_inform], Data_to_send);
	false -> %% logoff
	    logoff
    end,
    New_Args.

sup_update(Sup, Sup_Update) when not is_list(Sup)->
    sup_update(["BASE"], Sup_Update);
sup_update(Sup, [[$A,$D| SupName]|Tail]) ->
    NotNew=fun(CSup) -> (CSup /= SupName) end,
    sup_update([SupName|lists:filter(NotNew, Sup)], Tail);
sup_update(Sup, [[$R,$M| SupName]|Tail]) when is_list(Sup) ->
    sup_update(lists:delete(SupName, Sup), Tail);
sup_update(Sup, [[_HZ_]|Tail]) ->
    sup_update(Sup, Tail);
sup_update(Sup, [_HZ_]) ->
    sup_update(Sup, []);
sup_update(Sup, []) ->
    Sup.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% client functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%

client_get(Sid) ->
    F = fun()->
		mnesia:match_object(#client{sid=Sid,_='_'})
	end,

    case (catch mnesia:transaction(F)) of
	{atomic, [Client]} ->
	    Client#client{};
	_ ->
	    undefined
    end.

client_write(Client) when is_record(Client, client)->
    F=fun() ->
	      mnesia:write(Client)
      end,
    mnesia:transaction(F).

client_delete(Sid) ->
    mnesia:transaction(fun() ->
			       mnesia:delete({client, Sid})
		       end).

client_all() ->
    F = fun()->
		mnesia:match_object(#client{_='_'})
	end,

    case (catch mnesia:transaction(F)) of
	{atomic, Clients} when is_list(Clients) ->
	    Clients;
	_ ->
	    {undefined, ?FILE, ?LINE}
    end.

send(Data, State) ->
    send_to_socket(Data, State).

send_to_socket(Data, #state{socket=Socket}=State) when is_record(State, state) ->
    send_to_socket(Data, Socket);
send_to_socket(Data, Socket) when is_port(Socket) ->
    ?DEBUG(debug, "send_to_socket event '~w'~n", [Data]),
    case (catch gen_tcp:send(Socket, [Data, "\n"])) of
	ok ->
	    ok;
	{error,einval} ->
	    ?DEBUG(error, "~w has error '{error,einval}' when sending '~p'\n", [self(),Data]),
	    case (catch gen_tcp:send(Socket, [utf8:to_utf8(Data), "\n"])) of
		ok ->
		    ok;
		Error ->
		    ?DEBUG(error, "~w has error '~w' when sending '~p'\n", [self(), Error,Data])
	    end;
	Error ->
	    ?DEBUG(error, "~w has error '~w' when sending '~p'\n", [self(), Error,Data])
    end.

logoff(Msg, State) ->
    send_to_socket(Msg, State),
    {stop, normal, State}.
