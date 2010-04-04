%%%-------------------------------------------------------------------
%%% File    : eadc_client.erl
%%% Author  : airelain <airelain@eeepc>
%%% Description : 
%%%
%%% Created :  2 Apr 2010 by airelain <airelain@eeepc>
%%%-------------------------------------------------------------------
-module(eadc_client).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([send/2,sendn/2]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(TIMEOUT, 30000).
-include("eadc.hrl").

-export([get_socket_by_sid/1,all_senders/0]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, eadc_client}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    error_logger:info_msg("unhandled call ~p\n",[_Request]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({accept,Sender},#state{states=SockStates}=State) ->
    NewState=State#state{states=[{Sender,wait_data}|SockStates]},
    {noreply,NewState};
handle_cast({received,Sender,Data},State) ->
    Data_=lists:delete($\n, Data),
    handle_data(Sender,Data_,State);
handle_cast(_Msg, State) ->
    error_logger:info_msg("unhandled cast ~p\n",[_Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    error_logger:info_msg("unhandled info ~p\n",[_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    error_logger:info_msg("terminate ~p\n",[_Reason]),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

handle_data(Sender,"",State) -> %% keep alive
    %%gen_tcp:send(Socket,"\n"),
    sockroute:send(Sender,"\n"),
    {noreply, State};
handle_data(Sender,Data,State) ->
    Message=string:tokens(Data, " "),
    case Message of
	[[Header|Command_name]|Tail] ->
	    H = [Header],Cmd=Command_name,
	    case (catch handle_command(H,Cmd,Tail,Data,Sender,State)) of
		NewState when is_record(NewState,state) ->
		    {noreply,NewState};
		Error ->
		    error_logger:info_msg("Error in handle data: ~p\n",
					  [Error]),
		    {noreply, State}
	    end
    end.

%% суть хаба по хедеру понять где взять цели куда нужно
%% посылать сообщение, лишь иногда хабу нужно как-то менять
%% посылаемое сообещение или не посылать его вовсе

%% в итоге: берём хедер, получаем список получаетелей и шлём.
handle_command(H, Cmd, Tail, Data, Sender, State) ->
    SocketStates=State#state.states,
    case eadc_utils:get_val(Sender,SocketStates) of
	'NO KEY' ->
	    error_logger:format("What did just happend? ~p", [{H,Cmd,Tail,Data,Sender,State}]);
	StateName ->
	    case catch handle_command(H, Cmd, Tail, Data, Sender, StateName,State) of
		{Senders2send, Data2send, State2store} ->
		    error_logger:info_msg("command handled ~p\n",
					  [{Senders2send, Data2send, State2store}]),
		    State2store;
		{stop, Client} ->
		    stop_client(Client),
		    State;
		Other ->
		    error_logger:format("Other: ~p", [Other])
	    end
    end.

handle_command(H, Cmd, Tail, Data, Sender, StateName,State) ->
    Client=get_client_by_sender(Sender),
    {Senders, Args} = %% в данном случае мы получаем список получателей
	case H of  %% и параметры для обработчика одним махом =)
	    "B" ->
		[MySid | Par] = Tail,
		{[Sender|all_senders()], 
		 [{par, Par}, {my_sid, MySid}]};
	    "I" ->
		{[], 
		 [{par, Tail}]};
	    "H" ->
		{[], 
		 [{par, Tail}]};
	    "D" ->
		[MySid, TSid | Par] = Tail, 
		{[get_socket_by_sid(TSid)], 
		 [{par, Par}, {my_sid, MySid}, {tar_sid, TSid}]};
	    "E" ->
		[MySid, TSid | Par] = Tail, 
		{[get_socket_by_sid(MySid), get_socket_by_sid(TSid)], 
		 [{par, Par}, {my_sid, MySid}, {tar_sid, TSid}]};
	    "F" ->
		[MySid | Par] = Tail,
		{all_senders(), 
		 [{par, Par}, {my_sid, MySid}]}
	end,
    client_command(H,Cmd,Data,[{client,Client},{sender,Sender}|Args],Senders,StateName,State).

client_command("H","SUP",Data,Args,[],wait_data,State) ->
    Sender=eadc_utils:get_val(sender,Args),
    sockroute:send(Sender,"ISUP ADBAS0 ADBASE ADTIGR ADUCM0 ADUCMD\n"),
    Sid=1234,
    SID=eadc_utils:sid_to_s(Sid),
    sockroute:send(Sender,cat(["ISID ",SID,"\n"])),
    {ok, {IP, _Port}} = inet:peername(Sender#sender.socket),
    OK=client_write(#client{sid=Sid,sender=Sender,addr=IP,other=[]}),
    io:format("!!! ~p\n",[OK]),
    %%States=eadc_utils:set_val(Socket,wait_inf,State#state.states),
    States=[{Sender,wait_inf}],
    {[],Data,State#state{states=States}};
client_command("B","INF",Data,Args_,_Sockets,wait_inf,State) ->
    Client=eadc_utils:get_val(client,Args_),
    Sender=eadc_utils:get_val(sender,Args_),
    Sid=Client#client.sid,
    Addr=Client#client.addr,
    SID=eadc_utils:get_val(my_sid,Args_),
    send(Sender,"IINF CT32 VEEADC NIADCHub DE\n"),

    %% check_sid
    (SID == eadc_utils:sid_to_s(Sid)) orelse 
	begin
	    send(Sender,cat(["ISTA 240 ",eadc_utils:quote("SID is not correct")])),
	    throw({stop, Client})
	end,
    
    ?DEBUG(debug, "New client with BINF= '~s'~n", [Data]),
    ?DEBUG(error, "New client '~w'~n", [Addr]),
    {I1,I2,I3,I4} = Addr,

    %% check nick
    P_Inf=eadc_utils:parse_inf(Data),
    Nick=case eadc_utils:get_val('NI', "", P_Inf) of
	     "" -> "not_set:"++eadc_utils:base32_encode(eadc_utils:random_string(5));
	     N -> N
	 end,

    %% check ID, PD
    PID=eadc_utils:get_required_field('PD', P_Inf, Client),
    Cid_f=eadc_utils:get_required_field('ID', P_Inf, Client),
    

    Cid=get_uniq_cid(Cid_f),

    case catch eadc_utils:base32_encode(tiger:hash(eadc_utils:base32_decode(PID))) of
	Cid ->
	    ok;
	WRONGCID ->
	    _Msg=lists:concat(["User '", Nick, "' has wrong CID (",
			       WRONGCID,"). Be aware"]),
	    %%eadc_utils:broadcast({info, _Msg}),
	    eadc_utils:info_to_client(Client,"Your CID isn't corresponding to PID. "
				      "You are cheater.")
    end,


    
    Inf=inf_update(Data, [lists:concat(["I4",I1,".",I2,".",I3,".",I4]),
			  "PD","ID"++Cid, "NI"++Nick]),
    
    Login=case eadc_utils:account_get_login(Nick, Cid) of
	      {login, A} -> A;
	      _ -> undefined
	  end,
    
    New_Client=Client#client{
		 %%sid,
		 pid=self(),
		 cid=Cid,
		 %%sender,
		 nick=Nick,		 
		 login=Login,
		 inf=Inf
		 %%sup,
		 %%addr,
		 %%other,
		},

    Other_senders = all_senders(),

    Args=[{senders,Other_senders},{data,Inf},{sid,SID},{state,State}],

    eadc_utils:info_to_sender(Sender,thing_to_sring({login,Login})),

    X=case need_authority(Nick, Cid) of
	  true ->
	      Random=tiger:hash(eadc_utils:random_string(24)),
	      eadc_utils:send_to_client(Client,{args,["IGPA", eadc_utils:base32_encode(Random)]}),
	      Afun=fun() -> user_login(Client,Args) end,
	      NNClient=New_Client#client{other=[{random,Random},{triesleft,3},{afterverify,Afun}]},
	      client_write(NNClient),
	      {ok, wait_pass};
	  false ->
	      New_Args=user_login(Client,Args), %% user_login
	      {client, NClient}={client, eadc_utils:get_val(client, New_Args)},
	      case NClient of %% user_login
		  NClient when is_record(NClient, client) ->
		      {ok, normal};
		  {logoff, Logoff_Msg} ->
		      logoff(Logoff_Msg, State);
		  Why ->
		      {stop, Why, State}
	      end
      end,
    case X of
	{ok, StateName} ->
	    States=State#state.states,
	    {[],Data,State#state{states=eadc_utils:set_val(Sender,StateName,States)}};
	Error ->
	    ?DEBUG(error, "error xcvdfer ~p", [Error]),
	    {[],Data,State}
	end;
%% user not allow to change his CT or ID
    %%Inf_update=lists:filter(fun(A) -> 
	%%			    not (lists:prefix("CT", A) or
		%%			 lists:prefix("ID", A))
			%%    end, eadc_utils:get_val(par, Args)),
    %%New_Inf_full=inf_update(Client#client.inf, Inf_update),
    %%client_write(Client#client{inf=New_Inf_full}),
    %%New_Inf_to_send=inf_update(lists:sublist(New_Inf_full, 9), Inf_update),
    %% no any plugin
    %%[{pids, Pids}, {data, New_Inf_to_send}, {state, State}],

client_command(Header,Command,Data,Args,Pids,StateName,State) ->
    error_logger:info_msg("unhandled command ~p\n",[{Header, Command, Args, Pids,
						     StateName,State}]).


all_senders() ->
    MatchHead = #client{sender='$1', _='_'},Guard = [],Result = '$1',
    F = fun() ->
                mnesia:select(client,[{MatchHead, Guard, [Result]}])
        end,
    case catch mnesia:transaction(F) of
        {atomic, Senders} -> Senders;
        _Error -> []
    end.

get_socket_by_sid(Sid) when is_integer(Sid) ->
    MatchHead = #client{sid='$1', sender='$2', _='_'},
    Guard = [{'==', '$1', Sid}],
    Result = '$2',
        F = fun() ->
		    mnesia:select(client,[{MatchHead, Guard, [Result]}])
	    end,
    case catch mnesia:transaction(F) of
        {atomic, [Socket]} ->
	    %% list must contain one pid
	    Socket;
	Error -> {error, Error}
    end;


get_socket_by_sid(Sid) when is_list(Sid)->
    get_socket_by_sid(eadc_utils:unbase32(Sid)).


get_client_by_sender(Sender) when is_record(Sender,sender) ->
    case catch eadc_user:client_find(#client{sender=Sender,_='_'}) of
	[Client] ->
	    Client;
	[] ->
	    [];
	_Er ->
	    error_logger:format("error ~p\n",[_Er]),
	    []
    end.


cat(A) when is_list(A) ->
    lists:concat(A).

sendn(Sender,Data) ->
    send(Sender,cat([Data,"\n"])).

send(Sender,Data) ->
    sockroute:send(Sender,Data).

get_uniq_cid(Cid) ->
    MatchHead = #client{cid='$1', _='_'},Guard = [{'==', '$1', Cid}],Result = '$1',
    F = fun() ->
		mnesia:select(client,[{MatchHead, Guard, [Result]}])	
	end,
    case catch mnesia:transaction(F) of
	{atomic, []} -> %% not used CID
	    Cid;
	{atomic, [_|_]} -> %% CID allready in use, generate new
	    get_uniq_cid("CIDINUSE"++lists:sublist(eadc_utils:cid_to_s(eadc_utils:random((1 bsl 192)-1)) ,31));
	Error -> {error, Error} 
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

user_login(Client,Args) ->
    Hooked_Args=eadc_plugin:hook(user_login, [{client, Client}|Args]),
    {senders, Senders_to_inform}={senders,get_val(senders, Hooked_Args)},
    io:format("~p\n",[Senders_to_inform]),
    {data, Data_to_send}={data,get_val(data, Hooked_Args)},
    {client, Hooked_Client}={client,get_val(client, Hooked_Args)},
    {state, State} = {state, get_val(state, Hooked_Args)},
    New_Args=eadc_utils:set_val(client, Hooked_Client, Hooked_Args),
    case is_record(Hooked_Client, client) of
	true ->
	    lists:foreach(fun(#client{inf=CInf}) ->
				  eadc_utils:send_to_client(Hooked_Client, CInf)
			  end, client_all()),
	    client_write(Hooked_Client),
	    eadc_utils:send_to_senders([Hooked_Client#client.sender|Senders_to_inform],
				       Data_to_send);
	false -> %% logoff
	    logoff
    end,
    New_Args.

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

logoff(Msg, State) ->
    null.

get_val(A,B) ->
    eadc_utils:get_val(A,B).


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


stop_client(Client) ->
    Sender=Client#client.sender,
    Sid=Client#client.sid,
    sendn(Sender, cat(["IQUI ",eadc_utils:sid_to_s(Sid)])),%%FIXME
    sockroute:close(Sender),
    client_delete(Sid).

thing_to_sring(Thing) ->
    lists:flatten(io_lib:format("~p",[Thing])).


asd({tcp, Socket, Bin}, StateName, #state{socket=Socket, buf=Buf} = StateData) ->
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
    end.
