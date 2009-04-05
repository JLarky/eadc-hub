%%%-------------------------------------------------------------------
%%% File    : plugin_jabber_gate.erl
%%% Author  : JLarky <jlarky@gmail.com>
%%% Description : gate to jabber conference
%%%
%%% Created : 22 Mar 2009 by JLarky <jlarky@gmail.com>
%%%-------------------------------------------------------------------
-module(plugin_jabber_gate).

-behaviour(gen_fsm).

%% gen_fsm callbacks
-export([init/1, handle_event/3,handle_sync_event/4,
	 handle_info/3, terminate/3, code_change/4]).

-include("eadc.hrl").

-export(['WAIT FOR SOCKET'/2,
	 'WAIT FOR REPLY'/2,
	'WAIT FOR HANDSHAKE'/2,
	'NORMAL'/2]).

-record(plug_state, {
	  host,
	  port,
	  vhost,
	  conf,
	  secret,
	  socket,
	  tcp_buf="",
	  xml_buf=""
	 }).

-export([user_login/1,user_quit/1,chat_msg/1]).

-export([init/0,terminate/0]).

init() -> init(""),
	  ok.
terminate() -> 
    (catch gen_fsm:send_all_state_event(jabber_server, terminate)),
    ok.

init(Args) when is_list(Args) -> %% plugin hook
    process_flag(trap_exit, true),
    Optins=eadc_app:get_app_env(jabber_bot, error),
    Host=eadc_utils:get_val(ejabberd_host, Optins),
    Port=eadc_utils:get_val(ejabberd_port, Optins),
    Secret=eadc_utils:get_val(ejabberd_secret, Optins),

    io:format("!!~w\n", [gen_fsm:start_link({local, jabber_server}, ?MODULE, #plug_state{host=Host, port=Port,
								 secret=Secret, vhost="dc.punklan.net",
											 %%conf="test@conference.punklan.net"
											 conf="dc@conference.jabber.spbu.ru"
											}, [])]),
    Args;


init(State) when is_record(State, plug_state) ->
    {ok, 'WAIT FOR SOCKET', State, 1}.

'WAIT FOR SOCKET'(timeout, #plug_state{host=Host, port=Port, vhost=Vhost}=State) -> %% gen_fsm callback
    case (catch connect(Host, Port, Vhost)) of
	Sock when is_port(Sock) ->
	    {next_state, 'WAIT FOR REPLY', State#plug_state{socket=Sock, tcp_buf=""}};
	Error ->
	    ?DEBUG(error, "jabber-gate can't connect ~w\n", [Error]),
	    %%timer:sleep(5000),
	    {next_state, 'WAIT FOR SOCKET', State, 1000}
    end;
'WAIT FOR SOCKET'(Any, State) ->
    ?DEBUG(error, "jabber_gate unknown messge ~w\n", [{Any, State}]),
    {next_state, 'WAIT FOR SOCKET', State}.
connect(Host, Port, Vhost) ->
    {ok, Sock} = gen_tcp:connect(Host, Port, [{active, once}]),
    ok = gen_tcp:send(Sock, "<?xml version='1.0'?><stream:stream xmlns:stream='http://etherx.jabber.org/streams' "
		      ++"xmlns='jabber:component:accept' id='dc' to='"++Vhost++"'>"),
    Sock.

'WAIT FOR REPLY'({tcp, Bin}, #plug_state{secret=Secret}=StateData) ->
    ID=get_id(Bin),
    io:format("REPLY ~s ~s \n~w\n", [Bin, ID, StateData]),
    gen_tcp:send(StateData#plug_state.socket, "<handshake>"++string:to_lower(sha1:hexstring(ID++Secret))++"</handshake>"),
    {next_state, 'WAIT FOR HANDSHAKE', StateData};

'WAIT FOR REPLY'(Any, State) ->
    io:format("!!!!!!!!!1!!!1!!1! ~w\n", [{Any, State}]),
    {next_state, 'WAIT FOR REPLY', State}.

'WAIT FOR HANDSHAKE'({handle_xml, Xml}, StateData) ->
    io:format("HANDSHAKE ~p ~w\n", [Xml, StateData]),
    case Xml of
	{xmlelement,"handshake",[],[]} ->
	    lists:foreach(fun(#client{nick=Nick}) ->
				  gen_fsm:send_event(jabber_server, {user_login, Nick})
			  end, eadc_client_fsm:client_all()),
	    {next_state, 'NORMAL', StateData};
	Error ->
	    ?DEBUG(error, "JABBER PLUGIN: WRONG PASSWORD: ~w\n", [Error]),
	    {stop, normal, 'WRONG PASSWORD'}
    end.

'NORMAL'({user_login, Nick}, #plug_state{conf=Conf, vhost=Host}=StateData) ->
    A=xml:element_to_string({xmlelement, "presence", [{"from", Nick++"@"++Host}, {"to", Conf++"/"++Nick}], []}),
    'NORMAL'({send, A}, StateData);
'NORMAL'({user_quit, Nick}, #plug_state{conf=Conf, vhost=Host}=StateData) ->
    A=xml:element_to_string({xmlelement, "presence", [{"from", Nick++"@"++Host}, {"to", Conf},
						     {"type", "unavailable"}], []}),
    'NORMAL'({send, A}, StateData);
'NORMAL'({chat_msg, Nick, Msg}, #plug_state{conf=Conf, vhost=Host}=StateData) ->
    Body={xmlelement, "body", [], [{xmlcdata, Msg}]},
    A=xml:element_to_string({xmlelement, "message", [{"from", Nick++"@"++Host}, {"to", Conf},
						     {"type", "groupchat"}], [Body]}),
    'NORMAL'({send, A}, StateData);

'NORMAL'({handle_xml, Xml}, StateData) ->
    A=(catch handle_xml(Xml, StateData)),
    ?DEBUG(debug, "handle ~s \n~p\nwith error ~w\n", [utf8:to_utf8(lists:flatten(xml:element_to_string(Xml))),Xml, A]),
    {next_state, 'NORMAL', StateData};
'NORMAL'({send, Bin}, #plug_state{socket=Socket}=StateData) ->
    %%io:format("NORMAL send ~s\n", [Bin]),
    case (catch gen_tcp:send(Socket, Bin)) of
	ok -> ok;
	{error,einval} ->
	    case (catch gen_tcp:send(Socket, utf8:to_utf8(lists:flatten(Bin)))) of
		ok -> ok;
		Error -> ?DEBUG(error, "NORMAL send ~w\n\n", [Error])
	    end;
	Error ->
	    ?DEBUG(error, "NORMAL send ~w\n\n", [Error])
    end,
    {next_state, 'NORMAL', StateData};
'NORMAL'(Any, StateData) ->
    io:format("NORMAL !!!!!!!!!!!!!!!!!!!!!!!!!!!! ~w\n", [{Any,StateData}]),
    {next_state, 'NORMAL', StateData#plug_state{tcp_buf=""}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_fms
%%%%%%%%%%%%%%%%%%%%%%
handle_event(terminate, _StateName, StateData) ->
    {stop, normal, StateData};
handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

bla([]) ->
    [];
bla(Arg) ->
    case xml:string_to_element(Arg) of
        {XmlElement, Tail} ->
	    gen_fsm:send_event(jabber_server, {handle_xml, XmlElement}),
	    bla(Tail);
	{_StateName, {xmlelement, _Name, _Args, _Els}, [], _Acc} = A ->
	    A
    end.

handle_info({tcp, Socket, Bin}, 'WAIT FOR REPLY', #plug_state{socket=Socket}=StateData) ->
    inet:setopts(StateData#plug_state.socket, [{active, once}]),
    ?MODULE:'WAIT FOR REPLY'({tcp, Bin}, StateData#plug_state{tcp_buf=""});

handle_info({tcp, Socket, Bin_u}, StateName, 
	    #plug_state{socket=Socket, tcp_buf=Tcp_Buf, xml_buf=Buf}=StateData) ->
    inet:setopts(StateData#plug_state.socket, [{active, once}]),
    %%io:format("0=== ~p\n", [Bin_u]),
    Data=Tcp_Buf++Bin_u,
    case (catch utf8:from_utf8(Data)) of
	{'EXIT', _Error} ->
	    {next_state, StateName, StateData#plug_state{tcp_buf=Data}};
	Bin when is_list(Bin) ->
	    %%io:format("1=== ~p\n", [Bin]),
	    New_Buf=case Buf of
			[] ->
			    bla(Bin);
			{XStateName, XmlElement, [], Acc} ->
			    bla({XStateName, XmlElement, Bin, Acc})
		    end,
	    %%io:format("2=== ~p\n", [New_Buf]),
	    {next_state, StateName, StateData#plug_state{xml_buf=New_Buf, tcp_buf=""}}
    end;

handle_info({tcp_closed,_Socket}, _StateName, StateData) ->
    {next_state, 'WAIT FOR SOCKET', StateData, 1000};

handle_info(Any, StateName, StateData) ->
    io:format("!!!1! ~w\n", [{Any, StateName, StateData}]),
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% plugin callbacks
%%%%%%%%%%%%%%%%%%

user_login(Args) ->
    Nick=eadc_utils:get_val(nick, Args),
    case eadc_user:client_find(#client{nick=Nick, _='_'}) of
	[] -> % ok
	    gen_fsm:send_event(jabber_server, {user_login, Nick}),
	    Args;
	[_|_] -> % nick is used
	    gen_fsm:send_event(self(), {kill, "Nick in use"}),
	    Args
    end.

user_quit(Args) ->
    Sid=eadc_utils:get_val(sid, Args),
    Client=eadc_client_fsm:client_get(Sid),
    Nick=Client#client.nick,
    gen_fsm:send_event(jabber_server, {user_quit, Nick}),
    Args.

chat_msg(Args) ->
    case eadc_utils:get_val(pids, Args) of
	[] -> %some plugin already hooked this message
	    Args;
	_ ->
	    Sid=eadc_utils:get_val(sid, Args),
	    Msg=eadc_utils:get_val(msg, Args),
	    Client=eadc_client_fsm:client_get(Sid),
	    Nick=Client#client.nick,
	    %%io:format("------------------------======================-------------------- ~w\n",[{chat_msg, Nick, Msg}]),
	    (catch gen_fsm:send_event(jabber_server, {chat_msg, Nick, Msg})),
	    eadc_utils:set_val(pids, [], Args)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% dfjvnkdfvfdf
%%%%%%%%%%%%%%%%

handle_xml({xmlelement, Name, Attrs, Els}, State) ->
    ?DEBUG(error, "!!!  handle xml !!!!!", ""),
    Conf=State#plug_state.conf,
    Host=State#plug_state.vhost,
    From=utf8:to_utf8(eadc_utils:get_val("from", Attrs)),
    case Name of
	"message" ->
	    To=utf8:to_utf8(eadc_utils:get_val("to", Attrs)),

	    {from_nick, From_Nick}={from_nick,lists:nthtail(string:len(Conf)+1, From)},
	    {fclient,From_Nick, [From_Client]}={fclient, From_Nick, eadc_user:client_find(#client{nick=From_Nick, _='_'})},
	    

	    {to, [To_Nick, Host]}={to, string:tokens(To, "@")},
	    {client,To_Nick, [Client]}={client, To_Nick, eadc_user:client_find(#client{nick=To_Nick, _='_'})},
	    Pid=Client#client.pid,
	    %%eadc_utils:send_to_pid(Pid, {args, ["BMSG", atom_to_list( Client#client.sid), "sadsdfvfds"]}),
	    Msg=lists:foldl(fun({xmlelement, Name_, _Attrs_, Els_}, Acc) ->
				    Acc++case Name_ of
					     "body" ->
						 [{xmlcdata,Msg_}]=Els_,
						 Msg_;
					     _ ->
						 ""
					 end end, [], Els),
	    To_Send=utf8:to_utf8(xml:uncrypt(Msg)),
	    
	    From_Sid=From_Client#client.sid,
	    %%io:format("!!!!!! ~w", [Pid]),
	    eadc_utils:send_to_pid(Pid, {args, ["BMSG", eadc_utils:sid_to_s(From_Sid), To_Send]}),
	    %%io:format("handle_xml !!!!! ~w\n", [{message, From_Nick, To_Nick, Client#client.sid, Els, To_Send}]),
	    ok;
	"presence" ->
	    {from_presence,From_Nick}={from_presence,lists:nthtail(string:len(Conf)+1, From)},
	    %%io:format("\n!!!! \n!!!! ~s\n", [From_Nick]),
	    Client = case eadc_user:client_find(#client{nick=From_Nick, _='_'}) of
			 [X] -> X;
			 [] -> #client{}
		     end,
	    
	    case lists:prefix(Host, From) of
		false -> %% not dc client
		    case eadc_utils:get_val("type", Attrs) of
			"unavailable" -> %% do logout
			    Sid=Client#client.sid,
			    eadc_client_fsm:client_delete(Sid),
			    eadc_utils:broadcast({string, "IQUI "++eadc_utils:sid_to_s(Sid)}),
			    %%io:format("LOGOUT ~s\n", [atom_to_list(Sid)]),
			    ok;
			_other -> %% do login
			    case eadc_user:client_find(#client{nick=From_Nick, _='_'}) of
				[Client] when is_record(Client, client) -> %% already logged
				    ?DEBUG(error, "~s\n", [already_logged]);
				_ ->
				    ?DEBUG(error, "~s\n", [login]),
				    Cid=eadc_client_fsm:get_unical_cid(),
				    Sid=eadc_client_fsm:get_unical_SID(),
				    Inf="BINF "++eadc_utils:sid_to_s(Sid)++" ID"++Cid++" NI"++From_Nick++" DE"++Conf,
				    eadc_utils:broadcast({string, Inf}),
				    eadc_client_fsm:client_write(#client{cid=Cid, sid=Sid, nick=From_Nick, inf=Inf, pid=jabber_gate}),
				    ok
			    end,
			    ok
		    end,
		    ok;
		true ->
		    ok
	    end,
	    ?DEBUG(debug, "handle_xml !!!!! ~w\n", [{presence, From_Nick, Els}]),
	    ok;
	"iq" ->
	    To=eadc_utils:get_val("to", Attrs),
	    Type=eadc_utils:get_val("type", Attrs),
	    Id=eadc_utils:get_val("id", Attrs),
	    if ( (To==Host) and (Type == "get")) ->
		    %%io:format("handle_xml iq  :\n~s\n", [lists:flatten(xml:element_to_string({xmlelement, Name, Attrs, Els}))]),
		    gen_fsm:send_event(jabber_server, {send, "<iq from='"++Host++"' to='"++From++"' type='result' id='"++Id++"'>
<query xmlns='http://jabber.org/protocol/disco#info'>
<identity category='gateway' type='dc' name='DC gate' /></query></iq>"});
	       true ->
		    ?DEBUG(error, "handle_xml iq Other !!!!! :\n~s\n", [utf8:to_utf8(lists:flatten(xml:element_to_string({xmlelement, Name, Attrs, Els})))])
	    end,
	    ok;
	Other ->
	    ?DEBUG(error, "handle_xml Other !!!!! ~s:\n~s\n", [Other,lists:flatten(xml:element_to_string({xmlelement, Name, Attrs, Els}))]),
	    ok
    end.
	    
	    


%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions 
%%%%%%%%%%%%%%%%%%
get_id(String) ->
    {match,N, L}=regexp:match(String, " id=(.*) "),
    S=lists:nthtail(N+4, String),
    lists:sublist(S, L-7).
