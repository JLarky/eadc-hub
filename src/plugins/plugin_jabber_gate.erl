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
	  tcp_buf=""
	 }).

-export([user_login/1,user_quit/1,chat_msg/1]).

init(Args) when is_list(Args) -> %% plugin hook
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
    end.

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

'WAIT FOR HANDSHAKE'({tcp, Bin}, StateData) ->
    io:format("HANDSHAKE ~s ~w\n", [Bin, StateData]),
    case Bin of
	"<handshake/>" ->
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

'NORMAL'({tcp, Bin}, StateData) ->
    case (catch string_to_element(Bin)) of
	List when is_list(List) ->
	    lists:foreach(
	      fun(E) -> case (catch handle_xml(E, StateData)) of
			    ok -> ok;
			    Error -> io:format("handle ~s \nwith error ~w\n", [utf8:to_utf8(lists:flatten(xml:element_to_string(E))), Error])
			end end, List);
	Error ->
	    io:format("NORMAL unknown ~w\n", [Error])
    end,
    {next_state, 'NORMAL', StateData};
'NORMAL'({send, Bin}, #plug_state{socket=Socket}=StateData) ->
    %%io:format("NORMAL send ~s\n", [Bin]),
    case (catch gen_tcp:send(Socket, Bin)) of
	Any ->
	    io:format("NORMAL send ~w\n\n", [Any])
    end,
    {next_state, 'NORMAL', StateData};
'NORMAL'(Any, StateData) ->
    io:format("NORMAL !!!!!!!!!!!!!!!!!!!!!!!!!!!! ~w\n", [{Any,StateData}]),
    {next_state, 'NORMAL', StateData#plug_state{tcp_buf=""}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_fms
%%%%%%%%%%%%%%%%%%%%%%
handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

handle_info({tcp, Socket, Bin}, 'WAIT FOR REPLY', #plug_state{socket=Socket}=StateData) ->
    inet:setopts(StateData#plug_state.socket, [{active, once}]),
    ?MODULE:'WAIT FOR REPLY'({tcp, Bin}, StateData#plug_state{tcp_buf=""});

handle_info({tcp, Socket, Bin}, StateName, #plug_state{socket=Socket, tcp_buf=Buf}=StateData) ->
    inet:setopts(StateData#plug_state.socket, [{active, once}]),
    Data=Buf++Bin,
    case (catch string_to_element(Data)) of
	[{xmlelement, _name, _attr, _els}|_] ->
	    %%io:format("!--------!! ~s\n", [Data]),
	    ?MODULE:StateName({tcp, Data}, StateData#plug_state{tcp_buf=""});
	_Error ->
	    %%io:format("handle_info tcp ~w\n", [Error]),
	    {next_state, StateName, StateData#plug_state{tcp_buf=Data}}
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
    gen_fsm:send_event(jabber_server, {user_login, Nick}),
    Args.

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
	    io:format("------------------------======================-------------------- ~w\n",[{chat_msg, Nick, Msg}]),
	    gen_fsm:send_event(jabber_server, {chat_msg, Nick, Msg}),
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
	    To_Send=utf8:to_utf8(Msg),
	    
	    From_Sid=From_Client#client.sid,
	    %%io:format("!!!!!! ~w", [Pid]),
	    eadc_utils:send_to_pid(Pid, {args, ["BMSG", atom_to_list(From_Sid), To_Send]}),
	    %%io:format("handle_xml !!!!! ~w\n", [{message, From_Nick, To_Nick, Client#client.sid, Els, To_Send}]),
	    ok;
	"presence" ->
	    {from_presence,From_Nick}={from_presence,lists:nthtail(string:len(Conf)+1, From)},
	    io:format("\n!!!! \n!!!! ~s\n", [From_Nick]),
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
			    eadc_utils:broadcast({string, "IQUI "++atom_to_list(Sid)}),
			    io:format("LOGOUT ~s\n", [atom_to_list(Sid)]),
			    ok;
			_other -> %% do login
			    case eadc_user:client_find(#client{nick=From_Nick, _='_'}) of
				[Client] when is_record(Client, client) -> %% already logged
				    ?DEBUG(error, "~s\n", [already_logged]);
				_ ->
				    ?DEBUG(error, "~s\n", [login]),
				    Cid=eadc_client_fsm:get_unical_cid(eadc_utils:random_base32(39)),
				    Sid=eadc_client_fsm:get_unical_SID(),
				    Inf="BINF "++Sid++" ID"++Cid++" NI"++From_Nick++" DE"++Conf,
				    eadc_utils:broadcast({string, Inf}),
				    eadc_client_fsm:client_write(#client{cid=Cid, sid=list_to_atom(Sid), nick=From_Nick, inf=Inf, pid=jabber_gate}),
				    ok
			    end,
			    ok
		    end,
		    ok;
		true ->
		    ok
	    end,
	    io:format("handle_xml !!!!! ~w\n", [{presence, From_Nick, Els}]),
	    ok;
	"iq" ->
	    To=eadc_utils:get_val("to", Attrs),
	    Type=eadc_utils:get_val("type", Attrs),
	    Id=eadc_utils:get_val("id", Attrs),
	    if ( (To==Host) and (Type == "get")) ->
		    io:format("handle_xml iq  :\n~s\n", [lists:flatten(xml:element_to_string({xmlelement, Name, Attrs, Els}))]),
		    gen_fsm:send_event(jabber_server, {send, "<iq from='"++Host++"' to='"++From++"' type='result' id='"++Id++"'>
<query xmlns='http://jabber.org/protocol/disco#info'>
<identity category='gateway' type='dc' name='DC gate' /></query></iq>"});
	       true ->
		    io:format("handle_xml iq Other !!!!! :\n~s\n", [utf8:to_utf8(lists:flatten(xml:element_to_string({xmlelement, Name, Attrs, Els})))])
	    end,
	    ok;
	Other ->
	    io:format("handle_xml Other !!!!! ~s:\n~s\n", [Other,lists:flatten(xml:element_to_string({xmlelement, Name, Attrs, Els}))]),
	    ok
    end.
	    
	    


%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions 
%%%%%%%%%%%%%%%%%%
get_id(String) ->
    {match,N, L}=regexp:match(String, " id=(.*) "),
    S=lists:nthtail(N+4, String),
    lists:sublist(S, L-7).

string_to_element([]) ->
    [];
string_to_element(String) ->
    %%io:format("string_to_element ~s\n", [String]),
    {Xml, Tail}=xmerl_scan:string(String, [{validation, false},{quiet, true}]),
    [parse_(Xml)]++string_to_element(Tail).

parse_(Res) ->
    case Res of
	{xmlElement,Element,Element,[],
	 _xmlNamespace,
	 _,_num,
	 XmlAttribute, InnerXML,
	 _lang,_some_path,undeclared}->
	    if InnerXML /= [] ->
		    Els=lists:map(fun(Xml) -> parse_(Xml) end, InnerXML);
	       true ->
		    Els=[]
	    end,
	    {xmlelement, atom_to_list(Element), parse_attr(XmlAttribute), Els};
	{xmlText,_,_,[],Val,text} ->
	    {xmlcdata, Val};
	[] ->
	    []
    end.

parse_attr(Attrs) ->
    parse_attr(Attrs, []).
parse_attr([], Out) ->
    lists:reverse(Out);
parse_attr([XmlAttribute| Tail], Out) ->
    {xmlAttribute,Name,_,_,_hz,_,_num,_, Val,false}=XmlAttribute,
    parse_attr(Tail, [{atom_to_list(Name), Val}|Out]).
