%%%-------------------------------------------------------------------
%%% File    : plugin_hub_merge.erl
%%% Author  : JLarky <jlarky@jlarky.info>
%%% Description : Плагин для объединения хабов
%%%
%%% Created :  1 Mar 2009 by JLarky <jlarky@jlarky.info>
%%%-------------------------------------------------------------------
-module(plugin_hub_merge).
-author('jlarky@gmail.com').

-include("eadc.hrl").

-export([user_login/1, chat_msg/1, user_quit/1,
	 master_command/1]).

-define(GET_VAL(Key, Val), {value,{Key,Val}} = lists:keysearch(Key, 1, Args)).
-define(SEND_TO_NODES(Msg), lists:foreach(fun(Node) ->
						  {eadc_master, Node} ! {self(), Msg}
					  end, nodes())).
user_login(Args) ->
    ?DEBUG(debug, "user_l: ~w~n", [Args]),
    ?GET_VAL(sid, Sid),
    ?GET_VAL(pid, Pid),
    ?GET_VAL(inf, Inf),
    ?SEND_TO_NODES({command, 'new_client', [{inf, Inf}, {pid, Pid}]}), 
    ?DEBUG(debug, "user_l: ~w~n", [{Sid,Pid}]),
    false.

chat_msg(Args) ->
    ?DEBUG(debug, "chat_msg: ~w~n", [Args]),
    {value,{msg,Msg}} = lists:keysearch(msg, 1, Args),
    lists:foreach(fun(Node) ->
			  {eadc_master, Node} ! {self(), {command, 'BMSG', Msg}}
		  end, nodes()),
    false.

user_quit(Args) ->
    ?GET_VAL(msg, Msg),
    ?SEND_TO_NODES({command, 'user_quit', [{msg, Msg}]}), 
    ?DEBUG(debug, "User quit ~s", [Msg]),
    false.

master_command(Args) ->
    ?DEBUG(debug, "chat_msg: ~w~n", [Args]),
    {value,{cmd, Cmd}} = lists:keysearch(cmd, 1, Args),
    %%{value,{args, Arg}} = lists:keysearch(args, 1, Args),
    ?GET_VAL(args, Arg),
    case Cmd of
	'BMSG' ->
	    m_chat_msg(Arg);
	new_client ->
	    m_new_client(Arg);
	user_quit ->
	    m_user_quit(Arg)
    end,
    false.

m_chat_msg(Msg) ->
    broadcast(
      fun(Client) ->
	      {string, String} = 
		  eadc_utils:convert(
		    {list, ["BMSG", "AAAA",
			    eadc_utils:quote("From other hub: ")++Msg]}),
	      gen_fsm:send_event(Client, {send_to_socket, String})
      end).

m_new_client(Args) ->
    ?GET_VAL(inf, Inf),
    broadcast_string(Inf),
    ?DEBUG(debug, "!!!!!!! ~w", Args).

m_user_quit(Args) ->
    ?GET_VAL(msg, Msg),
    broadcast_string(Msg),
    ?DEBUG(debug, "!!!!!!! ~w", Args).

broadcast_string(String) -> 
    broadcast(
      fun(Client) ->
	      gen_fsm:send_event(Client, {send_to_socket, String})
      end).

broadcast(F) ->
    lists:foreach(F, eadc_client_fsm:all_pids()).
