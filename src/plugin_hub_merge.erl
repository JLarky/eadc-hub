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
    %% ICMD Luadch\sCommands\\General\\Help TTBMSG\s%[mySID]\s+help\\s\n CT1
    {string, String} = eadc_utils:convert({args, ["ICMD", "Luadch\sCommands\\General\\Help",
						  "TTBMSG\s%[mySID]\s+help\\s\n", "CT1"]}),
    gen_fsm:send_event(Pid, {send_to_socket, String}),
    ?SEND_TO_NODES({command, 'new_client', [{inf, Inf}, {pid, Pid}]}), 
    ?DEBUG(debug, "user_l: ~w~n", [{Sid,Pid}]),
    false.

chat_msg(Args) ->
    ?DEBUG(debug, "chat_msg: ~w~n", [Args]),
    ?GET_VAL(msg, Msg),
    ?GET_VAL(sid, Sid),
    ?SEND_TO_NODES({command, chat_msg, [{msg, Msg}, {sid, Sid}]}),
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
	chat_msg ->
	    m_chat_msg(Arg);
	new_client ->
	    m_new_client(Arg);
	user_quit ->
	    m_user_quit(Arg)
    end,
    false.

m_chat_msg(Args) ->
    ?GET_VAL(msg, Msg),
    ?GET_VAL(sid, Sid),
    {string, String} = eadc_utils:convert({list, ["BMSG", Sid, Msg]}),
    broadcast_string(String).

m_new_client(Args) ->
    ?GET_VAL(inf, Inf),
    ?GET_VAL(pid, Pid),
    broadcast_string(Inf),
    broadcast(fun(Client) ->
		      gen_fsm:send_event(Client, {new_client, Pid})
	      end),
    ?DEBUG(debug, "!!!====new_client====!!!! ~w", [Pid]).

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
