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
-include("eadc_plugin.hrl").

-export([user_login/1, chat_msg/1, user_quit/1,
	 master_command/1]).

-export([init/0,terminate/0]).

init() ->
     ok.
terminate() ->
     ok.


user_login(Args) ->
    ?DEBUG(debug, "user_l: ~w~n", [Args]),
    ?GET_VAL(sid, Sid),
    ?GET_VAL(pid, Pid),
    ?GET_VAL(inf, Inf),
    ?SEND_TO_NODES({command, 'new_client', [{inf, Inf}, {pid, Pid}]}), 
    ?DEBUG(debug, "user_l: ~w~n", [{Sid,Pid}]),
    Args.

chat_msg(Args) ->
    ?DEBUG(debug, "chat_msg: ~w~n", [Args]),
    ?GET_VAL(msg, Msg),
    ?GET_VAL(sid, Sid),
    ?SEND_TO_NODES({command, chat_msg, [{msg, Msg}, {sid, Sid}]}),
    Args.

user_quit(Args) ->
    ?GET_VAL(msg, Msg),
    ?SEND_TO_NODES({command, 'user_quit', [{msg, Msg}]}), 
    ?DEBUG(debug, "User quit ~s", [Msg]),
    Args.

master_command(Args) ->
    ?DEBUG(debug, "chat_msg: ~w~n", [Args]),
    ?GET_VAL(cmd, Cmd),
    ?GET_VAL(args, Arg),
    case Cmd of
	chat_msg ->
	    m_chat_msg(Arg);
	new_client ->
	    m_new_client(Arg);
	user_quit ->
	    m_user_quit(Arg)
    end,
    Args.

m_chat_msg(Args) ->
    ?GET_VAL(msg, Msg),
    ?GET_VAL(sid, Sid),
    eadc_utils:broadcast(
      fun(Pid) ->
	      eadc_utils:send_to_pid(Pid, {list, ["BMSG", Sid, Msg]})
      end).

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
    eadc_utils:broadcast({string,String}).

broadcast(F) ->
    eadc_utils:broadcast(F).
