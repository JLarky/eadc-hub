%%%-------------------------------------------------------------------
%%% File    : plugin_ten_lines.erl
%%% Author  : JLarky <jlarky@gmail.com>
%%% Description : 
%%%
%%% Created :  3 Mar 2009 by JLarky <jlarky@gmail.com>
%%%-------------------------------------------------------------------
-module(plugin_ten_lines).

-include("eadc.hrl").
-include("eadc_plugin.hrl").

-export([chat_msg/1, user_login/1]).

-define(ETS_CHECK, case ets:info(plugin_ten_lines) of
		       undefined ->
			   ets:new(plugin_ten_lines, [bag, named_table, public]);
		       _ -> ok
		   end).

chat_msg(Args) ->
    ?DEBUG(debug, "plugin 10 lines === ~w", [Args]),
    ?ETS_CHECK,
    ?GET_VAL(msg, Msg), ?GET_VAL(sid, Sid),
    ?GET_VAL(pid, _Pid), ?GET_VAL(nick, Nick),
    ets:insert(plugin_ten_lines, {time(), Sid, Msg, Nick}),
    Args.

user_login(Args) ->
    ?GET_VAL(pid, Pid),
    ?DEBUG(debug, "~w: user_login ~w", [?MODULE, Pid]),
    ?ETS_CHECK,
    Msgs = 
    ets:foldl(fun({_Time, _Sid, Msg, Nick}, Acc) ->
		      lists:concat([Acc, "-", Nick, " писал: ", Msg, "\n"])
	      end, "Последние сообщения хаба:\n", plugin_ten_lines),
    eadc_utils:send_to_pid(Pid, {args, ["ISTA", "000", Msgs]}),
    Args.
