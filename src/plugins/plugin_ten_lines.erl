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

-export([chat_msg/1, user_login/1, init/1]).

-define(ETS_CHECK, case ets:info(plugin_ten_lines) of
		       undefined ->
			   ets:new(plugin_ten_lines, [set, named_table, public]);
		       _ -> ok
		   end).

chat_msg(Args) ->
    ?DEBUG(debug, "plugin 10 lines === ~w", [Args]),
    ?ETS_CHECK,
    ?GET_VAL(msg, Msg), ?GET_VAL(sid, Sid),
    ?GET_VAL(pid, _Pid), ?GET_VAL(nick, Nick),
    New_Msg=[{time(), Sid, eadc_utils:unquote(Msg), Nick}],
    case ets:lookup(plugin_ten_lines, ten_lines) of
	[{ten_lines, [_|Tail]=Msgs}] ->
	    if length(Msgs) > 10 ->
		    Old_Msgs=Tail;
	       true ->
		    Old_Msgs=Msgs
	    end,
	    New_Msgs=Old_Msgs++New_Msg;
	[] ->
	    New_Msgs=New_Msg
    end,
    ets:insert(plugin_ten_lines, {ten_lines, New_Msgs}),
    Args.

user_login(Args) ->
    ?GET_VAL(pid, Pid),
    ?DEBUG(debug, "~w: user_login ~w", [?MODULE, Pid]),
    ?ETS_CHECK,
    [{ten_lines, Msgs}]= ets:lookup(plugin_ten_lines, ten_lines),
    Ten_lines = lists:foldl(fun({_Time, _Sid, Msg, Nick}, Acc) ->
				    lists:concat([Acc, "-", Nick, " писал: ", Msg, "\n"])
			    end, "Последние сообщения хаба:\n", Msgs),
    eadc_utils:info_to_pid(self(), Ten_lines),
    Args.

init(Args) ->
    ?ETS_CHECK,
    Args.
