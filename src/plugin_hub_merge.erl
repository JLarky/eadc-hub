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

-export([chat_msg/1, master_command/1]).

chat_msg(Args) ->
    ?DEBUG(debug, "chat_msg: ~w~n", [Args]),
    {value,{msg,Msg}} = lists:keysearch(msg, 1, Args),
    lists:foreach(fun(Node) ->
			  {eadc_master, Node} ! {self(), {command, 'BMSG', Msg}}
		  end, nodes()),
    false.

master_command(Args) ->
    ?DEBUG(debug, "chat_msg: ~w~n", [Args]),
    %%{value,{cmd, Cmd}} = lists:keysearch(msg, 1, Args),
    false.
