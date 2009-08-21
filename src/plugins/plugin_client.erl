%%%-------------------------------------------------------------------
%%% Author  : JLarky <jlarky@jlarky.info>
%%% Description : Plugin that acts like client
%%%
%%% Created : Thu Jul 23 15:38:23 MSD 2009
%%%-------------------------------------------------------------------
-module(plugin_client).
-author('jlarky@gmail.com').

-include("eadc.hrl").
-include("eadc_plugin.hrl").

-export([init/0,terminate/0]). %% required

-export([init/1,
	 chat_msg/1,
	 master_command/1
	]). %% some hook that you want to catch

init(Args) ->
    init(),
    Args.

%% @spec init() -> ok | {error, String}
%% @doc do init stuff and returns ok or {error, Message} if something's going wrong
init() ->
    eadc_plugin:bot_add(plugin_client, "Test", "Test bot", 5),
    ok.

%% @spec terminate() -> ok | {error, String}
%% @doc do terminate stuff and returns ok or {error, Message} if something's going wrong
terminate() ->
    eadc_plugin:bot_del(plugin_client),
    ok.

chat_msg(Args) ->
    Msg=eadc_utils:get_val(msg, Args),
    case Msg of
	".test" ->
	    A=Msg,
	    eadc_master ! {self(), {command, p_client_start, []}},
	    io:format("~p\n", [A]);
	_ -> ok
    end,
    
    Args.

master_command(Args) ->
    case eadc_utils:get_val(cmd, Args) of
	p_client_start ->
	    p_client_main:start_link();
	_ ->
	    ok
    end,
    Args.
