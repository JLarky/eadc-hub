%%%-------------------------------------------------------------------
%%% Author  : JLarky <jlarky@jlarky.info>
%%% Description : Some plugin for something
%%%
%%% Created :  
%%%-------------------------------------------------------------------
-module(plugin_skeleton).
-author('jlarky@gmail.com').

-include("eadc.hrl").

-export([init/0,terminate/0]). %% required for "plugin on/off" commands

-export([chat_msg/1]). %% some hook that you want to catch

%% @spec init() -> ok | {error, String}
%% @doc do init stuff and returns ok or {error, Message} if something's going wrong
init() ->
    ok.
%% @spec terminate() -> ok | {error, String}
%% @doc do terminate stuff and returns ok or {error, Message} if something's going wrong
terminate() ->
    ok.

chat_msg(Args) ->
    ?DEBUG(debug, "chat_msg: ~w~n", [Args]), %% it appears in debug
    Client=eadc_utils:get_val(client, Args), %% extracting 'client' param from 'Args'
    Msg=eadc_utils:get_val(msg, Args), %% extracting 'msg' param from 'Args'

    %% send message to sender
    eadc_utils:info_to_client(Client, "I see your message! You just wrote: "++Msg),
    %% Client is client record that have usefull information like Client#client.nick or Client#client.addr

    %% if we want see Erlang term in chat we have to do like that
    Out=[1,2,3,{1,2,3}], 
    Test=eadc_utils:format("~w", [Out]),
    eadc_utils:error_to_client(Client, Test),

    %% YOU ALWAYS MAST DO THAT
    Args.
