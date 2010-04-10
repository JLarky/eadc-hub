%%%-------------------------------------------------------------------
%%% Author  : JLarky <jlarky@jlarky.info>
%%% Description : Плагин для хаба по инвайту
%%%
%%% Created :  
%%%-------------------------------------------------------------------
-module(plugin_invite).
-author('jlarky@gmail.com').

-include("eadc.hrl").
-include("eadc_plugin.hrl").

-export([init/0,terminate/0]). %% required

-export([user_login/1,priv_msg/1,chat_msg/1,ctm/1,rcm/1]). %% some hook that you want to catch

%% @spec init() -> ok | {error, String}
%% @doc do init stuff and returns ok or {error, Message} if something's going wrong
init() ->
    [eadc_user:add_permission(X,user)||X<-rigths()],
    ok.
%% @spec terminate() -> ok | {error, String}
%% @doc do terminate stuff and returns ok or {error, Message} if something's going wrong
terminate() ->
    [eadc_user:del_permission(X,user)||X<-rigths()],
    ok.

rigths() ->
    ['main chat','private chat','download','search'].

user_login(Args) ->
    Client=eadc_utils:get_val(client,Args),
    case Client#client.login of
	Login when is_list(Login) ->
	    Args;
	_ ->
	    eadc_utils:info_to_client(Client, "Not invited!"),
	    eadc_utils:set_val(logoff, "Not invited!", Args)
    end.

chat_msg(Args) ->
    Client=eadc_utils:get_val(client,Args),
    case eadc_user:access(Client, 'main chat') of
	false ->
	    eadc_utils:info_to_client(Client, "You don't have permission"),
	    eadc_utils:set_val(senders, [], Args);
	true ->
	    A=eadc_utils:get_val(senders, [], Args),
	    io:format("~p\n", [A]),
	    Args
    end.

priv_msg(Args) ->
    Client=eadc_utils:get_val(client,Args),
    case eadc_user:access(Client, 'private chat') of
	false ->
	    eadc_utils:info_to_client(Client, "You don't have permission"),
	    eadc_utils:set_val(senders, [], Args);
	true ->
	    Args
    end.

rcm(Args) ->
    ctm(Args).

ctm(Args) ->
    Client=eadc_utils:get_val(client,Args),
    case eadc_user:access(Client, 'download') of
	true ->
	    Args;
	false ->
	    eadc_utils:set_val(senders, [], Args)
    end.
