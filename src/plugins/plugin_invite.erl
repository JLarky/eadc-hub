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
	    eadc_utils:info_to_pid(self(), "Not invited!"),
	    eadc_utils:set_val(client, Client#client{pid=notinvited}, Args)
    end.

chat_msg(Args) ->
    case eadc_user:access('chat chat') of
	false ->
	    eadc_utils:info_to_pid(self(), "You don't have permission"),
	    eadc_utils:set_val(pids, [], Args);
	true ->
	    A=eadc_utils:get_val(pids, [], Args),
	    io:format("~p\n", [A]),
	    Args
    end.

priv_msg(Args) ->
    case eadc_user:access('private chat') of
	false ->
	    eadc_utils:info_to_pid(self(), "You don't have permission"),
	    eadc_utils:set_val(pids, [], Args);
	true ->
	    Args
    end.

rcm(Args) ->
    ctm(Args).

ctm(Args) ->
    case eadc_user:access('main chat') of
	true ->
	    Args;
	false ->
	    eadc_utils:set_val(pids, [], Args)
    end.
