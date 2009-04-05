%%%-------------------------------------------------------------------
%%% File    : eadc_user.erl
%%% Author  : JLarky <jlarky@gmail.com>
%%% Description : 
%%%
%%% Created : 16 Mar 2009 by JLarky <jlarky@gmail.com>
%%%-------------------------------------------------------------------
-module(eadc_user).

-include("eadc.hrl").

%% API
-export([init/0]).
-export([access/1]).
-export([get_client_account/1, client_find/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init()
%% Description: function prepare mnesia tables or create them if missing
%%--------------------------------------------------------------------
init() ->
    eadc_app:start_table(account, [{attributes,
				    record_info(fields, account)},
				   {disc_copies, [node()]}], []),
    
    create_admin_account().

%%--------------------------------------------------------------------
%% Function: access(permission)
%% Description: return true if current user have rights for 'permission'
%%--------------------------------------------------------------------
access(_Permission) ->
    Account=get_client_account(self()),
    if Account#account.class==10 ->
	    true;
       true ->
	    false
    end.

%%--------------------------------------------------------------------
%% Function: get_client_account(Pid)
%% Description: return account record or undefinded of client
%%--------------------------------------------------------------------
get_client_account(Pid) ->
    F=fun() ->
	      [Client]=mnesia:match_object(#client{pid=Pid,_='_'}),
	      case Client#client.login of
		  'NO KEY' ->
		      undefined;
		  undefined ->
		      undefined;
		  Login ->
		      [Acc]=mnesia:match_object(#account{login=Login, _='_'}),
		      Acc#account{}
	      end
      end,
    {atomic, Out}=mnesia:transaction(F),
    Out.

%% @spec client_find(ClientPattern) -> [ClientRecord]
%% @doc returns list of Clients which match with ClentPattern
client_find(Client) when is_record(Client, client) ->
    F = fun()->
		mnesia:match_object(Client)
	end,

    case (catch mnesia:transaction(F)) of
	{atomic, Clients} ->
	    Clients;
	_ ->
	    undefined
    end.

%%====================================================================
%% Internal functions
%%====================================================================

create_admin_account() ->
    Admin=eadc_app:get_app_env(admin, []),
    Login=eadc_utils:get_val(login, Admin),
    Pass=eadc_utils:get_val(pass, Admin),
    if (Login == 'NO KEY') or (Pass == 'NO KEY') ->
	    dont_create_account;
       true ->
	    eadc_utils:account_write(
	      #account{login=Login, nick=Login, pass=Pass, class=10})
    end.
