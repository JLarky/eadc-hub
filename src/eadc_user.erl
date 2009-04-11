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
-export([access/1,access/2]).
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
    eadc_app:start_table(role, [{attributes,
				 record_info(fields, role)},
				{disc_copies, [node()]}], []),
    eadc_app:start_table(permission, [{attributes,
				       record_info(fields, permission)},
				      {disc_copies, [node()]}], []),
    ok.

%% @spec access(atom()) -> true | false
%% @doc returns true if current user have rights for 'permission'. 
%% returns true for users having root role for any 'permission'.
access(Permission) ->
    Account=case get_client_account(self()) of
		Acc when is_record(Acc, account) ->
		    Acc;
		_ ->
		    #account{roles=[anonymous]}
	    end,
    access(Permission, Account).

access(Permission, Account) ->
    case lists:member(root, Account#account.roles) of
	true -> %% access allways true for roots
	    true;
	false ->
	    Roles_allowed=case mnesia:dirty_read(permission, Permission) of
			      [Perm] -> Perm#permission.roles;
			      _ -> []
			  end,
	    Roles_taken=Account#account.roles,
	    lists:any(fun(Role_a) -> lists:member(Role_a,Roles_taken) end, Roles_allowed)
    end.

%%--------------------------------------------------------------------
%% Function: get_client_account(Pid)
%% Description: return account record or undefinded of client
%%--------------------------------------------------------------------
get_client_account(Pid) ->
    case mnesia:dirty_match_object(#client{pid=Pid, _='_'}) of
	[Client] ->
	    case Client#client.login of
		'NO KEY' ->
		    undefined;
		undefined ->
		    undefined;
		Login ->
		{acc,Login,[Acc]}={acc,Login,mnesia:dirty_read(account,Login)},      
		    Acc
	    end;
	_ ->
	    undefined
    end.

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

