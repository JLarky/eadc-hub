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
-export([access/1,access/2,is_user/1]).
-export([get_client_account/1, client_find/1]).
-export([add_permission/2,del_permission/2]).

%% Account functions
-export([account_write/1, account_all/0, account_get/1, account_get_login/2]).


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
    Run=fun()->
		lists:map(fun(Perm)-> mnesia:dirty_write(#permission{roles=[op],permission=Perm})
			  end, ['reg user','drop any','kick any','change topic','set files'])
	end,
    eadc_app:start_table(permission, [{attributes,
				       record_info(fields, permission)},
				      {disc_copies, [node()]}], [{run, Run}]),
    ok.

access(Account, Permission) when is_record(Account,account) ->
    access(Permission, Account);
access(Permission, Account) when is_record(Account,account) ->
    case lists:member(root, Account#account.roles) of
	true -> %% access allways true for roots
	    true;
	false ->
	    role_access(Account#account.roles,Permission)
    end.
access(Permission) ->
    role_access([anonymous], Permission).

role_access(Roles, Permission) ->
    Roles_allowed=case mnesia:dirty_read(permission, Permission) of
		      [Perm] ->
			  Perm#permission.roles;
		      _ -> []
		  end,
    lists:any(fun(Role_a) -> lists:member(Role_a,Roles) end, Roles_allowed).

is_user(#account{roles=Roles}=_Account) when Roles==[anonymous]->
    true;
is_user(Account) when is_record(Account,account) ->
    false.

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

%% @spec add_permission(Permission, Role) -> ok
%% @doc Addes permission to role
add_permission(Permission, Role) ->
    Roles=case mnesia:dirty_read(permission, Permission) of
	      [Perms] -> Perms#permission.roles;
	      [] -> []
	  end,
    case lists:member(Role,Roles) of
	true -> 
	    'already added';
	false ->
	    Record=#permission{permission=Permission,roles=[Role|Roles]},
	    mnesia:dirty_write(Record),ok
    end.

%% @spec del_permission(Permission, Role) -> ok
%% @doc Deletes permission to role
del_permission(Permission, Role) ->
    Roles=case mnesia:dirty_read(permission, Permission) of
	      [Perms] -> Perms#permission.roles;
	      [] -> []
	  end,
    case lists:member(Role,Roles) of
	false ->
	    'not exist';
	true ->
	    NewRoles=lists:delete(Role,Roles),
	    Record=#permission{permission=Permission,roles=NewRoles},
	    case NewRoles of
                [] ->
                    %% удаляет запись если удаляем последнюю роль из права.
                    mnesia:dirty_delete_object(Record#permission{roles=Roles});
                _ ->
                    mnesia:dirty_write(Record)
            end,ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Account functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @type account() = record(account)
%% @spec account_write(account()) -> {atomic, ok} | {aborted, Reason}
%% @doc writes account into mnesia table  
account_write(Account) when is_record(Account, account)->
    F=fun() ->
	      mnesia:write(Account)
      end,
    mnesia:transaction(F).

%% @spec account_all() -> Accounts | {error, Error}
%% Accounts = [account()]
%% @doc returns list of all accounts
account_all() ->
    F = fun()->
		mnesia:match_object(#account{_='_'})
	end,
    case (catch mnesia:transaction(F)) of
	{atomic, List} when is_list(List) ->
	    List;
	Error ->
	    {error, Error}
    end.

%% @spec account_get(string()) -> Account
%% Account = account()
%% @doc returns account record with Login
account_get(Login) ->
    F = fun()->
		mnesia:match_object(#account{login=Login,_='_'})
	end,

    case (catch mnesia:transaction(F)) of
	{atomic, [Account]} ->
	    Account#account{};
	_ ->
	    #account{roles=[anonymous]}
    end.

%% @spec account_get_login(string(), string()) -> {login, Ligin::string()} | false
%% @doc finds account with this Nick or Cid.
account_get_login(Nick, Cid) ->
    MatchHead = #account{cid='$1', nick='$2', _='_', login='$3'},
    Guard = [{'or',{'==','$2',Nick},{'==','$1',Cid}}], Result = '$3',
    F = fun() ->
		mnesia:select(account,[{MatchHead, Guard, [Result]}])
	end,
    A=(catch mnesia:transaction(F)),
    case A of
	{atomic, [Log]} ->
	    Log;
	_ ->
	    undefined
    end.



%%====================================================================
%% Internal functions
%%====================================================================

