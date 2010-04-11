-module(plugin_bot).

-export([user_login/1,
	 chat_msg/1,
	 init/1,
	 user_quit/1
	 %%priv_msg/1
	]).

-include("eadc.hrl").
-include("eadc_plugin.hrl").

-export([init/0,terminate/0]).

% export
-export([get_fields/2]).

init() ->
    eadc_app:start_table(ban, [{attributes,
				record_info(fields, ban)},
			       {disc_copies, [node()]}], []),
    ok.
terminate() ->
     {error, "This plugin can't be stopped"}.

init(_Args) ->
    init().

topic_to_clients(Clients) ->
    Topic=eadc_utils:get_option(mainchat, topic, "No topic set"),
    HubName=eadc_utils:get_option(hub, name, "EADC. ADC hub written in Erlang"),
    F=fun(C) -> eadc_utils:send_to_client(C, {args, ["IINF", "CT32", "VEJLarky's hub", 
						     "NI"++HubName, "DE"++Topic]}) end,
    lists:foreach(F, Clients).

user_login(Args) ->
    Client=eadc_utils:get_val(client,Args),
    eadc_utils:send_to_client(Client, {args, ["ICMD", "Commands\\General\\Help",
					      "TTBMSG\s%[mySID]\s!help\n", "CT15"]}),
    eadc_utils:send_to_client(Client, {args, ["ICMD", "Admin\\redirect",
					      "TTBMSG\s%[mySID]\s!redirectsid\\s%[userSID]"
					      "\\s%[line:Enter URL]\n", "CT15"]}),

    %%eadc_utils:send_to_pid(self(), {args, ["BINF", Bit_sid, "CT5", "ID"++Bot_id, "NItest-room", "DEтестовая комната"]}),
    topic_to_clients([Client]),

    %% check ban
    Nick=Client#client.nick,IP=Client#client.addr,
    %% banned nick ++ banned IP
    BanList=mnesia:dirty_match_object(#ban{nick=Nick, _='_'})++
	mnesia:dirty_match_object(#ban{ip=IP, _='_'}),
    %% find ban with maximum time
    MaxBan=lists:foldl(fun(#ban{time=Time}=Ban, #ban{time=MaxTime}=Acc_Ban) ->
			       case (Time > MaxTime) of %% if bantime not expired
				   true -> Ban;
				   false -> Acc_Ban
			       end
		       end, #ban{time=0}, BanList), 

    Now=eadc_utils:get_unix_timestamp(now()),
    Diff=MaxBan#ban.time-Now,
    case (Diff > 0) of
	true -> %% user still banned
	    #ban{op=OPName,reason=Reason}=MaxBan,
	    BanMsg="Banned by "++OPName++". Reason: "++Reason,
	    eadc_utils:send_to_client(Client, eadc_utils:a2s(["ISTA", "231", BanMsg])),
	    eadc_utils:send_to_client(Client, eadc_utils:a2s(["ISTA", "231", lists:concat([Diff," sec(s) left"])])),
	    eadc_utils:set_val(logoff, "", Args);
	false -> %% ban time is expired
	    motd(Client),
	    Args
    end.

motd(Client) ->
    %% MOTD
    Motd_def="Добро пожаловать в ADC-хаб написанный на Erlang. Страничка проекта http://wiki.github.com/JLarky/eadc-hub на ней можно узнать что такое ADC и почему именно Erlang.
Инструкции по установке http://github.com/JLarky/eadc-hub/blob/master/INSTALL.ru.txt",
    Motd_cfg=eadc_utils:get_option(files, motd, Motd_def),
    %% if Motd_conf is file use this file for MOTD
    MOTD=case (catch file:read_file(Motd_cfg)) of
	     {ok, Motd} ->
		 binary_to_list(Motd);
	     _ ->
		 Motd_cfg
	 end,
    eadc_utils:info_to_client(Client, MOTD).

user_quit(Args) ->
    State=eadc_utils:get_val(state, Args),
    Other=State#state.other,
    Banned=eadc_utils:get_val(banned, Other),
    case Banned of
	true -> %% don't send QUI message for banned users, because they really didn't login
	    _New_Args=eadc_utils:set_val(msg, "", Args);
	_ ->
	    Args
    end.

chat_msg(Args) ->
    ?DEBUG(debug, "chat_msg: ~w~n", [Args]),
    ?GET_VAL(msg, Msg),
    ?GET_VAL(data, Data),
    case Msg of 
	[$!|Command] -> %% command
	    Params=eadc_utils:s2a(Command),
	    State=eadc_utils:get_val(state, Args),
	    Client=eadc_utils:get_val(client, Args),
	    try do_command(Params, State, Client)
	    catch
		throw:{error, Error} ->
		    eadc_utils:error_to_client(Client, Error);
		  Error ->
		    eadc_utils:error_to_client(Client, "Report to admin.\nError: "++
					       eadc_utils:thing_to_string(Error))
	    end,
	    Args1=eadc_utils:set_val(msg, [], Args),
	    eadc_utils:set_val(senders, [], Args1);
	_ -> 
	    eadc_utils:set_val(data, lists:sublist(Data, 512), Args)
    end.

do_command([Command|Args], _State, Client) ->
    Account=eadc_user:account_get(Client#client.login),
    case Command of
	"help" ->
	    Hlp="All hub commands:
User's commands:
 !help - shows this help
 !regme <password> - registers new user with password <password>
 !passwd <password> - changes current user's password to <password>
 !userlist - shows all users with hidden passwords
 !version - shows hub's version info

Admin's commands:
 !regnewuser <username> <pass> - register user with password <pass>
 !topic <topic> - sets hub's topic
 !getconfig - shows all set options
 !setconfig <key> <val> - sets hub's option 
 !setfile motd <Message> - sets MOTD
 !setfile motd <filename> - sets MOTD

Opertor's commands:
!drop <username> <reason> - Drops user from hub
!kick <username> <reason> - Bans user for 5 min and dropes from hub
!ban <username> <minutes> <reason> - Bans user for the time

Plugins:
 !plugin on <plugin> - turns on plugin and adds it to autostart
 !plugin off <plugin> - turns off plugin and removes it from autostart
 !pluginlist - shows all running plugins

Roles:
 !addtorole <role> <permission> - addes permission to role
 !delfromrole <role> <permission> - deletes permission from role
 !addrole <role> <login> - addes role to login
 !delrole <role> <login> - deletes role from login\n",
	    eadc_utils:info_to_client(Client, Hlp);
	"version" ->
	    FileName="../.git/refs/heads/master",
	    case file:read_file(FileName) of
		{ok, File} when is_binary(File) ->
		    Commit=binary_to_list(File),
		    Out="http://github.com/JLarky/eadc-hub/commit/"++Commit;
		_ ->
		    Out="File "++FileName++" not found"
	    end,
	    eadc_utils:info_to_client(Client, Out);
	"regnewuser" ->
	    case eadc_user:access(Account,'reg user') of
		true ->
		    case get_fields(Args, 2) of
			[UserName|Pass_] ->
			    Pass=string:join(Pass_, " "),
			    Ok=eadc_user:account_write(#account{login=UserName, nick=UserName,
								 pass=Pass, roles=[user]}),
			    eadc_utils:info_to_client(Client, lists:flatten(io_lib:format("~p",
										       [Ok])));
			_ -> throw({error, "usage: !regnewuser <name> <pass>"})
		    end;
		false ->
		    throw({error, "You don't have permission."})
	    end;
	"regme" ->
	    case get_fields(Args, 1) of
		[Pass] -> ok;
		Pass   -> throw({error, "usage: !regme <pass>"})
	    end,
	    UserName=Client#client.nick,
	    case mnesia:table_info(account, size) of
		0 -> %% first user
		    eadc_utils:info_to_client(Client, "Register superuser."),
		    Roles=[user,root];
		_ ->
		    Roles=[user]
	    end,
	    case Client#client.login of
		undefined ->
		    fine;
		UserLogin ->
		    throw({error,"You are allredy registered as '"++UserLogin++"'"})
	    end,
	    Access=eadc_user:access(Account,'self registration'),
	    case Access or (Roles==[user,root]) of
		true ->
		    {atomic, ok}=eadc_user:account_write(#account{login=UserName, nick=UserName,
								   pass=Pass,roles=Roles}),
		    eadc_utils:info_to_client(Client, "User '"++UserName++"' was registered");
		false ->
		    eadc_utils:error_to_client(Client, "You don't have permission.")
	    end;
	"passwd" ->
	    case get_fields(Args, 1) of
		[Pass] -> ok;
		Pass   -> throw({error, "usage: !passwd <pass>"})
	    end,
	    
	    case eadc_user:is_user(Account) of
		false ->
		    eadc_utils:error_to_client(Client, "You are not registred");
		true ->
		    {atomic, ok}=eadc_user:account_write(Account#account{pass=Pass}),
		    eadc_utils:info_to_client(Client, "Password was successfully changed")
	    end;
	"drop" ->
	    case eadc_user:access(Account,'drop any') of
		true ->
		    case get_fields(Args, 2) of
			[UserName, Reason] ->
			    OPName=Client#client.nick,
			    drop(Client,UserName, OPName, Reason);
			_ -> throw({error, "usage !drop <name> <reason>"})
		    end;
		false ->
		    eadc_utils:info_to_client(Client, "You don't have permission.")
	    end;
	"kick" ->
	    case eadc_user:access(Account,'kick any') of
                true ->
		    case get_fields(Args, 2) of
                        [UserName, Reason] ->
			    OPName=Client#client.nick,
			    ban(Client,UserName, OPName, Reason),
			    drop(Client,UserName, OPName, Reason);
			_ -> throw({error, "usage !kick <name> <reason>"})
                    end;
		false ->
		    eadc_utils:info_to_client(Client, "You don't have permission.")
	    end;
	"ban" ->
	    case eadc_user:access(Account,'ban any') of
                true ->
		    case get_fields(Args, 3) of
                        [UserName, Time_, Reason] ->
			    OPName=Client#client.nick,
			    case (catch list_to_integer(Time_)) of
				Time when is_integer(Time) ->
				    ban(UserName, OPName, Time, Reason);
				_ ->
				    throw({error, "usage !ban <name> <minutes> <reason>"})
			    end;
			_ -> throw({error, "usage !ban <name> <minutes> <reason>"})
                    end;
		false ->
		    eadc_utils:info_to_client(Client, "You don't have permission.")
	    end;
	"userlist" ->
	    List=lists:map(fun(A) -> A#account{pass="***"} end, eadc_user:account_all()),
	    eadc_utils:info_to_client(Client, eadc_utils:format("Users:\n~p", [List]));
	"topic" ->
	    case eadc_user:access(Account,'change topic') of
		true ->
		    Topic=string:join(Args, " "),
		    _Ok=eadc_utils:set_option(mainchat, topic, Topic),
		    topic_to_clients(eadc_client:client_all()),
		    eadc_utils:broadcast({info, Client#client.nick++" sets the topic to: "++Topic});
		false ->
		    eadc_utils:info_to_client(Client, "You don't have permission.")
	    end;
	"setconfig" ->
	    case eadc_user:access(Account,'set config') of
		true ->
                    case get_fields(Args, 2) of
			[Key | Rest] ->
			    Val=string:join(Rest, " "),
			    _Ok=eadc_utils:set_option(hub, list_to_atom(Key), Val),
			    eadc_utils:info_to_client(Client, "OK");
			_ -> throw({error, "usage: !setconfig <key> <val>"})
		    end;
		false ->
		    eadc_utils:info_to_client(Client, "You don't have permission.")
	    end;
	"getconfig" ->
	    case eadc_user:access(Account,'get config') of
		true ->
		    AllOptions=eadc_utils:get_options({option, {hub, '_'}, '_'}),
		    Options=lists:map(fun(E) -> {hub, Key}=E#option.id,Val=E#option.val, 
						lists:concat([Key," => '",Val,"'\n"]) end,
				      AllOptions),
		    Out=string:join(Options, " "),
		    eadc_utils:info_to_client(Client, "Config list:\n "++Out);
		false ->
		    eadc_utils:info_to_client(Client, "You don't have permission.")
	    end;
        "setfile" ->
            case eadc_user:access(Account,'set files') of
                true ->
                    case get_fields(Args, 2) of
			[Key | Rest] ->
			    Val=string:join(Rest, " "),
			    _Ok=eadc_utils:set_option(files, list_to_atom(Key), Val),
			    eadc_utils:info_to_client(Client, "OK");
			_ -> throw({error, "usage: !setfile <key> <val>"})
		    end;
                false ->
                    eadc_utils:info_to_client(Client, "You don't have permission.")
            end;
	"plugin" ->
	    case eadc_user:access(Account,'plugin manage') of
                true ->
		    ok;
		false ->
		    throw({error,"You don't have permission."})
	    end,
	    case get_fields(Args, 2) of
		[_,_] -> ok;
		_ -> throw({error, "usage: !plugin on/off <name>"})
	    end,
	    {MName,F,M}=case Args of
			    ["on", Name|_] ->
				{list_to_atom(Name),init, "initialized."};
			    ["off", Name|_] ->
				{list_to_atom(Name),terminate, "terminated"};
			    _ ->
				throw({error, "Wrong parametrs."})
			end,
	    PL=eadc_plugin:get_plugins(),
	    New_PL=case {F,lists:member(MName,PL)} of
		       {init, true} ->
			   throw({error, "Plugin already loaded"});
		       {terminate,false} ->
			   throw({error, "Plugin not loaded"});
		       {init, false} ->
			   PL++[MName];
		       {terminate,true} ->
			   lists:delete(MName,PL)
		   end,
	    case (catch MName:F()) of
		{'EXIT',{undef,[{MName,F,[]}|_]}} ->
		    throw({error,"Plugin "++atom_to_list(MName)++" can't be "++M++"."});
		ok ->
		    eadc_plugin:set_plugins(New_PL),
		    eadc_utils:info_to_client(Client, "Plugin "++atom_to_list(MName)
					   ++" have been "++M++".");
		{error, Error} ->
		    eadc_utils:info_to_client(Client, lists:flatten(Error))
	    end;
	"pluginlist" ->
	    case eadc_user:access(Account,'plugin manage') of
                true ->
		    PL=eadc_plugin:get_plugins(),
		    Out=lists:foldl(fun(Pname, Acc) -> Acc++"\n"++atom_to_list(Pname) end,"",PL),
		    eadc_utils:info_to_client(Client, Out);
		false ->
		    eadc_utils:info_to_client(Client, "You don't have permission.")
	    end;
	"redirect" ->
	    case eadc_user:access(Account,'redirect') of
                true ->
		    [Nick|_]=Args,
		    [Cl|_]=eadc_user:client_find(#client{nick=Nick, _='_'}),
		    eadc_utils:redirect_to(Cl, "dchub://jlarky.punklan.net");
		false ->
		    eadc_utils:info_to_client(Client, "You don't have permission.")
	    end;
	"redirectsid" ->
	    case eadc_user:access(Account,'redirect') of
		true ->
		    ok;
		false ->
		    throw({error,"You don't have permission."})
            end,
	    [Sid_|Url_]= get_fields(Args,2),
	    Sid=eadc_utils:unbase32(Sid_),[Url|_]=Url_,
	    Cl=eadc_client:client_get(Sid),
	    eadc_utils:redirect_to(Cl, Url);
	"redirectall" ->
	    case eadc_user:access(Account,'redirect all') of
		true ->
		    ok;
		false ->
		    throw({error,"You don't have permission."})
            end,
	    [Url|_]=Args,
	    F=fun(User_Client) ->
		      eadc_utils:redirect_to(User_Client, Url),
		      io:format("~p\n", [{User_Client#client.nick, Url}])
	      end,
	    lists:foreach(F, eadc_client:client_all());
	Command when (Command=="addtorole") or (Command=="delfromrole") ->
            case eadc_user:access(Account,'change permission') of
                true ->  ok;
                false -> throw({error,"You don't have permission."})
            end,
	    [Role|Permission_]= case get_fields(Args,2) of
				   A=[_,_] -> A;
				   _ -> throw({error, "Error: see help for usage"})
			       end,
	    Permission=list_to_atom(string:join(Permission_, " ")),
	    case mnesia:dirty_read(permission, Permission) of
		[Perms] ->
		    Roles=Perms#permission.roles;
		[] ->
		    Roles=[]
	    end, NewRoles=
		case {Command,lists:member(list_to_atom(Role),Roles)} of
		    {"addtorole",false}  -> [list_to_atom(Role)|Roles];
		    {"delfromrole",true} -> lists:delete(list_to_atom(Role), Roles);
		    {"addtorole", true} -> throw({error, "Already added"});
		    {"delfromrole", false} -> throw({error, "No such permission in role"})
		end,
	    Record=#permission{permission=Permission,roles=NewRoles},
	    case NewRoles of
		[] ->
		    %% удаляет запись если удаляем последнюю роль из права.
		    mnesia:dirty_delete_object(Record#permission{roles=Roles});
		_ ->
		    mnesia:dirty_write(Record)
	    end,
	    eadc_utils:info_to_client(Client, "OK");
	Command when (Command=="addrole") or (Command=="delrole") ->
            case eadc_user:access(Account,'change permission') of
                true ->  ok;
                false -> throw({error,"You don't have permission."})
            end,
	    [Role|Login_]=case get_fields(Args,2) of
			     A=[_,_] -> A;
			     _ -> throw({error, "Error: see help for usage"})
			 end,
	    Login=string:join(Login_," "),
	    Acc=eadc_user:account_get(Login),
	    Roles=case eadc_user:is_user(Acc) of
		      true ->
			  Acc#account.roles;
		      _ ->
			  throw({error, "Account not found. Name of account is case-sensetive."})
		  end, NewRoles=
		case {Command,lists:member(list_to_atom(Role),Roles)} of
		    {"addrole",false}-> [list_to_atom(Role)|Roles];
		    {"delrole",true} -> lists:delete(list_to_atom(Role), Roles);
		    {"addrole",true} -> throw({error, "Already added"});
		    {"delrole",false}-> throw({error, "Account doesn't have this role"})
		end,
	    eadc_user:account_write(Acc#account{roles=NewRoles}),
	    eadc_utils:info_to_client(Client, "OK");
	_ ->
	    eadc_utils:info_to_client(Client, "Unknown command '"++Command++"'")
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utils
%%%%%

drop(Client,UserName, OPName, Reason) ->
    try
	[User_Client]=eadc_user:client_find(#client{nick=UserName, _='_'}),
	eadc_utils:broadcast({info, UserName++" dropped by "++OPName++". Reason: "++Reason}),
	eadc_client:logoff(User_Client, "Dropped")
    catch
	error:{badmatch,[]} ->
	    eadc_utils:info_to_client(Client, "User not found.")
    end.

ban(Me,UserName, OPName, Reason) ->
    ban(Me,UserName, OPName, 5, Reason).

ban(Me,UserName, OPName, Time, Reason) ->
    Client=
	case eadc_user:client_find(#client{nick=UserName, _='_'}) of
	    [] -> throw({error, "User '"++UserName++"' not found."});
	    [Client_]  -> Client_
	end,
    IP=Client#client.addr,
    ban(Me,UserName, OPName, IP, Time, Reason).

ban(Me,UserName, OPName, IP, Time, Reason) when is_integer(Time) ->
    Ban=#ban{nick=UserName, ip=IP, time=eadc_utils:get_unix_timestamp(now())+Time*60,
	     op=OPName, reason=Reason},
    ok=mnesia:dirty_write(Ban),
    eadc_utils:info_to_client(Me, "User '"++UserName++"' banned").


%% @spec get_field(Args::list(), Number_of_Fields::integer()) -> List | []
%% @doc Takes List of N strings, and makes list of NoF while N >= NoF and returns []
%% if N is not enough
%% @end
get_fields(Args, Number_of_Fields) when is_integer(Number_of_Fields) ->
    get_fields(Args, Number_of_Fields, _Acc=[]).

get_fields([], Number_of_Fields, _Acc) when Number_of_Fields > 0->
    Number_of_Fields;
get_fields([], Number_of_Fields, Acc) when Number_of_Fields == 0->
    Acc;
get_fields(Args, Number_of_Fields, Acc) when Number_of_Fields == 1-> 
    Acc++[string:join(Args, " ")];
get_fields([Head|Tail], Number_of_Fields, Acc) when Number_of_Fields > 1-> 
    get_fields(Tail, Number_of_Fields-1, Acc++[Head]).

