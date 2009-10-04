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

topic_to_pids(Pids) ->
    Topic=eadc_utils:get_option(mainchat, topic, "No topic set"),
    HubName=eadc_utils:get_option(hub, name, "EADC. ADC hub written in Erlang"),
    F=fun(P) -> eadc_utils:send_to_pid(P, {args, ["IINF", "CT32", "VEJLarky's hub", "NI"++HubName, "DE"++Topic]}) end,
    lists:foreach(F, Pids).

user_login(Args) ->
    eadc_utils:send_to_pid(self(), {args, ["ICMD", "Commands\\General\\Help",
					   "TTBMSG\s%[mySID]\s!help\n", "CT1"]}),
%%    eadc_utils:send_to_pid(self(), {args, ["ICMD", "Admin\\redirect",
%%					   "TTBMSG\s%[mySID]\s!redirectsid\\s%[userSID]\n", "CT2"]}),

    %%eadc_utils:send_to_pid(self(), {args, ["BINF", Bit_sid, "CT5", "ID"++Bot_id, "NItest-room", "DEтестовая комната"]}),
    topic_to_pids([self()]),

    %% check ban
    Nick=eadc_utils:get_val(nick, Args),IP=eadc_utils:get_val(addr, Args),
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
    case (MaxBan#ban.time > Now) of
	true -> %% user still banned
	    #ban{op=OPName,reason=Reason}=MaxBan,
	    BanMsg=eadc_utils:quote("Banned by "++OPName++". Reason: "++Reason),
	    State=eadc_utils:get_val(state, Args),Other=State#state.other,
	    New_State=State#state{other=eadc_utils:set_val(banned, true, Other)},
	    New_Args1=eadc_utils:set_val(client, {logoff,"ISTA 231 "++BanMsg}, Args),
	    New_Args2=eadc_utils:set_val(state, New_State, New_Args1),
	    New_Args2;
	false -> %% ban time is expired
	    motd(),
	    Args
    end.

motd() ->
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
    eadc_utils:info_to_pid(self(), MOTD).

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
		    eadc_utils:error_to_pid(self(), Error);
		  Error ->
		    throw({error,Error})
	    end,
	    Args1=lists:keyreplace(msg, 1, Args, {msg, []}),
	    lists:keyreplace(pids, 1, Args1, {pids, []});
	_ -> 
	    lists:keyreplace(data, 1, Args, {data, lists:sublist(Data, 512)})
    end.

do_command([Command|Args], _State, Client) ->
    case Command of
	"help" ->
	    Hlp="All hub commands:
User's commands:
 !help - shows this help
 !regme <password> - registers new user with password <password>
 !userlist - shows all users with hidden passwords

Admin's commands:
 !regnewuser <username> <pass> - register user with password <pass>
 !regclass <username> <class> - changes user's class to <class>
 !userlist - shows all users with their passwords
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
	    eadc_utils:info_to_pid(self(), Hlp);
	"regnewuser" ->
	    case eadc_user:access('reg user') of
		true ->
		    case get_fields(Args, 2) of
			[UserName|Pass_] ->
			    Pass=string:join(Pass_, " "),
			    Ok=eadc_utils:account_write(#account{login=UserName, nick=UserName,
								 pass=Pass, roles=[user]}),
			    eadc_utils:info_to_pid(self(), lists:flatten(io_lib:format("~p",
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
		    eadc_utils:info_to_pid(self(), "Register superuser."),
		    Roles=[user,root];
		_ ->
		    Roles=[user]
	    end,
	    case Client#client.login of
		'NO KEY' ->
		    fine;
		UserLogin ->
		    throw({error,"You are allredy registered as '"++UserLogin++"'"})
	    end,
	    case eadc_user:access('self registration') or (Roles==[root]) of
		true ->
		    {atomic, ok}=eadc_utils:account_write(#account{login=UserName, nick=UserName,
								   pass=Pass,roles=Roles}),
		    eadc_utils:info_to_pid(self(), "User '"++UserName++"' was registered");
		false ->
		    eadc_utils:error_to_pid(self(), "You don't have permission.")
	    end;
	"drop" ->
	    case eadc_user:access('drop any') of
		true ->
		    case get_fields(Args, 2) of
			[UserName, Reason] ->
			    OPName=Client#client.nick,
			    drop(UserName, OPName, Reason);
			_ -> throw({error, "usage !drop <name> <reason>"})
		    end;
		false ->
		    eadc_utils:info_to_pid(self(), "You don't have permission.")
	    end;
	"kick" ->
	    case eadc_user:access('kick any') of
                true ->
		    case get_fields(Args, 2) of
                        [UserName, Reason] ->
			    OPName=Client#client.nick,
			    ban(UserName, OPName, Reason),
			    drop(UserName, OPName, Reason);
			_ -> throw({error, "usage !kick <name> <reason>"})
                    end;
		false ->
		    eadc_utils:info_to_pid(self(), "You don't have permission.")
	    end;
	"ban" ->
	    case eadc_user:access('ban any') of
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
		    eadc_utils:info_to_pid(self(), "You don't have permission.")
	    end;
	"userlist" ->
	    List=eadc_utils:account_all(),
	    case eadc_user:access('view pass') of
		true ->
		    New_List=List;
		false ->
		    New_List=lists:map(fun(A) -> A#account{pass="***"} end, List)
	    end,
	    eadc_utils:info_to_pid(self(), lists:flatten(io_lib:format("Users:\n~p", [New_List])));
	"topic" ->
	    case eadc_user:access('change topic') of
		true ->
		    Topic=string:join(Args, " "),
		    _Ok=eadc_utils:set_option(mainchat, topic, Topic),
		    AllPids=eadc_client_fsm:all_pids(),
		    topic_to_pids(AllPids);		    
		false ->
		    eadc_utils:info_to_pid(self(), "You don't have permission.")
	    end;
	"setconfig" ->
	    case eadc_user:access('set config') of
		true ->
                    case get_fields(Args, 2) of
			[Key | Rest] ->
			    Val=string:join(Rest, " "),
			    _Ok=eadc_utils:set_option(hub, list_to_atom(Key), Val),
			    eadc_utils:info_to_pid(self(), "OK");
			_ -> throw({error, "usage: !setconfig <key> <val>"})
		    end;
		false ->
		    eadc_utils:info_to_pid(self(), "You don't have permission.")
	    end;
	"getconfig" ->
	    case eadc_user:access('get config') of
		true ->
		    AllOptions=eadc_utils:get_options({option, {hub, '_'}, '_'}),
		    Options=lists:map(fun(E) -> {hub, Key}=E#option.id,Val=E#option.val, 
						lists:concat([Key," => '",Val,"'\n"]) end,
				      AllOptions),
		    Out=string:join(Options, " "),
		    eadc_utils:info_to_pid(self(), "Config list:\n "++Out);
		false ->
		    eadc_utils:info_to_pid(self(), "You don't have permission.")
	    end;
        "setfile" ->
            case eadc_user:access('set files') of
                true ->
                    case get_fields(Args, 2) of
			[Key | Rest] ->
			    Val=string:join(Rest, " "),
			    _Ok=eadc_utils:set_option(files, list_to_atom(Key), Val),
			    eadc_utils:info_to_pid(self(), "OK");
			_ -> throw({error, "usage: !setfile <key> <val>"})
		    end;
                false ->
                    eadc_utils:info_to_pid(self(), "You don't have permission.")
            end;
	"plugin" ->
	    case eadc_user:access('plugin manage') of
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
		    eadc_utils:info_to_pid(self(), "Plugin "++atom_to_list(MName)
					   ++" have been "++M++".");
		{error, Error} ->
		    eadc_utils:info_to_pid(self(), lists:flatten(Error))
	    end;
	"pluginlist" ->
	    case eadc_user:access('plugin manage') of
                true ->
		    PL=eadc_plugin:get_plugins(),
		    Out=lists:foldl(fun(Pname, Acc) -> Acc++"\n"++atom_to_list(Pname) end,"",PL),
		    eadc_utils:info_to_pid(self(), Out);
		false ->
		    eadc_utils:info_to_pid(self(), "You don't have permission.")
	    end;
	"redirect" ->
	    case eadc_user:access('redirect') of
                true ->
		    [Nick|_]=Args,
		    [Cl|_]=eadc_user:client_find(#client{nick=Nick, _='_'}),
		    {Pid, Sid}={Cl#client.pid, Cl#client.sid},
		    eadc_utils:redirect_to(Pid, Sid, "dchub://jlarky.punklan.net");
		false ->
		    eadc_utils:info_to_pid(self(), "You don't have permission.")
	    end;
	"redirectsid" ->
	    case eadc_user:access('redirect') of
		true ->
		    [SID_|_]=Args,Sid=eadc_utils:unbase32(SID_),
		    io:format("~p\n", [Sid]),
		    Pid=eadc_client_fsm:get_pid_by_sid(Sid),
		    eadc_utils:redirect_to(Pid, Sid, "dchub://jlarky.punklan.net");
		false ->
		    eadc_utils:info_to_pid(self(), "You don't have permission.")
	    end;
	Command when (Command=="addtorole") or (Command=="delfromrole") ->
            case eadc_user:access('change permission') of
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
	    eadc_utils:info_to_pid(self(), "OK");
	Command when (Command=="addrole") or (Command=="delrole") ->
            case eadc_user:access('change permission') of
                true ->  ok;
                false -> throw({error,"You don't have permission."})
            end,
	    [Role|Login_]=case get_fields(Args,2) of
			     A=[_,_] -> A;
			     _ -> throw({error, "Error: see help for usage"})
			 end,
	    Login=string:join(Login_," "),
	    Acc=eadc_utils:account_get(Login),
	    Roles=case is_record(Acc,account) of
		      true ->
			  Acc#account.roles;
		      _ ->
			  throw({error, "Account not found. Name of account is case-sensative."})
		  end, NewRoles=
		case {Command,lists:member(list_to_atom(Role),Roles)} of
		    {"addrole",false}-> [list_to_atom(Role)|Roles];
		    {"delrole",true} -> lists:delete(list_to_atom(Role), Roles);
		    {"addrole",true} -> throw({error, "Already added"});
		    {"delrole",false}-> throw({error, "Account doesn't have this role"})
		end,
	    eadc_utils:account_write(Acc#account{roles=NewRoles}),
	    eadc_utils:info_to_pid(self(), "OK");
	_ ->
	    eadc_utils:info_to_pid(self(), "Unknown command '"++Command++"'")
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utils
%%%%%

drop(UserName, OPName, Reason) ->
    try
	[User_Client]=eadc_user:client_find(#client{nick=UserName, _='_'}),
	Pid_to_kill=User_Client#client.pid,
	eadc_utils:broadcast({info, UserName++" dropped by "++OPName++". Reason: "++Reason}),
	gen_fsm:send_event(Pid_to_kill, {kill, "Dropped"})
    catch
	error:{badmatch,[]} ->
	    eadc_utils:info_to_pid(self(), "User not found.")
    end.

ban(UserName, OPName, Reason) ->
    ban(UserName, OPName, 5, Reason).

ban(UserName, OPName, Time, Reason) ->
    Client=
	case eadc_user:client_find(#client{nick=UserName, _='_'}) of
	    [] -> throw({error, "User '"++UserName++"' not found."});
	    [Client_]  -> Client_
	end,
    IP=Client#client.addr,
    ban(UserName, OPName, IP, Time, Reason).

ban(UserName, OPName, IP, Time, Reason) when is_integer(Time) ->
    Ban=#ban{nick=UserName, ip=IP, time=eadc_utils:get_unix_timestamp(now())+Time*60,
	     op=OPName, reason=Reason},
    ok=mnesia:dirty_write(Ban),
    eadc_utils:info_to_pid(self(), "User '"++UserName++"' banned").


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

