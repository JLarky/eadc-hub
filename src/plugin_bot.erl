-module(plugin_bot).

-export([user_login/1,
	 chat_msg/1
	 %%init/1,
	 %%priv_msg/1
	]).

-include("eadc.hrl").
-include("eadc_plugin.hrl").

-export([init/0,terminate/0]).

init() ->
     ok.
terminate() ->
     {error, "This plugin can't be stopped"}.

init(Args) ->
    Cid=eadc_client_fsm:get_unical_cid(),
    Sid=eadc_client_fsm:get_unical_SID(),
    Nick="test-room",
    Inf="BINF "++eadc_utils:sid_to_s(Sid)++" CT5"++" ID"++Cid++" NI"++Nick++" DEтестовая\\sкомната",
    eadc_client_fsm:client_write(#client{cid=Cid, sid=Sid, nick=Nick, inf=Inf, pid=undefined}),
    Args.

topic_to_pids(Pids) ->
    Topic=eadc_utils:get_option(mainchat, topic, "No topic set"),
    HubName=eadc_utils:get_option(hub, name, "EADC. ADC hub written in Erlang"),
    F=fun(P) -> eadc_utils:send_to_pid(P, {args, ["IINF", "CT32", "VEJLarky's hub", "NI"++HubName, "DE"++Topic]}) end,
    lists:foreach(F, Pids).

user_login(Args) ->
    eadc_utils:send_to_pid(self(), {args, ["ICMD", "Commands\\General\\Help",
					   "TTBMSG\s%[mySID]\s!help\n", "CT1"]}),
    eadc_utils:send_to_pid(self(), {args, ["ICMD", "Admin\\redirect",
					   "TTBMSG\s%[mySID]\s!redirectsid\\s%[userSID]\n", "CT2"]}),

    %%eadc_utils:send_to_pid(self(), {args, ["BINF", Bit_sid, "CT5", "ID"++Bot_id, "NItest-room", "DEтестовая комната"]}),
    topic_to_pids([self()]),
    eadc_utils:info_to_pid(self(), "Добро пожаловать в ADC-хаб написанный на Erlang. Страничка проекта http://wiki.github.com/JLarky/eadc-hub на ней можно узнать что такое ADC и почему именно Erlang."),
    Args.

chat_msg(Args) ->
    ?DEBUG(debug, "chat_msg: ~w~n", [Args]),
    ?GET_VAL(msg, Msg),
    ?GET_VAL(data, Data),
    case Msg of 
	[$!|Command] -> %% command
	    {ok, Params}=regexp:split(Command, " "),
	    State=eadc_utils:get_val(state, Args),
	    Client=eadc_utils:get_val(client, Args),
	    do_command(Params, State, Client),
	    Args1=lists:keyreplace(msg, 1, Args, {msg, []}),
	    lists:keyreplace(pids, 1, Args1, {pids, []});
	_ -> 
	    lists:keyreplace(data, 1, Args, {data, lists:sublist(Data, 512)})
    end.

do_command([Command|Args], State, Client) ->
    case Command of
	"help" ->
	    Hlp="All hub commands:
User's commands:
 !help - shows this help
 !regme <password> - registers new user with password <password>
 !userlist - shows all users with hidden passwords

Admin's commands:
 !regclass <user> <class> - changes user's class to <class>
 !userlist - shows all users with their passwords
 !topic <topic> - sets hub's topic
 !getconfig - shows all set options
 !setconfig <key> <val> - sets hub's option 

Plugins:
 !plugin on <plugin> - turns on plugin and adds it to autostart
 !plugin off <plugin> - turns off plugin and removes it from autostart
 !pluginlist - shows all running plugins

Roles:
 !addtorole <role> <permission> - addes permission to role
 !delfromrole <role> <permission> - deletes permission from role
 !addrole <role> <login> - addes role to login
 !delrole <role> <login> - deletes role from login
",
	    eadc_utils:info_to_pid(self(), Hlp);
	"regnewuser" ->
	    case eadc_user:access('reg user') of
		true ->
		    [UserName,Pass|_]=Args,
		    Ok=eadc_utils:account_write(#account{login=UserName, nick=UserName,pass=Pass}),
		    eadc_utils:info_to_pid(self(), lists:flatten(io_lib:format("~p", [Ok])));
		false ->
		    eadc_utils:info_to_pid(self(), "You don't have permission.")
	    end;
	"regme" ->
	    [Pass|_]=Args,UserName=Client#client.nick,
	    case mnesia:table_info(account, size) of
		0 -> %% first user
		    eadc_utils:info_to_pid(self(), "Register superuser."),
		    Roles=[root];
		_ ->
		    Roles=[]
	    end,
	    case eadc_user:access('self registration') or (Roles==[root]) of
		true ->
		    {atomic, ok}=eadc_utils:account_write(#account{login=UserName, nick=UserName,
								   pass=Pass,roles=Roles}),
		    eadc_utils:info_to_pid(self(), "User '"++UserName++"' was registered");
		false ->
		    eadc_utils:error_to_pid(self(), "You don't have permission.")
	    end;
	"kick" ->
	    case eadc_user:access('kick any') of
		true ->
		    try
			[User|_]=Args,UserName=Client#client.nick,
			[User_Client]=eadc_user:client_find(#client{nick=User, _='_'}),
			Pid_to_kill=User_Client#client.pid,
			eadc_utils:broadcast(fun(Pid) -> eadc_utils:info_to_pid(Pid, "OP "++UserName++" is trying to kick '"++User++"'") end),
			timer:sleep(100),
			gen_fsm:send_event(Pid_to_kill, kill_your_self)			
		    catch
			error:{badmatch,[]} ->
			    eadc_utils:info_to_pid(self(), "User not found.")
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
		    Ok=eadc_utils:set_option(mainchat, topic, Topic),
		    AllPids=eadc_client_fsm:all_pids(),
		    topic_to_pids(AllPids);		    
		false ->
		    eadc_utils:info_to_pid(self(), "You don't have permission.")
	    end;
	"setconfig" ->
	    case eadc_user:access('set config') of
		true ->
		    [Key | Rest] = Args,
		    Val=string:join(Rest, " "),
		    Ok=eadc_utils:set_option(hub, list_to_atom(Key), Val);
		false ->
		    eadc_utils:info_to_pid(self(), "You don't have permission.")
	    end;
	"getconfig" ->
	    case eadc_user:access('get config') of
		true ->
		    AllOptions=eadc_utils:get_options({option, {hub, '_'}, '_'}),
		    Options=lists:map(fun(E) -> {hub, Key}=E#option.id, Val=E#option.val, atom_to_list(Key)++" => '"++Val++"'\n" end, AllOptions),
		    Out=string:join(Options, " "),
		    eadc_utils:info_to_pid(self(), "Config list:\n"++Out);
		false ->
		    eadc_utils:info_to_pid(self(), "You don't have permission.")
	    end;
	"plugin" ->
	    case eadc_user:access('plugin manage') of
                true ->
		    {MName,F,M}=case Args of
				    ["on", Name|_] ->
					{list_to_atom(Name),init, "initialized."};
				    ["off", Name|_] ->
					{list_to_atom(Name),terminate, "terminated"};
				    _ ->
					eadc_utils:info_to_pid(self(), "Wrong parametrs."),
					{"","",""}
				end,
		    case (catch MName:F()) of
			{'EXIT',{undef,[{MName,F,[]}|_]}} ->
			    eadc_utils:error_to_pid(self(), "Plugin "++atom_to_list(MName)
						    ++" can't be "++M++".");
			ok ->
			    PL=eadc_plugin:get_plugins(),
			    case F of
				init ->
				    eadc_plugin:set_plugins(PL++[MName]);
				terminate ->
				    eadc_plugin:set_plugins(lists:delete(MName,PL))
			    end,
			    eadc_utils:info_to_pid(self(), "Plugin "++atom_to_list(MName)
						   ++" have been "++M++".");
			{error, Error} ->
			    eadc_utils:info_to_pid(self(), lists:flatten(Error))
		    end;
		false ->
		    eadc_utils:info_to_pid(self(), "You don't have permission.")
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
	    [Nick|_]=Args,
	    [Cl|_]=eadc_user:client_find(#client{nick=Nick, _='_'}),
	    {Pid, Sid}={Cl#client.pid, Cl#client.sid},
	    eadc_utils:redirect_to(Pid, Sid, "dchub://jlarky.punklan.net"),
	    eadc_utils:info_to_pid(self(), lists:flatten(io_lib:format("~w", [{Pid, Sid}])));
	"redirectsid" ->
	    [SID_|_]=Args,Sid=eadc_utils:unbase32(SID_),
	    io:format("~p\n", [Sid]),
	    Pid=eadc_client_fsm:get_pid_by_sid(Sid),
	    eadc_utils:redirect_to(Pid, Sid, "dchub://jlarky.punklan.net"),
	    eadc_utils:info_to_pid(self(), lists:flatten(io_lib:format("~w", [{Pid, Sid}])));
	Role_ when (Role_=="addtorole") or (Role_=="delfromrole") ->
	    [Role|Perm]=Args,Permission=string:join(Perm, " "),
	    case mnesia:dirty_read(permission, list_to_atom(Permission)) of
		[Perms] ->
		    Roles=Perms#permission.roles;
		[] ->
		    Roles=[]
	    end,
	    NewRoles=case Role_ of
			        "addtorole" ->
			     [list_to_atom(Role)|Roles];
			 "delfromrole" ->
			     lists:delete(list_to_atom(Role), Roles)
				          end,
	    mnesia:dirty_write(#permission{permission=list_to_atom(Permission),roles=NewRoles}),
	    ok;
	Role_ when (Role_=="addrole") or (Role_=="delrole") ->
	    [Role|Log]=Args,Login=string:join(Log, " "),
	    Acc=eadc_utils:account_get(Login),
	    Roles=Acc#account.roles,
	    NewRoles=case Role_ of
			 "addrole" ->
			     [list_to_atom(Role)|Roles];
			 "delrole" ->
			     lists:delete(list_to_atom(Role), Roles)
		     end,
	    eadc_utils:account_write(Acc#account{roles=NewRoles});
	_ ->
	    io:format("~s", [Command]),
	    eadc_utils:info_to_pid(self(), "Unknown command"),
	    Out=Command,
	    Test=lists:flatten(io_lib:format("~w", [Out])),
	    eadc_utils:error_to_pid(self(), Test)
    end.

priv_msg(Args) ->
    {list, [_EMSG, From_Sid, _To_Sid, Mesg, _PMFJKJ]}=eadc_utils:convert({string, eadc_utils:get_val(data, Args)}),

    Pid=list_to_pid("<0.119.0>"),
    A=eadc_utils:set_val(pids, eadc_client_fsm:all_pids(), Args),
    eadc_utils:set_val(data, "EMSG "++From_Sid++" KVSP "++Mesg++" PMKVSP", A).
