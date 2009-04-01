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
    Cid=eadc_client_fsm:get_unical_cid(eadc_utils:random_base32(39)),
    Sid=eadc_client_fsm:get_unical_SID(),
    Nick="test-room",
    Inf="BINF "++Sid++" CT5"++" ID"++Cid++" NI"++Nick++" DEтестовая\\sкомната",
    eadc_client_fsm:client_write(#client{cid=Cid, sid=list_to_atom(Sid), nick=Nick, inf=Inf, pid=undefined}),
    Args.

topic_to_pids(Pids) ->
    Topic=eadc_utils:get_option(mainchat, topic, "No topic set"),
    HubName=eadc_utils:get_option(hub, name, "EADC. ADC hub written in Erlang"),
    F=fun(P) -> eadc_utils:send_to_pid(P, {args, ["IINF", "CT32", "VEJLarky's hub", "NI"++HubName, "DE"++Topic]}) end,
    lists:foreach(F, Pids).

user_login(Args) ->
    eadc_utils:send_to_pid(self(), {args, ["ICMD", "Commands\\General\\Help",
					   "TTBMSG\s%[mySID]\s!help\n", "CT1"]}),

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
	    lists:keyreplace(data, 1, Args, {data, utf8:to_utf8(lists:sublist(utf8:from_utf8(Data), 1024))})
	    %%lists:keyreplace(msg, 1, Args, {msg, utf8:to_utf8(lists:sublist(utf8:from_utf8(Msg), 1024))})
	    %%lists:keyreplace(data, 1, Args, {data, utf8:to_utf8(lists:sublist(utf8:from_utf8(""), 1024))})
    
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
	    {atomic, ok}=eadc_utils:account_write(#account{login=UserName, nick=UserName,pass=Pass}),
	    eadc_utils:info_to_pid(self(), lists:flatten(io_lib:format("Password of user ~s was set to '~s'", [UserName, Pass])));
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
	"regclass" ->
	    case eadc_user:access('reg class') of
		true ->
		    [Login, Class|_]=Args,
		    Account=eadc_utils:account_get(Login),
		    {atomic, ok}=eadc_utils:account_write(
				   Account#account{class=list_to_integer(Class)}),
		    eadc_utils:info_to_pid(self(), lists:flatten(io_lib:format("Class of user ~s was set to '~s'", [Login, Class])));
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
				    eadc_plugin:set_plugins([MName|PL]);
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
