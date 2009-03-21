-module(plugin_bot).

-export([user_login/1,
	 chat_msg/1
	 %%init/1,
	 %%priv_msg/1
	]).

-include("eadc.hrl").
-include("eadc_plugin.hrl").

init(Args) ->
    Cid=eadc_client_fsm:get_unical_cid(eadc_utils:random_base32(39)),
    Sid=eadc_client_fsm:get_unical_SID(),
    Nick="test-room",
    Inf="BINF "++Sid++" CT5"++" ID"++Cid++" NI"++Nick++" DEтестовая\\sкомната",
    eadc_client_fsm:client_write(#client{cid=Cid, sid=list_to_atom(Sid), nick=Nick, inf=Inf, pid=undefined}),
    Args.


user_login(Args) ->
    eadc_utils:send_to_pid(self(), {args, ["ICMD", "Commands\\General\\Help",
					   "TTBMSG\s%[mySID]\s!help\n", "CT1"]}),

	%%eadc_utils:send_to_pid(self(), {args, ["BINF", Bit_sid, "CT5", "ID"++Bot_id, "NItest-room", "DEтестовая комната"]}),
    eadc_utils:send_to_pid(self(), {args, ["IINF", "CT32", "VEJLarky's hub", "NIEADC-hub", "DE}{абе"]}),
    eadc_utils:info_to_pid(self(), "Добро пожаловать в ADC-хаб написанный на Erlang. Страничка проекта http://wiki.github.com/JLarky/eadc-hub на ней можно узнать что такое ADC и почему именно Erlang."),
    Args.

chat_msg(Args) ->
    ?DEBUG(debug, "chat_msg: ~w~n", [Args]),
    ?GET_VAL(msg, Msg),
    case Msg of 
	[$!|Command] -> %% command
	    {ok, Params}=regexp:split(Command, " "),
	    State=eadc_utils:get_val(state, Args),
	    Client=eadc_utils:get_val(client, Args),
	    do_command(Params, State, Client),
	    Args1=lists:keyreplace(msg, 1, Args, {msg, []}),
	    lists:keyreplace(pids, 1, Args1, {pids, []});
	_ -> 
	    Args
    end.

do_command([Command|Args], State, Client) ->
    case Command of
	"help" ->
	    Hlp="All hub commands:
User's commands:
 !help - show this help
 !regme <password> - register new user with password <password>'
 !userlist - show all users with hidden passwords

Admin's commands:
 !regclass <user> <class> - change users's class to <class>
 !userlist - show all users with their's passwords
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
			?DEBUG(error, "!!! ~w", []),
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
