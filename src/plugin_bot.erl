-module(plugin_bot).

-export([user_login/1,
	chat_msg/1]).

-include("eadc.hrl").
-include("eadc_plugin.hrl").

user_login(Args) ->
    eadc_utils:send_to_pid(self(), {args, ["ICMD", "Commands\\General\\Help",
					   "TTBMSG\s%[mySID]\s!help\n", "CT1"]}),

    eadc_utils:send_to_pid(self(), {args, ["IINF", "CT32", "VEJLarky's hub", "NIEADC-hub", "DE}{абе"]}),
    eadc_utils:send_to_pid(self(), {args, ["IINF", "ASDF", "I40.0.0.0","CT5", "NItest-root", "DEтестовая комната"]}),
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
		    Ok=eadc_utils:account_new(#account{login=UserName, nick=UserName,pass=Pass}),
		    eadc_utils:info_to_pid(self(), lists:flatten(io_lib:format("~p", [Ok])));
		false ->
		    eadc_utils:info_to_pid(self(), "You don't have permission.")
	    end;
	"regme" ->
	    [Pass|_]=Args,UserName=Client#client.nick,
	    {atomic, ok}=eadc_utils:account_new(#account{login=UserName, nick=UserName,pass=Pass}),
	    eadc_utils:info_to_pid(self(), lists:flatten(io_lib:format("Password of user ~s was set to '~s'", [UserName, Pass])));
	"regclass" ->
	    case eadc_user:access('reg class') of
		true ->
		    [Login, Class|_]=Args,
		    Account=eadc_utils:account_get(Login),
		    {atomic, ok}=eadc_utils:account_new(
				   Account#account{class=list_to_integer(Class)}),
		    eadc_utils:info_to_pid(self(), lists:flatten(io_lib:format("Class of user ~s was set to '~s'", [Login, Class])));
		false ->
		    eadc_utils:info_to_pid(self(), "You don't have permission.")
	    end;
	"userlist" ->
	    {atomic, List}=eadc_utils:account_list(),
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

