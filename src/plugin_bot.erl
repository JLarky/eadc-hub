-module(plugin_bot).

-export([user_login/1,
	chat_msg/1]).

-include("eadc.hrl").
-include("eadc_plugin.hrl").

user_login(Args) ->
    eadc_utils:send_to_pid(self(), {args, ["ICMD", "Commands\\General\\Help",
					   "TTBMSG\s%[mySID]\s!help\n", "CT1"]}),

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
	    do_command(Params, State),
	    Args1=lists:keyreplace(msg, 1, Args, {msg, []}),
	    lists:keyreplace(pids, 1, Args1, {pids, []});
	_ -> 
	    Args
    end.

do_command([Command|Args], State) ->
    case Command of
	"help" ->
	    Hlp="All hub commands:
!help - show this help
!regme <password> - register new user with password <password>'
!regclass <user> <class> - change users's class to <class>

For now all commands not require any privileges. Enjoy.",
	    eadc_utils:info_to_pid(self(), Hlp);
	"regnewuser" ->
	    [UserName,Pass|_]=Args,
	    Ok=eadc_utils:account_new(#account{login=UserName, nick=UserName,pass=Pass}),
	    eadc_utils:info_to_pid(self(), lists:flatten(io_lib:format("~p", [Ok])));
	"regme" ->
	    [Pass|_]=Args,UserName=State#state.nick,
	    {atomic, ok}=eadc_utils:account_new(#account{login=UserName, nick=UserName,pass=Pass}),
	    eadc_utils:info_to_pid(self(), lists:flatten(io_lib:format("Password of user ~s was set to '~s'", [UserName, Pass])));
	"regclass" ->
	    [Login, Class|_]=Args,
	    Account=eadc_utils:account_get(Login),
	    {atomic, ok}=eadc_utils:account_new(Account#account{class=list_to_integer(Class)}),
	    eadc_utils:info_to_pid(self(), lists:flatten(io_lib:format("Class of user ~s was set to '~s'", [Login, Class])));
	"userlist" ->
	    List=eadc_utils:account_list(),
	    eadc_utils:info_to_pid(self(), lists:flatten(io_lib:format("~p", [List])));
	_ ->
	    io:format("~s", [Command]),
	    eadc_utils:info_to_pid(self(), "Unknown command"),
	    Out=Command,
	    Test=lists:flatten(io_lib:format("~w", [Out])),
	    eadc_utils:error_to_pid(self(), Test)
    end.

