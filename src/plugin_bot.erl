-module(plugin_bot).

-export([user_login/1,
	chat_msg/1]).

-include("eadc.hrl").
-include("eadc_plugin.hrl").

user_login(Args) ->
    {value,{pid,Pid}} = lists:keysearch(pid, 1, Args),
    {string, String1} = eadc_utils:convert({args, ["IINF", "CT32", "VEJLarky's hub", "NIADC-hub", "DEБот"]}),
    {string, String2} = eadc_utils:convert({args, ["ISTA", "000", "Добро пожаловать в ADC-хаб написанный на Erlang. Страничка проекта http://wiki.github.com/JLarky/eadc-hub"]}),
    gen_fsm:send_event(Pid, {send_to_socket, String1}),
    gen_fsm:send_event(Pid, {send_to_socket, String2}),
    Args.

chat_msg(Args) ->
    ?DEBUG(debug, "chat_msg: ~w~n", [Args]),
    ?GET_VAL(msg, Msg),
    case Msg of 
	[$!|Command] -> %% command
	    do_command(Command),
	    lists:keyreplace(pids, 1, Args, {pids, []});
	_ -> 
	    Args
    end.

do_command(Command) ->
    case Command of
	"help" ->
	    Hlp="All hub commands:
!help - for show this help",
	    eadc_utils:info_to_pid(self(), Hlp);
	_ ->
	    eadc_utils:info_to_pid(self(), "неопознаная комманда =)"),
	    Out=Command,
	    Test=lists:flatten(io_lib:format("~w", [Out])),
	    eadc_utils:error_to_pid(self(), Test)
    end.

