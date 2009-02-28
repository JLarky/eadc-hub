-module(plugin_bot).

-export([user_login/1,
	chat_msg/1]).

-include("eadc.hrl").

user_login(Args) ->
    {value,{pid,Pid}} = lists:keysearch(pid, 1, Args),
    {string, String1} = eadc_utils:convert({args, ["IINF", "CT32", "VEJLarky's hub", "NIADC-hub", "DEБот"]}),
    {string, String2} = eadc_utils:convert({args, ["ISTA", "000", "Добро пожаловать в ADC-хаб написанный на Erlang"]}),
    gen_fsm:send_event(Pid, {send_to_socket, String1}),
    gen_fsm:send_event(Pid, {send_to_socket, String2}).

chat_msg(Args) ->
    ?DEBUG(debug, "", Args),
    %%{value,{pid,Pid}} = lists:keysearch(pid, 1, Args),
    %%{string, String1} = eadc_utils:convert({args, ["BMSG", "AAAA", "echo"]}),
    %%gen_fsm:send_event(Pid, {send_to_socket, String1}).
    ok.
