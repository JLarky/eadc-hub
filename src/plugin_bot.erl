-module(plugin_bot).

-export([user_login/1]).

-include("eadc.hrl").

user_login(Args) ->
    {value,{pid,Pid}} = lists:keysearch(pid, 1, Args),
    {string, String1} = eadc_utils:convert({list, ["IINF", "CT32", "VEJLarky's hub", "NIADC-hub", "DEБот\n"]}),
    {string, String2} = eadc_utils:convert({list, ["ISTA", "000", "Добро пожаловать в ADC-хаб написанный на Erlang\n"]}),
    gen_fsm:send_event(Pid, {send_to_socket, String1}),
    gen_fsm:send_event(Pid, {send_to_socket, String2}).
