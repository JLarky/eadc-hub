%%%% Helping MACRO for use in plugins

-define(GET_VAL(Key, Val), {value,{Key,Val}} = lists:keysearch(Key, 1, Args)).
-define(SEND_TO_NODES(Msg), lists:foreach(fun(Node) ->
						  {eadc_master, Node} ! {self(), Msg}
					  end, nodes())).
