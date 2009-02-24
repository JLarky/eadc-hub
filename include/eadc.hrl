
-define(DEBUG(Type, Format, Data), begin
				       io:format("\npid ~w:", [self()]),
				       case Type of
					   debug ->
					       error_logger:info_msg(Format, Data);
					   info ->
					       error_logger:info_msg(Format, Data);
					   error ->
					       error_logger:error_msg(Format, Data);
					   _ ->
					       error_logger:info_msg(Format, Data)
				       end
				   end).

-record(client, {
	  sid, %% SID
	  pid  %% PID
	  }).
