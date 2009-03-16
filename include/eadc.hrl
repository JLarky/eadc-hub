
-define(DEBUG(Type, Format, Data), begin
				       case Type of
					   debug ->
					       %%error_logger:info_msg(Format, Data);
					       ok;
					   info ->
					       error_logger:info_msg(Format, Data);
					   error ->
					       error_logger:error_msg(Format, Data);
					   _ ->
					       error_logger:info_msg(Format, Data)
				       end
				   end).

-record(state, {
	  socket,    % client socket
	  addr,      % client address
	  sid,       % client's SID
	  cid,       % client's CID
	  inf,       % INF string to send to other clients
	  buf,       % buffer for client messages sended in several tcp pockets
	  nick,
	  login,     % user login
	  random,    % random string that hub send to user
	  triesleft, % amount of tries before hub kicks user
	  afterverify % contain function that must be executed after entering pass
	 }).

-record(client, {
	  sid, %% SID
	  pid, %% PID
	  nick,
	  cid,
	  login
	  }).

-record(account, {
	  login="",
	  pass="",
	  class=1,
	  nick="",
	  cid="",
	  info=[]}).
