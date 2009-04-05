
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
	  buf,       % buffer for client messages sended in several tcp pockets
	  addr,      % client address
	  sid,       % client's SID
	  login,     % user login
	  random,    % random string that hub send to user
	  triesleft, % amount of tries before hub kicks user
	  afterverify% contain function that must be executed after entering pass
	 }).

-record(client, {
	  sid, %% SID. Key field
	  pid, %% PID or {m, f, a}
	  cid, %% client's CID
	  nick,
	  login,
	  inf, %% INF string to send to other clients
	  addr %% client address
	  }).

-record(account, {
	  login="",
	  pass="",
	  class=1,
	  nick="",
	  cid="",
	  info=[]}).

-record(option, {
	  id={other, key}, %% {ns, key} NameSpace, useful for plugin or module name
	  val    %% value
	 }).
