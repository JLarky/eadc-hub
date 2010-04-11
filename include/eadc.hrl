
-define(DEBUG(Type, Format, Data), eadc_utils:debug(Type, Format, Data)).

-define(ERROR(Format, Data), error_logger:error_msg(Format, Data)).

-record(connect,{
	  sender,      % sender record
	  buff=[],     % buffer for unfinished commands
	  statename,   % ADC state name
	  adcsid,      % ADC SID
	  pre_client
	  }).

-record(state, {
	  states,
	  socket,    % client socket
	  buf,       % buffer for client messages sended in several tcp pockets
	  addr,      % client address
	  sid,       % client's SID
	  other=[],  % for store additional data
	  login,     % user login
	  random,    % random string that hub send to user
	  triesleft, % amount of tries before hub kicks user
	  afterverify% contain function that must be executed after entering pass
	 }).

-record(client, {
	  sid, %% SID. Key field                      %%ADC
	  pid, %% controlling plugin's name
	  cid, %% client's CID                        %%ADC
	  sender, %% {pid,socket} to send messages
	  nick,                                       %%ADC
	  login,
	  inf, %% INF string to send to other clients %%ADC
	  sup=[], %% SUP list of supported features
	  addr, %% client address
	  other
	  }).

-record(account, {
	  login="",
	  pass="",
	  nick="",
	  cid="",
	  roles=[anonymous],
	  info=[]}).

-record(option, {
	  id={other, key}, %% {ns, key} NameSpace, useful for plugin or module name
	  val    %% value
	 }).

-record(role, {role, desc}).
-record(permission, {permission, roles}).
-record(ban, {nick, ip, time, op, reason}).
-record(sender, {socket,pid}).
