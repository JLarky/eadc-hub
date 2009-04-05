{application, eadc,
 [
  {description, "ADC-HUB writen in Erlang"},
  {vsn, "0.4"},
  {id, "eadc"},
  {modules,      [eadc_listener, eadc_client_fsm, eadc_utils, eadc_plugin]},
  {registered,   [tcp_server_sup, tcp_listener]},
  {applications, [kernel, stdlib]},
  %%
  %% mod: Specify the module name to start the application, plus args
  %%
  {mod, {eadc_app, []}},
  {env, []}
 ]
}.
