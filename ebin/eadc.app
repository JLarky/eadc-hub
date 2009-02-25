{application, eadc,
 [
  {description, "ADC-HUB writing in erlang"},
  {vsn, "0.1"},
  {id, "eadc"},
  {modules,      [eadc_listener, eadc_client_fsm, eadc_utils]},
  {registered,   [tcp_server_sup, tcp_listener]},
  {applications, [kernel, stdlib]},
  %%
  %% mod: Specify the module name to start the application, plus args
  %%
  {mod, {eadc_app, []}},
  {env, []}
 ]
}.

