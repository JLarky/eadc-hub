{application, eadc,
 [
  {description, "ADC-HUB writen in Erlang"},
  {vsn, "0.4"},
  {id, "eadc"},
  {modules,      [eadc_app, eadc_user, eadc_listener, eadc_client, eadc_utils,
  eadc_plugin, eadc_master, utf8, plugin_bot]},
  {registered,   [tcp_server_sup, tcp_listener]},
  {applications, [kernel, stdlib,tiger]},
  %%
  %% mod: Specify the module name to start the application, plus args
  %%
  {mod, {eadc_app, []}},
  {env, []}
 ]
}.
