{application, eadc,
 [
  {description, "ADC-HUB writen in Erlang"},
  {vsn, "0.4"},
  {id, "eadc"},
  {modules,      [eadc_app, eadc_client, eadc_connect_state, eadc_listener,
  eadc_master, eadc_plugin, eadc_user, eadc_utils, sockroute,
  plugin_bot, plugin_punklan, plugin_ten_lines]},
  {registered,   [tcp_server_sup, tcp_listener]},
  {applications, [kernel, stdlib,tiger]},
  %%
  %% mod: Specify the module name to start the application, plus args
  %%
  {mod, {eadc_app, []}},
  {env, []}
 ]
}.
