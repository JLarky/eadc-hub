{application, tiger,
 [
  {description, "Tiger Hash written in Erlang"},
  {vsn, "0.1"},
  {id, "tiger"},
  {modules,      [tiger]},
  {registered,   [tiger]},
  {applications, [kernel, stdlib]},
  %%
  %% mod: Specify the module name to start the application, plus args
  %%
  {mod, {tiger, []}},
  {env, []}
 ]
}.

