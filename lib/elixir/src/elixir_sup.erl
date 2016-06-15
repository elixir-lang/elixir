-module(elixir_sup).
-behaviour(supervisor).
-export([init/1, start_link/0]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, ok).

init(ok) ->
  Workers = [
    {
      elixir_config,
      {elixir_config, start_link, []},

      permanent,                    % Restart  = permanent | transient | temporary
      2000,                         % Shutdown = brutal_kill | int() >= 0 | infinity
      worker,                       % Type     = worker | supervisor
      [elixir_config]               % Modules  = [Module] | dynamic
    },

    {
      elixir_code_server,
      {elixir_code_server, start_link, []},

      permanent,                    % Restart  = permanent | transient | temporary
      2000,                         % Shutdown = brutal_kill | int() >= 0 | infinity
      worker,                       % Type     = worker | supervisor
      [elixir_code_server]          % Modules  = [Module] | dynamic
    }
  ],

  {ok, {{one_for_one, 3, 10}, Workers}}.
