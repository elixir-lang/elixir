-module(elixir_sup).
-behaviour(supervisor).
-export([init/1,start_link/1]).

start_link(Args) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(Args) ->
  Supervisors = [
    {
      elixir_code_server_sup,
      { elixir_code_server, start_link, Args },

      permanent,                    % Restart  = permanent | transient | temporary
      2000,                         % Shutdown = brutal_kill | int() >= 0 | infinity
      worker,                       % Type     = worker | supervisor
      [elixir_code_server]          % Modules  = [Module] | dynamic
    }
  ],

  {ok, {{one_for_one, 3, 10}, Supervisors}}.