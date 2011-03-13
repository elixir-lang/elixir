-module(elixir_sup).

-behaviour(supervisor).

-export([init/1,start_link/0]).

%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc A startup function for whole supervisor. Started by application
%% @end 
%%--------------------------------------------------------------------
-spec start_link() -> {'error',_} | {'ok',pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).



init([]) ->
  Supervisors = [
    {   elixir_code_server_sup,
        {elixir,start_link,[]},
        permanent,                               % Restart  = permanent | transient | temporary
        2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
        worker,                                  % Type     = worker | supervisor
        [elixir]                      % Modules  = [Module] | dynamic
    }
  ],
  
  {ok, {{one_for_one, 3, 10}, Supervisors}}.
