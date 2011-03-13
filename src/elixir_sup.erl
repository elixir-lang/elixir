-module(elixir_sup).

-behaviour(supervisor).

-export([init/1,start_link/0]).

-export([start_file/2]).

%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc A startup function for whole supervisor. Started by application
%% @end 
%%--------------------------------------------------------------------
-spec start_link() -> {'error',_} | {'ok',pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


start_file(URL, Options) ->
  supervisor:start_child(http_one_file_sup, [URL, Options]).

init([http_file]) ->
    {ok,
        {{simple_one_for_one, 3, 10},
            [
              % MediaEntry
              {   undefined,                               % Id       = internal id
                  {http_file,start_link,[]},             % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [http_file]                            % Modules  = [Module] | dynamic
              }
            ]
        }
    };


init([]) ->
  Supervisors = [
    {   elixir_code_server_sup,
        {elixir,boot,[]},
        permanent,                               % Restart  = permanent | transient | temporary
        2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
        worker,                                  % Type     = worker | supervisor
        [elixir]                      % Modules  = [Module] | dynamic
    }
  ],
  
  {ok, {{one_for_one, 3, 10}, Supervisors}}.
