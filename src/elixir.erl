-module(elixir).
-export([start/0, require/1, eval/1, eval/2, eval/3, eval/4, eval/5, parse/2, parse/3]).
-include("elixir.hrl").
-include_lib("kernel/include/file.hrl").

% OTP Application API
-export([start_app/0, start/2, stop/1, config_change/3]).

start(_Type, _Args) ->
  % Ensure elixir_object_methods is loaded and running
  code:ensure_loaded(elixir_object_methods),

  % Load stdlib files
  BasePath = stdlib_path(),
  BaseFiles = [internal_require(filename:join(BasePath, File)) || File <- stdlib_files()],

  % Boot the code server with supervisor
  elixir_sup:start_link([BasePath, BaseFiles]).

stop(_S) ->
  ok.

config_change(_Changed, _New, _Remove) ->
  ok.

% Start elixir as an application.
start_app() ->
  case lists:keyfind(?MODULE,1, application:loaded_applications()) of
    false -> application:start(?MODULE);
    _ -> ok
  end.

% Boot and process given options. Invoked by Elixir's script.
start() ->
  start_app(),
  CodeInit = elixir_constants:lookup('Code::Init'),
  'exCode::Init':process_argv(CodeInit, init:get_plain_arguments()).

% Return the full path for the Elixir installation.
stdlib_path() ->
  case os:getenv("ELIXIR_PATH") of
    false ->
      case code:lib_dir(elixir,lib) of
        {error, bad_name} ->
          Dirname = filename:dirname(?FILE),
          filename:join([Dirname, "..", "lib"]);
        Path when is_list(Path) ->
          Path
      end;
    Path -> filename:join([Path, "lib"])
  end.

% Stblib files that are loaded and compiled.
stdlib_files() ->
  [
    "object.ex",
    "module.ex",
    "io.ex",
    "atom.ex",
    "list.ex",
    "numeric.ex",
    "integer.ex",
    "float.ex",
    "tuple.ex",
    "string.ex",
    "ordered_dict.ex",
    "regexp.ex",
    "bit_string.ex",
    "process.ex",
    "port.ex",
    "reference.ex",
    "function.ex",
    "gen_server.ex",
    "record.ex",
    "file.ex",
    "code.ex",
    "code/formatter.ex",
    "code/init.ex",
    "code/server.ex"
  ].

internal_require(Filepath) ->
  require(Filepath, [{self,[]}]),
  Filepath.

require(Filepath) ->
  require(Filepath, []).

require(Filepath, Binding) ->
  List = read_file(Filepath),
  eval(List, Binding, Filepath).

% Read a file as utf8
read_file(FileName) ->
  {ok, Device} = file:open(FileName, [read, {encoding, utf8}]),
  read_file(Device, []).

read_file(Device, Acc) ->
  case io:get_line(Device, "") of
    eof  ->
      file:close(Device),
      lists:append(lists:reverse(Acc));
    Line ->
      read_file(Device, [Line|Acc])
  end.

% Evaluates a string
eval(String) -> eval(String, []).
eval(String, Binding) -> eval(String, Binding, "nofile").
eval(String, Binding, Filename) -> eval(String, Binding, Filename, 1).
eval(String, Binding, Filename, Line) -> eval(String, Binding, Filename, Line, #elixir_scope{}).
eval(String, Binding, Filename, Line, Scope) ->
  SelfBinding = case proplists:get_value(self, Binding) of
    undefined -> lists:append(Binding, [{self,elixir_constants:lookup('Object')}]);
    _  -> Binding
  end,
  { ParseTree, NewScope } = parse(String, SelfBinding, Filename, Line, Scope),
  {value, Value, NewBinding} = erl_eval:exprs(ParseTree, SelfBinding),
  {Value, final_binding(NewBinding, NewScope#elixir_scope.vars) }.

% Parse string and transform tree to Erlang Abstract Form format
parse(String, Binding) -> parse(String, Binding, "nofile").
parse(String, Binding, Filename) -> parse(String, Binding, Filename, 1, #elixir_scope{}).
parse(String, Binding, Filename, Line, Scope) ->
  NewScope = Scope#elixir_scope{vars=binding_dict(Binding), filename=Filename},
  elixir_transform:parse(String, Line, NewScope).

binding_dict(List) -> binding_dict(List, dict:new()).
binding_dict([{self,_}|T], Dict) -> binding_dict(T, Dict);
binding_dict([{H,_}|T], Dict) -> binding_dict(T, dict:store(H, H, Dict));
binding_dict([], Dict) -> Dict.

final_binding(Binding, Vars) -> final_binding(Binding, [], Binding, Vars).
final_binding([{self,_}|T], Acc, Binding, Vars) -> final_binding(T, Acc, Binding, Vars);
final_binding([{Var,_}|T], Acc, Binding, Vars) ->
  case atom_to_list(Var) of
    [$X|_] -> final_binding(T, Acc, Binding, Vars);
    _ ->
      RealName = dict:fetch(Var, Vars),
      RealValue = proplists:get_value(RealName, Binding, nil),
      final_binding(T, [{Var, RealValue}|Acc], Binding, Vars)
  end;

final_binding([], Acc, _Binding, _Vars) -> lists:reverse(Acc).
