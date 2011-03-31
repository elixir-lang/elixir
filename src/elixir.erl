-module(elixir).
-export([start/0, require/2, eval/1, eval/2, eval/3, eval/4, eval/5, parse/2, parse/3]).
-include("elixir.hrl").
-include_lib("kernel/include/file.hrl").

% OTP Application API
-export([start_app/0, start/2, stop/1, config_change/3]).

start(_Type, _Args) ->
  % Ensure elixir_object_methods is loaded and running
  code:ensure_loaded(elixir_object_methods),

  % Load stdlib files
  Regexp = cache_regexp(),
  BasePath = stdlib_path(),
  BaseFiles = [internal_require(filename:join(BasePath, File), Regexp) || File <- stdlib_files()],

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
  'Code::Init':process_argv(CodeInit, init:get_plain_arguments()).

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
    "set.ex",
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

recache() -> os:getenv("ELIXIR_RECACHE") == "1".

% Load core elixir objects.
% Note we pass the self binding as nil when loading object because
% the default binding is Object, which at this point is not defined.
cache_regexp() ->
  { ok, Regexp } = re:compile("^%\s*elixir:\s*cache\s*$"),
  Regexp.

internal_require(Filepath, Regexp) ->
  { ok, Source } = file:read_file_info(Filepath),
  require(Filepath, Source, [{self,[]}], Regexp, recache()),
  Filepath.

check_compiled() ->
  case get(elixir_compile_core) of
    undefined -> [];
    _ ->
      io:format("ERROR: Using cache directive in a file that requires other files, exiting ...\n"),
      exit(badrequire)
  end.

require(Filepath, Source) ->
  require(Filepath, Source, [], cache_regexp(), recache()).

% Require a file by attempting to read its cached bytecode
% or loading it manually.
require(Filepath, Source, Binding, Regexp, true) ->
  check_compiled(),
  Compiled = filename:rootname(Filepath, ".ex") ++ ".exb",
  load(Filepath, Compiled, Binding, Regexp);

require(Filepath, Source, Binding, Regexp, _) ->
  check_compiled(),
  Compiled = filename:rootname(Filepath, ".ex") ++ ".exb",
  case file:read_file_info(Compiled) of
    { ok, Info } ->
      BinTime = Info#file_info.mtime,
      SourceTime = Source#file_info.mtime,
      case BinTime > SourceTime of
        true ->
          { ok, Exb } = file:read_file(Compiled),
          Terms = binary_to_term(Exb),
          lists:foreach(fun({M,F,B}) -> code:load_binary(M, F, B) end, Terms);
        false ->
          load(Filepath, Compiled, Binding, Regexp)
      end;
    _ -> load(Filepath, Compiled, Binding, Regexp)
  end.

% If we got here, it means we could not load the cached one.
% Load the file and create a new cache if the directive says so.
load(Filepath, Compiled, Binding, Regexp) ->
  { First, List } = read_file(Filepath),
  case re:run(First, Regexp) of
    nomatch ->
      put(elixir_compile_core, undefined),
      eval(List, Binding, Filepath);
    _ ->
      try
        put(elixir_compile_core, []),
        eval(List, Binding, Filepath),
        ok = file:write_file(Compiled, term_to_binary(get(elixir_compile_core)))
      after
        put(elixir_compile_core, undefined)
      end
  end.

% Read a file
read_file(FileName) ->
  {ok, Device} = file:open(FileName, [read, {encoding, utf8}]),
  read_file(Device, []).

read_file(Device, Acc) ->
  case io:get_line(Device, "") of
    eof  ->
      file:close(Device),
      Reverse = lists:reverse(Acc),
      { file_head(Reverse), lists:append(Reverse) };
    Line -> read_file(Device, [Line|Acc])
  end.

file_head([])    -> [];
file_head([H|T]) -> H.

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
