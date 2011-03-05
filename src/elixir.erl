-module(elixir).
-export([boot/0, require/2, eval/1, eval/2, eval/3, eval/4, eval/5, parse/2, parse/3]).
-include("elixir.hrl").
-include_lib("kernel/include/file.hrl").

boot() ->
  % Ensure elixir_object_methods is loaded and running
  code:ensure_loaded(elixir_object_methods),

  % Load stdlib files
  Regexp = cache_regexp(),
  BasePath = stdlib_path(),
  BaseFiles = [internal_require(filename:join(BasePath, File), Regexp) || File <- stdlib_files()],

  % Boot the code server
  CodeServer = elixir_constants:lookup('Code::Server'),
  'Code::Server':start(CodeServer, BasePath, BaseFiles),

  % Process given options
  CodeInit = elixir_constants:lookup('Code::Init'),
  'Code::Init':process_argv(CodeInit, init:get_plain_arguments()).

% Return the full path for the Elixir installation.
stdlib_path() ->
  case os:getenv("ELIXIR_PATH") of
    false ->
      Dirname = filename:dirname(?FILE),
      filename:join([Dirname, "..", "lib"]);
    Path -> filename:join([Path, "lib"])
  end.

% Stblib files that are loaded and compiled.
stdlib_files() ->
  [
    "object.ex",
    "module.ex",
    "io.ex",
    "atom.ex",
    "numeric.ex",
    "integer.ex",
    "float.ex",
    "list.ex",
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
    "file.ex",
    "code.ex",
    "code/init.ex",
    "code/server.ex"
  ].

recache() -> os:getenv("ELIXIR_RECACHE") == "1".

% Load core elixir objects.
% Note we pass the self binding as nil when loading object because
% the default binding is Object, which at this point is not defined.
cache_regexp() ->
  { ok, Regexp } = re:compile("\\A%\s*elixir:\s*cache\s*$", [multiline]),
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
  {ok, Binary} = file:read_file(Filepath),
  case re:run(Binary, Regexp) of
    nomatch ->
      put(elixir_compile_core, undefined),
      eval(binary_to_list(Binary), Binding, Filepath);
    _ ->
      try
        put(elixir_compile_core, []),
        eval(binary_to_list(Binary), Binding, Filepath),
        ok = file:write_file(Compiled, term_to_binary(get(elixir_compile_core)))
      after
        put(elixir_compile_core, undefined)
      end
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
  ParseTree = parse(String, SelfBinding, Filename, Line, Scope),
  {value, Value, NewBinding} = erl_eval:exprs(ParseTree, SelfBinding),
  {Value, proplists:delete(self, NewBinding)}.

% Parse string and transform tree to Erlang Abstract Form format
parse(String, Binding) -> parse(String, Binding, "nofile").
parse(String, Binding, Filename) -> parse(String, Binding, Filename, 1, #elixir_scope{}).
parse(String, Binding, Filename, Line, Scope) ->
  Vars = lists:usort(proplists:get_keys(Binding)),
  NewScope = Scope#elixir_scope{vars=Vars, filename=Filename},
  { NewForms, _ } = elixir_transform:parse(String, Line, NewScope),
  NewForms.
