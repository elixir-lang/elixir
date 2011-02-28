-module(elixir).
-export([boot/0, eval/1, eval/2, eval/3, eval/4, eval/5, parse/2, parse/3, require_file/1, require_file/2]).
-include("elixir.hrl").

% Boot up Elixir setting up tables and loading main files.
boot() ->
  code:ensure_loaded(elixir_object_methods),
  BasePath = stdlib_path(),
  BaseFiles = stdlib_files(),
  load_core_classes(BasePath, BaseFiles),
  boot_code_server(BasePath, BaseFiles).

% Return the full path for the Elixir installation.
stdlib_path() ->
  case os:getenv("ELIXIR_PATH") of
    false ->
      Dirname = filename:dirname(?FILE),
      filename:join([Dirname, "..", "lib"]);
    Path -> filename:join([Path, "lib"])
  end.

% Returns all stdlib files that are loaded on boot.
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
    "code.ex"
  ].

% Load core elixir classes.
% Note we pass the self binding as nil when loading object because
% the default binding is Object, which at this point is not defined.
load_core_classes(BasePath, BaseFiles) ->
  Loader = fun(File) ->
    Filepath = filename:join(BasePath, File),
    {ok, Binary} = file:read_file(Filepath),
    eval(binary_to_list(Binary), [{self, []}], Filepath)
  end,
  lists:foreach(Loader, BaseFiles).

% Finally boot the code server!
boot_code_server(BasePath, BaseFiles) ->
  Expanded = 'File::Mixin':expand_path(self, BasePath),
  State = { [Expanded], BaseFiles },
  Module = 'Code::Server':'__callbacks_module__'(self),
  gen_server:start({local, elixir_code_server}, Module, State, []).

% Paths are hardcoded here. We need to add support to load paths.
% TODO This could be done completely with Elixir code.
require_file(Path) ->
  Dirname = filename:dirname(?FILE),
  Paths = [
    filename:join([Dirname, "..", "lib"]),
    filename:join([Dirname, "..", "test", "elixir"]),
    "."
  ],
  require_file(Path ++ ".ex", Paths).

require_file(Path, []) ->
  elixir_errors:raise(enoent, "could not load file ~ts", [Path]);

require_file(Path, [H|T]) ->
  Filepath = filename:join(H, Path),
  case file:read_file(Filepath) of
    { ok, Binary } -> eval(binary_to_list(Binary), [], Filepath);
    { error, enoent } -> require_file(Path, T);
    { error, Reason } -> elixir_errors:raise(Reason, "could not load file ~ts", [Filepath])
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