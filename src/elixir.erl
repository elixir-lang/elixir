-module(elixir).
-export([boot/0, eval/1, eval/2, eval/3, parse/2, require_file/1, require_file/2]).
-include("elixir.hrl").

% Boot up Elixir setting up tables and loading main files.
boot() ->
  code:ensure_loaded(elixir_object_methods),
  load_core_classes().

% Load core elixir classes.
% Note we pass the self binding as nil when loading object because
% the default binding is Object, which at this point is not defined.
load_core_classes() ->
  Dirname = filename:dirname(?FILE),
  Basepath = filename:join([Dirname, "..", "lib"]),
  Loader = fun(Class) ->
    Filepath = filename:join(Basepath, Class),
    {ok, Binary} = file:read_file(Filepath),
    eval(binary_to_list(Binary), [{self, []}], Filepath)
  end,
  lists:foreach(Loader, [
    'object.ex',
    'module.ex',
    'code.ex',
    'io.ex',
    'atom.ex',
    'numeric.ex',
    'integer.ex',
    'float.ex',
    'list.ex',
    'tuple.ex',
    'string.ex',
    'ordered_dict.ex',
    'regexp.ex',
    'binary.ex'
  ]).

% Paths are hardcoded here. We need to add support to load paths.
% TODO This could be done completely with Elixir code.
require_file(Path) ->
  Dirname = filename:dirname(?FILE),
  Paths = [
    filename:join([Dirname, "..", "lib"]),
    filename:join([Dirname, "..", "test", "elixir"])
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

eval(String, Binding, Filename) ->
  SelfBinding = case proplists:get_value(self, Binding) of
    undefined -> lists:append(Binding, [{self,elixir_constants:lookup('Object')}]);
    _  -> Binding
  end,
  {value, Value, NewBinding} = erl_eval:exprs(parse(String, SelfBinding, Filename), SelfBinding),
  {Value, proplists:delete(self, NewBinding)}.

% Parse string and transform tree to Erlang Abstract Form format
parse(String, Binding) -> parse(String, Binding, "nofile").

parse(String, Binding, Filename) ->
  { NewForms, _ } = elixir_transform:parse(String, Binding, Filename),
  NewForms.