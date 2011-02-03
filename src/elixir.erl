-module(elixir).
-export([boot/0, eval/1, eval/2, eval/3, parse/2, load_file/2]).
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
  Loader = fun(Class) -> load_file(filename:join(Basepath, Class), [{self, []}]) end,
  lists:foreach(Loader, [
    'object.ex',
    'module.ex',
    'atom.ex',
    'numeric.ex',
    'integer.ex',
    'float.ex',
    'list.ex',
    'string.ex',
    'dict.ex',
    'regexp.ex' 
  ]).

% Loads a given file
load_file(Filepath, Bindings) ->
  {ok, Device} = file:open(Filepath, [read, unicode]),
  String = read_line(Device, []),
  eval(String, Bindings, Filepath).

% Read each line as UTF8.
read_line(Device, Accum) ->
  case io:get_line(Device, []) of
    eof  -> file:close(Device), lists:flatten(lists:reverse(Accum));
    Line -> read_line(Device, [Line|Accum])
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