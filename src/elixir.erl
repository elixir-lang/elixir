-module(elixir).
-export([boot/0, eval/1, eval/2, parse/2]).
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
    'numeric.ex',
    'integer.ex',
    'float.ex'
  ]).

% Loads a given file
load_file(Filepath, Bindings) ->
  {ok, Bin} = file:read_file(Filepath),
  eval(binary_to_list(Bin), Bindings).

% Evaluates a string
eval(String) -> eval(String, []).

eval(String, Binding) ->
  SelfBinding = case proplists:get_value(self, Binding) of
                  undefined -> lists:append(Binding, [{self,elixir_constants:lookup('Object')}]);
                  _  -> Binding
                end,
  {value, Value, NewBinding} = erl_eval:exprs(parse(String, SelfBinding), SelfBinding),
  {Value, proplists:delete(self, NewBinding)}.

% Parse string and transform tree to Erlang Abstract Form format
parse(String, Binding) ->
  { NewForms, _ } = elixir_transform:parse(String, Binding),
  NewForms.
