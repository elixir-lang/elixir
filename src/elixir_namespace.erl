-module(elixir_namespace).
-export([transform/3, build/3, compile/3, modulize/1]).
-include("elixir.hrl").

modulize(Args) -> list_to_atom(lists:concat([modulize_(Arg) || Arg <- Args])).

modulize_(Arg) ->
  case Ref = atom_to_list(Arg) of
    "::" ++ _ -> Ref;
    _ -> list_to_atom("::" ++ Ref)
  end.

transform(Line, Kind, S) ->
  Filename  = S#elixir_scope.filename,
  Namespace = S#elixir_scope.namespace,
  Args = [{integer, Line, Line}, {string, Line, Filename}, {atom, Line, Namespace}],
  ?ELIXIR_WRAP_CALL(Line, ?MODULE, Kind, Args).

build(_Line, _Filename, Namespace) ->
  AttributeTable = ?ELIXIR_ATOM_CONCAT([a, Namespace]),
  ets:new(AttributeTable, [set, named_table, private]),
  elixir_def_method:new_method_table(Namespace).

compile(Line, Filename, Namespace) ->
  try
    {E0, Macros, F0} = elixir_def_method:unwrap_stored_methods(Namespace),

    { E1, F1 } = add_extra_function(E0, F0, {'__macros__', 0}, macros_function(Line, Macros)),

    Base = [
      {attribute, Line, module, Namespace},
      {attribute, Line, file, {Filename,Line}},
      % {attribute, Line, compile, no_auto_import()},
      {attribute, Line, export, E1} | F1
    ],

    Transform = fun(X, Acc) -> [transform_attribute(Line, X)|Acc] end,
    Table = ?ELIXIR_ATOM_CONCAT([a, Namespace]),
    Forms = ets:foldr(Transform, Base, Table),
    load_form(Forms, Filename)
  after
    ets:delete(?ELIXIR_ATOM_CONCAT([a,Namespace])),
    ets:delete(?ELIXIR_ATOM_CONCAT([m,Namespace]))
  end.

load_form(Forms, Filename) ->
  case compile:forms(Forms, [return]) of
    {ok, ModuleName, Binary, Warnings} ->
      case get(elixir_compiled) of
        Current when is_list(Current) ->
          put(elixir_compiled, [{ModuleName,Binary}|Current]);
        _ ->
          []
      end,
      format_warnings(Filename, Warnings),
      code:load_binary(ModuleName, Filename, Binary);
    {error, Errors, Warnings} ->
      format_warnings(Filename, Warnings),
      format_errors(Filename, Errors)
  end.

% EXTRA FUNCTIONS

add_extra_function(Exported, Functions, Pair, Contents) ->
  case lists:member(Pair, Exported) of
    true -> elixir_errors:error({internal_method_overridden, Pair});
    false -> { [Pair|Exported], [Contents|Functions] }
  end.

macros_function(Line, Macros) ->
  SortedMacros = lists:sort(Macros),

  { Tuples, [] } = elixir_tree_helpers:build_list(fun({Name, Arity}, Y) ->
    { { tuple, Line, [ {atom, Line, Name}, { integer, Line, Arity } ] }, Y }
  end, SortedMacros, Line, []),

  { function, Line, '__macros__', 0,
    [{ clause, Line, [], [], [Tuples]}]
  }.

% ATTRIBUTES

% no_auto_import() ->
%   {no_auto_import, [
%     {size, 1}, {length, 1}, {error, 2}, {self, 1}, {put, 2},
%     {get, 1}, {exit, 1}, {exit, 2}
%   ]}.

transform_attribute(Line, X) ->
  {attribute, Line, element(1, X), element(2, X)}.

% ERROR HANDLING

format_errors(_Filename, []) ->
  exit({nocompile, "compilation failed but no error was raised"});

format_errors(Filename, Errors) ->
  lists:foreach(fun ({_, Each}) ->
    lists:foreach(fun (Error) -> elixir_errors:handle_file_error(Filename, Error) end, Each)
  end, Errors).

format_warnings(Filename, Warnings) ->
  lists:foreach(fun ({_, Each}) ->
    lists:foreach(fun (Warning) -> elixir_errors:handle_file_warning(Filename, Warning) end, Each)
  end, Warnings).