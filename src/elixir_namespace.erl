-module(elixir_namespace).
-export([transform/3, build/3, compile/3]).
-include("elixir.hrl").

transform(Line, Kind, S) ->
  Filename  = S#elixir_scope.filename,
  Namespace = S#elixir_scope.namespace,
  Args = [{integer, Line, Line}, {string, Line, Filename}, {atom, Line, Namespace}],
  ?ELIXIR_WRAP_CALL(Line, ?MODULE, Kind, Args).

build(Line, Filename, Namespace) ->
  AttributeTable = ?ELIXIR_ATOM_CONCAT([a, Namespace]),
  ets:new(AttributeTable, [set, named_table, private]),
  elixir_def_method:new_method_table(Namespace).

compile(Line, Filename, Namespace) ->
  try
    {Export, Macros, Functions} = elixir_def_method:unwrap_stored_methods(Namespace),

    Base = [
      {attribute, Line, module, Namespace},
      {attribute, Line, file, {Filename,Line}},
      % {attribute, Line, compile, no_auto_import()},
      {attribute, Line, export, Export} | Functions
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

% ATTRIBUTES

% no_auto_import() ->
%   {no_auto_import, [
%     {size, 1}, {length, 1}, {error, 2}, {self, 1}, {put, 2},
%     {get, 1}, {exit, 1}, {exit, 2}
%   ]}.

transform_attribute(Line, X) ->
  {attribute, Line, element(1, X), element(2, X)}.

% ERROR HANDLING

format_errors(Filename, []) ->
  exit({nocompile, "compilation failed but no error was raised"});

format_errors(Filename, Errors) ->
  lists:foreach(fun ({_, Each}) ->
    lists:foreach(fun (Error) -> elixir_errors:handle_file_error(Filename, Error) end, Each)
  end, Errors).

format_warnings(Filename, Warnings) ->
  lists:foreach(fun ({_, Each}) ->
    lists:foreach(fun (Warning) -> elixir_errors:handle_file_warning(Filename, Warning) end, Each)
  end, Warnings).