-module(elixir_module).
-export([transform/2, transform/3, transform/4,
  build/3, compile/3, compile/4, format_error/1]).
-include("elixir.hrl").

%% Transformation of args and scope into a compiled erlang call.
%% The abstract form for extra arguments may be given and they
%% will be passed to the invoked function.

transform(Kind, S) -> transform(Kind, S, element(1, S#elixir_scope.module)).
transform(Kind, S, Line) -> transform(Kind, S, Line, []).
transform(Kind, S, Line, Raw) ->
  Filename  = S#elixir_scope.filename,
  { _, Module } = S#elixir_scope.module,
  Args = [{integer, Line, Line}, {string, Line, Filename}, {atom, Line, Module}|Raw],
  ?ELIXIR_WRAP_CALL(Line, ?MODULE, Kind, Args).

%% Hook that builds the table for the module.

build(_Line, _Filename, Module) ->
  build_table(Module),
  elixir_def:build_table(Module).

%% Hook for compilation. A function may be passed as fourth argument.
%% The function is invoked before the module is compiled.

compile(Line, Filename, Module) ->
  compile(Line, Filename, Module, fun() -> [] end).

compile(Line, Filename, Module, Fun) ->
  try
    Result = Fun(),

    {E0, Macros, F0} = elixir_def:unwrap_stored_definitions(Module),
    { E1, F1 } = add_extra_function(Line, Filename, E0, F0, {'__macros__', 0}, macros_function(Line, Macros)),

    Base = [
      {attribute, Line, module, Module},
      {attribute, Line, file, {Filename,Line}},
      % {attribute, Line, compile, no_auto_import()},
      {attribute, Line, export, E1} | F1
    ],

    Transform = fun(X, Acc) -> [transform_attribute(Line, X)|Acc] end,
    Forms = ets:foldr(Transform, Base, table(Module)),
    load_form(Forms, Filename),
    Result
  after
    delete_table(Module),
    elixir_def:delete_table(Module)
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

add_extra_function(Line, Filename, Exported, Functions, Pair, Contents) ->
  case lists:member(Pair, Exported) of
    true -> elixir_errors:form_error(Line, Filename, ?MODULE, {internal_function_overridden, Pair});
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

%% Table methods

table(Module) ->
  ?ELIXIR_ATOM_CONCAT([a, Module]).

build_table(Module) ->
  ets:new(table(Module), [set, named_table, private]).

delete_table(Module) ->
  ets:delete(table(Module)).

% ATTRIBUTES

% no_auto_import() ->
%   {no_auto_import, [
%     {size, 1}, {length, 1}, {error, 2}, {self, 1}, {put, 2},
%     {get, 1}, {exit, 1}, {exit, 2}
%   ]}.

transform_attribute(Line, X) ->
  {attribute, Line, element(1, X), element(2, X)}.

% ERROR HANDLING

format_error({internal_function_overridden,{Name,Arity}}) ->
  io_lib:format("function ~s/~B is internal and should not be overriden", [Name, Arity]).

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