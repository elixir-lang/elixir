-module(elixir_module).
-export([transform/4, compile/4, format_error/1,
  read_attribute/2, insert_attribute/3, update_attribute/3,
  ensure_loaded/4]).
-include("elixir.hrl").

ensure_loaded(Line, Module, S, Force) ->
  case not Force andalso lists:member(Module, S#elixir_scope.scheduled) of
    true  -> ok;
    false ->
      case code:ensure_loaded(Module) of
        { module, _ }   -> ok;
        { error, What } ->
          elixir_errors:form_error(Line, S#elixir_scope.filename, ?MODULE, { unloaded_module, { Module, What } })
      end
  end.

%% TABLE METHODS

table(Module) ->
  ?ELIXIR_ATOM_CONCAT([a, Module]).

read_attribute(Module, Attr) ->
  ets:lookup_element(table(Module), Attr, 2).

insert_attribute(Module, Attr, Value) ->
  ets:insert(table(Module), { Attr, Value }).

update_attribute(Module, Attr, Fun) ->
  Current = read_attribute(Module, Attr),
  insert_attribute(Module, Attr, Fun(Current)).

%% TRANSFORMATION METHODS

%% Transformation of args and scope into a compiled erlang call.
%% The abstract form for extra arguments may be given and they
%% will be passed to the invoked function.

transform(Line, Filename, Ref, Block) ->
  Tree = elixir_tree_helpers:abstract_syntax(Block),
  Args = [{integer, Line, Line}, {string, Line, Filename}, Ref, Tree],
  ?ELIXIR_WRAP_CALL(Line, ?MODULE, compile, Args).

%% The compilation hook.

compile(Line, Filename, Module, Block) when is_atom(Module) ->
  build(Module),

  try
    { Result, S }   = eval_form(Line, Filename, Module, Block),
    { All, Forms0 } = base_form(Line, Filename, Module),

    Table  = table(Module),
    _Data  = destructive_read(Table, data),

    %% Handle attributes
    Transform0 = fun(X, Acc) -> [transform_attribute(Line, X)|Acc] end,
    Forms1 = ets:foldr(Transform0, Forms0, Table),

    %% Handle only imports
    OnlyImports = elixir_import:only_imports(Module),
    Transform1 = fun(X, Acc) -> transform_import(Line, X, Acc) end,
    { Imported, Forms1 } = lists:mapfoldr(Transform1, Forms1, OnlyImports),

    %% Ensure no conflicts
    AllWithImported = lists:append([All|Imported]),
    elixir_import:ensure_no_macro_conflict(Line, Module, AllWithImported, S),

    load_form(Forms1, Filename),
    Result
  after
    ets:delete(table(Module)),
    elixir_def:delete_table(Module),
    elixir_import:delete_table(Module)
  end;

compile(Line, Filename, Other, _Block) ->
  elixir_errors:form_error(Line, Filename, ?MODULE, { invalid_module, Other }).

%% Hook that builds both attribute and functions and set up common hooks.

build(Module) ->
  %% Attribute table
  Table = table(Module),
  ets:new(Table, [set, named_table, private]),

  %% Default attributes
  ets:insert(Table, { data, [] }),

  %% Function table
  elixir_def:build_table(Module),

  %% Import tables
  elixir_import:build_table(Module).

%% Receives the module representation and evaluates it.

eval_form(Line, Filename, Module, Block) ->
  S = #elixir_scope{filename=Filename, module={Line,Module}},
  { TBlock, TS } = elixir_translator:translate_each(Block, S),
  { value, Result, _ } = erl_eval:exprs([TBlock], []),
  { Result, TS }.

%% Return the base form with module, exports and function declaratins.

base_form(Line, Filename, Module) ->
  {E0, Private, Macros, F0} = elixir_def:unwrap_stored_definitions(Module),
  { E1, F1 } = add_extra_function(Line, Filename, E0, F0, {'__macros__', 0}, macros_function(Line, Macros)),

  { E1 ++ Private, [
    {attribute, Line, module, Module},
    {attribute, Line, file, {Filename,Line}},
    {attribute, Line, export, E1} | F1
  ] }.

%% Loads the form into the code server.

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

% ATTRIBUTES

% no_auto_import() ->
%   {no_auto_import, [
%     {size, 1}, {length, 1}, {error, 2}, {self, 1}, {put, 2},
%     {get, 1}, {exit, 1}, {exit, 2}
%   ]}.

transform_import(Line, X, Acc) ->
  { element(2, X), [{attribute, Line, import, X}|Acc] }.

transform_attribute(Line, X) ->
  {attribute, Line, element(1, X), element(2, X)}.

destructive_read(Table, Attribute) ->
  Value = ets:lookup_element(Table, Attribute, 2),
  ets:delete(Table, Attribute),
  Value.

% ERROR HANDLING

format_error({internal_function_overridden,{Name,Arity}}) ->
  io_lib:format("function ~s/~B is internal and should not be overriden", [Name, Arity]);

format_error({unloaded_module,{ Module, What }}) ->
  io_lib:format("module ~s is not loaded, reason: ~s", [Module, What]);

format_error({invalid_module, Module}) ->
  io_lib:format("invalid module name: ~p", [Module]).

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