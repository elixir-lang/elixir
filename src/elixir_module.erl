-module(elixir_module).
-export([translate/4, compile/4, data/1, data/2, data_table/1,
   format_error/1, binding_and_scope_for_eval/3]).
-include("elixir.hrl").

binding_and_scope_for_eval(Opts, Module, Binding) ->
  binding_and_scope_for_eval(Opts, Module, Binding, elixir:scope_for_eval(Opts)).

binding_and_scope_for_eval(_Opts, Module, Binding, S) ->
  {
    binding_for_eval(Module, Binding),
    S#elixir_scope{module=Module}
  }.

binding_for_eval(Module, Binding) -> [{'_EXMODULE',Module}|Binding].

data(Module) ->
  ets:lookup_element(data_table(Module), data, 2).

data(Module, Value) ->
  ets:insert(data_table(Module), { data, Value }).

%% TABLE METHODS

data_table(Module) ->
  ?ELIXIR_ATOM_CONCAT([d, Module]).

docs_table(Module) ->
  ?ELIXIR_ATOM_CONCAT([o, Module]).

%% TRANSFORMATION METHODS

%% Transformation of args and scope into a compiled erlang call.
%% The abstract form for extra arguments may be given and they
%% will be passed to the invoked function.

translate(Line, Ref, Block, S) ->
  MetaBlock = elixir_tree_helpers:abstract_syntax(Block),
  MetaS     = elixir_variables:serialize_scope(S),

  Args = [{integer, Line, Line}, Ref, MetaBlock, MetaS],
  ?ELIXIR_WRAP_CALL(Line, ?MODULE, compile, Args).

%% The compilation hook.

compile(Line, Module, Block, RawS) when is_atom(Module) ->
  S = elixir_variables:deserialize_scope(RawS),
  C = elixir_compiler:get_opts(),
  Filename = S#elixir_scope.filename,

  check_module_availability(Line, Filename, Module, C),
  build(Module),

  try
    Result           = eval_form(Line, Filename, Module, Block, S),
    { Funs, Forms0 } = functions_form(Line, Filename, Module, C),
    Forms1           = attributes_form(Line, Filename, Module, Forms0),

    elixir_import:ensure_no_local_conflict(Line, Filename, Module, Funs),
    elixir_import:ensure_no_import_conflict(Line, Filename, Module, Funs),

    Final = [
      {attribute, Line, module, Module},
      {attribute, Line, file, {Filename,Line}} | Forms1
    ],

    load_form(Final, S),
    Result
  after
    ets:delete(data_table(Module)),
    ets:delete(docs_table(Module)),
    elixir_def:delete_table(Module),
    elixir_def_local:delete_table(Module),
    elixir_import:delete_table(Module)
  end;

compile(Line, Other, _Block, RawS) ->
  S = elixir_variables:deserialize_scope(RawS),
  elixir_errors:form_error(Line, S#elixir_scope.filename, ?MODULE, { invalid_module, Other }).

%% Hook that builds both attribute and functions and set up common hooks.

build(Module) ->
  %% Table with meta information about the module.
  DataTable = data_table(Module),
  ets:new(DataTable, [set, named_table, private]),
  ets:insert(DataTable, { data, [] }),
  ets:insert(DataTable, { attributes, [] }),
  ets:insert(DataTable, { overridable, [] }),
  ets:insert(DataTable, { compile_callbacks, [] }),
  ets:insert(DataTable, { registered_attributes, [behavior, behaviour, compile, vsn, on_load] }),

  %% Keep docs in another table since we don't want to pull out
  %% all the binaries every time a new documentation is stored.
  DocsTable = docs_table(Module),
  ets:new(DocsTable, [ordered_set, named_table, private]),

  %% We keep a separated table for function definitions,
  %% another for locals calls and another one for imports.
  %% We keep them in different tables for organization
  %% and speed purpose (since the imports and locals ones
  %% are frequently written to).
  elixir_def:build_table(Module),
  elixir_def_local:build_table(Module),
  elixir_import:build_table(Module).

%% Receives the module representation and evaluates it.

eval_form(Line, Filename, Module, Block, RawS) ->
  Temp = ?ELIXIR_ATOM_CONCAT(["COMPILE-",Module]),
  { Binding, S } = binding_and_scope_for_eval([{file,Filename}], Module, [], RawS),
  { Value, NewS } = elixir_compiler:eval_forms([Block], Line, Temp, S),
  elixir_def_overridable:store_pending(Module),
  { Callbacks, FinalS } = callbacks_for(Line, compile_callbacks, Module, [Module], NewS),
  elixir:eval_forms(Callbacks, binding_for_eval(Module, Binding), FinalS#elixir_scope{check_clauses=false}),
  Value.

%% Return the form with exports and function declarations.

functions_form(Line, Filename, Module, C) ->
  { Export, Private, Macros, PMacros, Functions } = elixir_def:unwrap_stored_definitions(Module),

  { FinalExport, FinalFunctions } =
    add_info_function(Line, Filename, Module, Export, Functions, Macros, C),

  elixir_def_local:check_unused_local_macros(Filename, Module, PMacros),

  { FinalExport ++ Private, [
    {attribute, Line, export, lists:sort(FinalExport)} | FinalFunctions
  ] }.

%% Add attributes handling to the form

attributes_form(Line, _Filename, Module, Current) ->
  Transform = fun(X, Acc) -> [translate_attribute(Line, X)|Acc] end,
  Attributes = ets:lookup_element(data_table(Module), attributes, 2),
  lists:foldl(Transform, Current, Attributes).

%% Loads the form into the code server.

load_form(Forms, S) ->
  elixir_compiler:module(Forms, S, fun(ModuleName, Binary) ->
    case get(elixir_compiled) of
      Current when is_list(Current) ->
        put(elixir_compiled, [{ModuleName,Binary}|Current]);
      _ ->
        []
    end
  end).

check_module_availability(Line, Filename, Module, Compiler) ->
  case elixir_compiler:get_opt(ignore_module_conflict, Compiler) of
    false ->
      case code:ensure_loaded(Module) of
        { module, _ } -> elixir_errors:form_error(Line, Filename, ?MODULE, { module_defined, Module });
        { error, _ }  -> []
      end;
    true ->
      []
  end.

% EXTRA FUNCTIONS

add_info_function(Line, Filename, Module, Export, Functions, Macros, C) ->
  Pair = { '__info__', 1 },
  case lists:member(Pair, Export) of
    true  -> elixir_errors:form_error(Line, Filename, ?MODULE, {internal_function_overridden, Pair});
    false ->
      Docs = elixir_compiler:get_opt(docs, C),
      Contents = { function, Line, '__info__', 1, [
        macros_clause(Line, Macros),
        data_clause(Line, Module),
        docs_clause(Line, Module, Docs),
        moduledoc_clause(Line, Module, Docs),
        else_clause(Line)
      ] },
      { [Pair|Export], [Contents|Functions] }
  end.

macros_clause(Line, Macros) ->
  Sorted = lists:sort(Macros),
  { clause, Line, [{ atom, Line, macros }], [], [elixir_tree_helpers:abstract_syntax(Sorted)] }.

docs_clause(Line, Module, true) ->
  Docs = ets:tab2list(docs_table(Module)),
  { clause, Line, [{ atom, Line, docs }], [], [elixir_tree_helpers:abstract_syntax(Docs)] };

docs_clause(Line, _Module, _) ->
  { clause, Line, [{ atom, Line, docs }], [], [{ atom, Line, nil }] }.

moduledoc_clause(Line, Module, true) ->
  Docs = '__MAIN__.Module':read_data(Module, moduledoc),
  { clause, Line, [{ atom, Line, moduledoc }], [], [elixir_tree_helpers:abstract_syntax({ Line, Docs })] };

moduledoc_clause(Line, _Module, _) ->
  { clause, Line, [{ atom, Line, moduledoc }], [], [{ atom, Line, nil }] }.

data_clause(Line, Module) ->
  DataTable  = data_table(Module),
  Data       = ets:lookup_element(DataTable, data, 2),
  Pruned     = translate_data(Data),
  { clause, Line, [{ atom, Line, data }], [], [elixir_tree_helpers:abstract_syntax(Pruned)] }.

else_clause(Line) ->
  Info = { call, Line, { atom, Line, module_info }, [{ var, Line, atom }] },
  { clause, Line, [{ var, Line, atom }], [], [Info] }.

% HELPERS

callbacks_for(Line, Kind, Module, Args, S) ->
  Table = data_table(Module),
  Callbacks = ets:lookup_element(Table, Kind, 2),

  { Exprs, Refers } = lists:mapfoldl(
    fun (X, Acc) -> each_callback_for(Line, Args, X, Acc) end,
    S#elixir_scope.refer, Callbacks),

  { Exprs, S#elixir_scope{refer=Refers} }.

each_callback_for(Line, Args, {M,F}, Acc) ->
  Expr = { { '.', Line, [M,F] }, Line, Args },
  Refer = case orddict:find(M, Acc) of
    { ok, _ } -> Acc;
    _ -> orddict:store(M, M, Acc)
  end,
  { Expr, Refer }.

% ATTRIBUTES & DATA

translate_attribute(Line, X) ->
  { attribute, Line, element(1, X), element(2, X) }.

translate_data([{K,V}|T]) when
  K == doc; K == moduledoc; K == spec; K == type;
  K == export_type; K == callbacks; K == overridable;
  V == nil ->
  translate_data(T);

translate_data([{K,V}|T]) ->
  [{K,V}|translate_data(T)];

translate_data([]) -> [].

% ERROR HANDLING

format_error({ internal_function_overridden, { Name, Arity } }) ->
  io_lib:format("function ~s/~B is internal and should not be overriden", [Name, Arity]);

format_error({ invalid_module, Module}) ->
  io_lib:format("invalid module name: ~p", [Module]);

format_error({ module_defined, Module }) ->
  io_lib:format("module ~s already defined (please ensure remove compiled files before recompiling a module)",
    [elixir_errors:inspect(Module)]).
