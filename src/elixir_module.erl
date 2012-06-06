-module(elixir_module).
-export([translate/4, compile/4, data_table/1,
   format_error/1, scope_for_eval/2, binding_for_eval/2]).
-include("elixir.hrl").

scope_for_eval(Module, #elixir_scope{} = S) ->
  S#elixir_scope{module=Module};

scope_for_eval(Module, Opts) ->
  scope_for_eval(Module, elixir:scope_for_eval(Opts)).

binding_for_eval(Module, Binding) -> [{'_@MODULE',Module}|Binding].

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
  MetaS     = elixir_scope:serialize(S),

  Args = [{integer, Line, Line}, Ref, MetaBlock, MetaS],
  ?ELIXIR_WRAP_CALL(Line, ?MODULE, compile, Args).

%% The compilation hook.

compile(Line, Module, Block, RawS) when is_atom(Module) ->
  S = elixir_scope:deserialize(RawS),
  C = elixir_compiler:get_opts(),
  Filename = S#elixir_scope.filename,

  check_module_availability(Line, Filename, Module, C),
  build(Module),

  try
    Result           = eval_form(Line, Module, Block, S),
    { Funs, Forms0 } = functions_form(Line, Filename, Module, C),
    Forms1           = attributes_form(Line, Filename, Module, Forms0),

    elixir_import:ensure_no_local_conflict(Line, Filename, Module, Funs),
    elixir_import:ensure_no_import_conflict(Line, Filename, Module, Funs),

    Final = [
      {attribute, Line, file, {Filename,Line}},
      {attribute, Line, module, Module} | Forms1
    ],

    load_form(Final, S),
    Result
  after
    ets:delete(data_table(Module)),
    ets:delete(docs_table(Module)),
    elixir_def:delete_table(Module),
    elixir_import:delete_table(Module)
  end;

compile(Line, Other, _Block, RawS) ->
  S = elixir_scope:deserialize(RawS),
  elixir_errors:form_error(Line, S#elixir_scope.filename, ?MODULE, { invalid_module, Other }).

%% Hook that builds both attribute and functions and set up common hooks.

build(Module) ->
  %% Table with meta information about the module.
  DataTable = data_table(Module),
  ets:new(DataTable, [set, named_table, public]),
  ets:insert(DataTable, { '__overridable', [] }),
  ets:insert(DataTable, { '__compile_callbacks', [] }),

  Attributes = [behavior, behaviour, on_load, spec, type, export_type, callback, compile],
  ets:insert(DataTable, { '__acc_attributes', Attributes }),
  ets:insert(DataTable, { '__persisted_attributes', [vsn|Attributes] }),

  %% Keep docs in another table since we don't want to pull out
  %% all the binaries every time a new documentation is stored.
  DocsTable = docs_table(Module),
  ets:new(DocsTable, [ordered_set, named_table, public]),

  %% We keep a separated table for function definitions
  %% and another one for imports. We keep them in different
  %% tables for organization and speed purpose (since the
  %% imports table is frequently written to).
  elixir_def:build_table(Module),
  elixir_import:build_table(Module).

%% Receives the module representation and evaluates it.

eval_form(Line, Module, Block, RawS) ->
  Temp = ?ELIXIR_ATOM_CONCAT(["COMPILE-",Module]),
  S = scope_for_eval(Module, RawS),
  { Value, NewS } = elixir_compiler:eval_forms([Block], Line, Temp, S),
  elixir_def_overridable:store_pending(Module),
  eval_callbacks(Line, Module, '__compile_callbacks', [Module], NewS),
  Value.

%% Return the form with exports and function declarations.

functions_form(Line, Filename, Module, C) ->
  { Export, Private, Def, Defmacro, Defmacrop, Functions } = elixir_def:unwrap_stored_definitions(Module),

  { FinalExport, FinalFunctions } =
    add_info_function(Line, Filename, Module, Export, Functions, Def, Defmacro, C),

  Recorded = elixir_import:recorded_locals(Module),
  elixir_def_local:check_unused_local_macros(Filename, Recorded, Defmacrop),

  { FinalExport ++ Private, [
    {attribute, Line, export, lists:sort(FinalExport)} | FinalFunctions
  ] }.

%% Add attributes handling to the form

attributes_form(Line, _Filename, Module, Current) ->
  Table = data_table(Module),

  AccAttrs = ets:lookup_element(Table, '__acc_attributes', 2),
  PersistedAttrs = ets:lookup_element(Table, '__persisted_attributes', 2),

  Transform = fun({ Key, Value }, Acc) ->
    case lists:member(Key, PersistedAttrs) of
      false -> Acc;
      true  ->
        Attrs = case lists:member(Key, AccAttrs) of
          true  -> Value;
          false -> [Value]
        end,
        lists:foldl(fun(X, Final) -> [{ attribute, Line, Key, X }|Final] end, Acc, Attrs)
    end
  end,

  ets:foldl(Transform, Current, Table).

%% Loads the form into the code server.

load_form(Forms, S) ->
  elixir_compiler:module(Forms, S, fun(ModuleName, Binary) ->
    case get(elixir_compiled) of
      Current when is_list(Current) ->
        put(elixir_compiled, [{ModuleName,Binary}|Current]),
        case get(elixir_parent_compiler) of
          undefined -> [];
          PID -> PID ! { module_available, self(), ModuleName, Binary }
        end;
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

add_info_function(Line, Filename, Module, Export, Functions, Def, Defmacro, C) ->
  Pair = { '__info__', 1 },
  case lists:member(Pair, Export) of
    true  -> elixir_errors:form_error(Line, Filename, ?MODULE, {internal_function_overridden, Pair});
    false ->
      Docs = elixir_compiler:get_opt(docs, C),
      Contents = { function, Line, '__info__', 1, [
        functions_clause(Line, Def),
        macros_clause(Line, Defmacro),
        docs_clause(Line, Module, Docs),
        moduledoc_clause(Line, Module, Docs),
        compile_clause(Line),
        else_clause(Line)
      ] },
      { [Pair|Export], [Contents|Functions] }
  end.

functions_clause(Line, Def) ->
  Sorted = ordsets:from_list([{'__info__',1}|Def]),
  { clause, Line, [{ atom, Line, functions }], [], [elixir_tree_helpers:abstract_syntax(Sorted)] }.

macros_clause(Line, Defmacro) ->
  Sorted = ordsets:from_list(Defmacro),
  { clause, Line, [{ atom, Line, macros }], [], [elixir_tree_helpers:abstract_syntax(Sorted)] }.

docs_clause(Line, Module, true) ->
  Docs = ordsets:from_list(ets:tab2list(docs_table(Module))),
  { clause, Line, [{ atom, Line, docs }], [], [elixir_tree_helpers:abstract_syntax(Docs)] };

docs_clause(Line, _Module, _) ->
  { clause, Line, [{ atom, Line, docs }], [], [{ atom, Line, nil }] }.

moduledoc_clause(Line, Module, true) ->
  Docs = '__MAIN__.Module':read_attribute(Module, moduledoc),
  { clause, Line, [{ atom, Line, moduledoc }], [], [elixir_tree_helpers:abstract_syntax({ Line, Docs })] };

moduledoc_clause(Line, _Module, _) ->
  { clause, Line, [{ atom, Line, moduledoc }], [], [{ atom, Line, nil }] }.

compile_clause(Line) ->
  Info = { call, Line, { atom, Line, module_info }, [{ atom, Line, compile }] },
  WrappedInfo = ?ELIXIR_WRAP_CALL(Line, '__MAIN__.Keyword', 'from_enum', [Info]),
  { clause, Line, [{ atom, Line, compile }], [], [WrappedInfo] }.

else_clause(Line) ->
  Info = { call, Line, { atom, Line, module_info }, [{ var, Line, atom }] },
  { clause, Line, [{ var, Line, atom }], [], [Info] }.

% HELPERS

eval_callbacks(Line, Module, Name, Args, RawS) ->
  S         = RawS#elixir_scope{check_clauses=false},
  Binding   = binding_for_eval(Module, []),
  Callbacks = ets:lookup_element(data_table(Module), Name, 2),
  Requires  = S#elixir_scope.requires,

  lists:foreach(fun({M,F}) ->
    Expr  = { { '.', Line, [M,F] }, Line, Args },
    Scope = case ordsets:is_element(M, Requires) of
      true  -> S;
      false -> S#elixir_scope{requires=ordsets:add_element(M, Requires)}
    end,

    { Tree, _ } = elixir_translator:translate_each(Expr, Scope),

    try
      erl_eval:exprs([Tree], Binding)
    catch
      Kind:Reason ->
        Info = { M, F, Args, [{ file, binary_to_list(S#elixir_scope.filename) }, { line, Line }] },
        erlang:raise(Kind, Reason, [Info|erlang:get_stacktrace()])
    end
  end, Callbacks).

% ERROR HANDLING

format_error({ internal_function_overridden, { Name, Arity } }) ->
  io_lib:format("function ~s/~B is internal and should not be overriden", [Name, Arity]);

format_error({ invalid_module, Module}) ->
  io_lib:format("invalid module name: ~p", [Module]);

format_error({ module_defined, Module }) ->
  io_lib:format("module ~s already defined (please remove already compiled files before recompiling a module)",
    [elixir_errors:inspect(Module)]).
