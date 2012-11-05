-module(elixir_module).
-export([translate/4, compile/5, data_table/1, eval_quoted/4, format_error/1]).
-include("elixir.hrl").
-compile({parse_transform, elixir_transform}).

eval_quoted(Module, Quoted, RawBinding, Opts) ->
  Binding = binding_for_eval(Module, RawBinding),
  Scope   = scope_for_eval(Module, Opts),

  elixir_def:reset_last(Module),

  case lists:keyfind(line, 1, Opts) of
    { line, Line } -> Line;
    false -> Line = 1
  end,

  { Value, FinalBinding, _Scope } = elixir:eval_quoted([Quoted], Binding, Line, Scope),
  { Value, FinalBinding }.

scope_for_eval(Module, #elixir_scope{} = S) ->
  S#elixir_scope{module=Module};

scope_for_eval(Module, Opts) ->
  scope_for_eval(Module, elixir:scope_for_eval(Opts)).

binding_for_eval(Module, Binding) -> [{'_@MODULE',Module}|Binding].

%% TABLE METHODS

data_table(Module) ->
  Module.

docs_table(Module) ->
  ?ELIXIR_ATOM_CONCAT([o, Module]).

spec_table(Module) ->
  ?ELIXIR_ATOM_CONCAT([s, Module]).

%% TRANSFORMATION FUNCTIONS

%% Transformation of args and scope into a compiled erlang call.
%% The abstract form for extra arguments may be given and they
%% will be passed to the invoked function.

translate(Line, Ref, Block, S) ->
  MetaBlock = elixir_tree_helpers:abstract_syntax(Block),
  MetaS     = elixir_scope:serialize(S),

  Vars = orddict:fold(fun(Key, Value, { Acc, Counter }) ->
    { { cons, Line, { tuple, Line, [
      { atom, Line, Key },
      { atom, Line, ?ELIXIR_ATOM_CONCAT(["_@", Counter]) },
      { var,  Line, Value }
    ] }, Acc }, Counter + 1 }
  end, { { nil, Line }, 0 }, S#elixir_scope.vars),

  Args = [{integer, Line, Line}, Ref, MetaBlock, element(1, Vars), MetaS],
  ?ELIXIR_WRAP_CALL(Line, ?MODULE, compile, Args).

%% The compilation hook.

compile(Line, Module, Block, Vars, #elixir_scope{} = S) when is_atom(Module) ->
  C = elixir_compiler:get_opts(),
  File = S#elixir_scope.file,

  check_module_availability(Line, File, Module, C),
  build(Line, File, Module),

  try
    Result = eval_form(Line, Module, Block, Vars, S),
    { Export, Private, Def, Defmacro, Defmacrop, Functions } = elixir_def:unwrap_stored_definitions(Module),

    { All, Forms0 } = functions_form(Line, File, Module, Export, Private, Def, Defmacro, Defmacrop, Functions, C),
    Forms1          = specs_form(Line, Module, Defmacro, Defmacrop, Forms0, C),
    Forms2          = attributes_form(Line, File, Module, Forms1),

    elixir_import:ensure_no_local_conflict(Line, File, Module, All),
    elixir_import:ensure_no_import_conflict(Line, File, Module, All),

    Final = [
      { attribute, Line, file, { binary_to_list(File), Line } },
      { attribute, Line, module, Module } | Forms2
    ],

    Binary = load_form(Line, Final, S),
    { module, Module, Binary, Result }
  after
    ets:delete(data_table(Module)),
    ets:delete(docs_table(Module)),
    ets:delete(spec_table(Module)),
    elixir_def:delete_table(Module),
    elixir_import:delete_table(Module)
  end;

compile(Line, Other, _Block, _Vars, #elixir_scope{file=File}) ->
  elixir_errors:form_error(Line, File, ?MODULE, { invalid_module, Other });

compile(Line, Module, Block, Vars, RawS) ->
  Dict = [{ X, Y } || { X, Y, _ } <- Vars],
  S = elixir_scope:deserialize(RawS, Dict),
  compile(Line, Module, Block, Vars, S).

%% Hook that builds both attribute and functions and set up common hooks.

build(Line, File, Module) ->
  %% Table with meta information about the module.
  DataTable = data_table(Module),

  case ets:info(DataTable, name) == DataTable of
    true  -> elixir_errors:form_error(Line, File, ?MODULE, { module_in_definition, Module });
    false -> []
  end,

  ets:new(DataTable, [set, named_table, public]),
  ets:insert(DataTable, { '__overridable', [] }),
  ets:insert(DataTable, { before_compile, [] }),
  ets:insert(DataTable, { after_compile, [] }),

  case elixir_compiler:get_opt(docs) of
    true -> ets:insert(DataTable, { on_definition, [{ 'Elixir.Module', compile_doc }] });
    _    -> ets:insert(DataTable, { on_definition, [] })
  end,

  Attributes = [behavior, behaviour, on_load, spec, type, export_type, opaque, callback, compile],
  ets:insert(DataTable, { '__acc_attributes', [before_compile,after_compile,on_definition|Attributes] }),
  ets:insert(DataTable, { '__persisted_attributes', [vsn|Attributes] }),

  %% Keep docs in another table since we don't want to pull out
  %% all the binaries every time a new documentation is stored.
  DocsTable = docs_table(Module),
  ets:new(DocsTable, [ordered_set, named_table, public]),

  %% Holds specs in a format it is easier to query.
  SpecTable = spec_table(Module),
  ets:new(SpecTable, [duplicate_bag, named_table, public]),

  %% We keep a separated table for function definitions
  %% and another one for imports. We keep them in different
  %% tables for organization and speed purpose (since the
  %% imports table is frequently written to).
  elixir_def:build_table(Module),
  elixir_import:build_table(Module).

%% Receives the module representation and evaluates it.

eval_form(Line, Module, Block, Vars, RawS) ->
  Temp = ?ELIXIR_ATOM_CONCAT(["COMPILE-",Module]),
  S = scope_for_eval(Module, RawS),
  { Value, NewS } = elixir_compiler:eval_forms([Block], Line, Temp, Vars, S),
  elixir_def_overridable:store_pending(Module),
  eval_callbacks(Line, Module, before_compile, [Module], NewS),
  elixir_def_overridable:store_pending(Module),
  Value.

%% Return the form with exports and function declarations.
functions_form(Line, File, Module, Export, Private, Def, Defmacro, Defmacrop, Functions, C) ->
  { FinalExport, FinalFunctions } =
    add_info_function(Line, File, Module, Export, Functions, Def, Defmacro, C),

  Recorded = elixir_import:recorded_locals(Module),
  elixir_def_local:check_unused_local_macros(File, Recorded, Defmacrop),

  { FinalExport ++ Private, [
    {attribute, Line, export, lists:sort(FinalExport)} | FinalFunctions
  ] }.

%% Add attributes handling to the form

attributes_form(Line, _File, Module, Current) ->
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

%% Specs

specs_form(Line, Module, Defmacro, Defmacrop, Forms, C) ->
  case elixir_compiler:get_opt(internal, C) of
    true -> Forms;
    _    ->
      Callbacks = 'Elixir.Module':get_attribute(Module, callback),
      Specs     = [translate_spec(Spec, Defmacro, Defmacrop) ||
                    Spec <- 'Elixir.Module':get_attribute(Module, spec)],

      'Elixir.Module':delete_attribute(Module, spec),
      'Elixir.Module':delete_attribute(Module, callback),

      Temp = specs_attributes(Line, spec, Forms, Specs),
      specs_attributes(Line, callback, Temp, Callbacks)
  end.

specs_attributes(Line, Type, Forms, Specs) ->
  Keys = lists:ukeysort(1, Specs),
  lists:foldl(fun({ Tuple, _ }, Acc) ->
    Values = [V || { K, V } <- Specs, K == Tuple],
    [{ attribute, Line, Type, { Tuple, Values } }|Acc]
  end, Forms, Keys).

translate_spec({ Spec, Rest }, Defmacro, Defmacrop) ->
  case ordsets:is_element(Spec, Defmacrop) of
    true  -> { Spec, Rest };
    false ->
      case ordsets:is_element(Spec, Defmacro) of
        true ->
          { Name, Arity } = Spec,
          { { ?ELIXIR_MACRO(Name), Arity + 1 }, spec_for_macro(Rest) };
        false ->
          { Spec, Rest }
      end
  end.

spec_for_macro({ type, Line, 'fun', [{ type, _, product, Args }|T] }) ->
  NewArgs = [{type,Line,term,[]}|Args],
  { type, Line, 'fun', [{ type, Line, product, NewArgs }|T] };

spec_for_macro(Else) -> Else.

%% Loads the form into the code server.

load_form(Line, Forms, S) ->
  elixir_compiler:module(Forms, S, fun(Module, Binary) ->
    EvalS = scope_for_eval(Module, S),
    eval_callbacks(Line, Module, after_compile, [Module, Binary], EvalS),

    case get(elixir_compiled) of
      Current when is_list(Current) ->
        put(elixir_compiled, [{Module,Binary}|Current]),
        case get(elixir_compiler_pid) of
          undefined -> [];
          PID -> PID ! { module_available, self(), Module, Binary }
        end;
      _ -> []
    end,

    Binary
  end).

check_module_availability(Line, File, Module, Compiler) ->
  case elixir_compiler:get_opt(ignore_module_conflict, Compiler) of
    false ->
      case code:ensure_loaded(Module) of
        { module, _ } ->
          elixir_errors:handle_file_warning(File, { Line, ?MODULE, { module_defined, Module } });
        { error, _ } ->
          []
      end;
    true ->
      []
  end.

% EXTRA FUNCTIONS

add_info_function(Line, File, Module, Export, Functions, Def, Defmacro, C) ->
  Pair = { '__info__', 1 },
  case lists:member(Pair, Export) of
    true  -> elixir_errors:form_error(Line, File, ?MODULE, {internal_function_overridden, Pair});
    false ->
      Docs = elixir_compiler:get_opt(docs, C),
      Contents = { function, Line, '__info__', 1, [
        functions_clause(Line, Def),
        macros_clause(Line, Module, Def, Defmacro),
        docs_clause(Line, Module, Docs),
        moduledoc_clause(Line, Module, Docs),
        module_clause(Line, Module),
        else_clause(Line)
      ] },
      { [Pair|Export], [Contents|Functions] }
  end.

functions_clause(Line, Def) ->
  All = ordsets:add_element({'__info__',1}, Def),
  { clause, Line, [{ atom, Line, functions }], [], [elixir_tree_helpers:abstract_syntax(All)] }.

macros_clause(Line, Module, Def, Defmacro) ->
  All = handle_builtin_macros(Module, Def, Defmacro),
  { clause, Line, [{ atom, Line, macros }], [], [elixir_tree_helpers:abstract_syntax(All)] }.

handle_builtin_macros('Elixir.Kernel', Def, Defmacro) ->
  ordsets:subtract(ordsets:union(Defmacro,
    elixir_dispatch:in_erlang_macros()), Def);
handle_builtin_macros(_, _Def, Defmacro) -> Defmacro.

module_clause(Line, Module) ->
  { clause, Line, [{ atom, Line, module }], [], [{ atom, Line, Module }] }.

docs_clause(Line, Module, true) ->
  Docs = ordsets:from_list(ets:tab2list(docs_table(Module))),
  { clause, Line, [{ atom, Line, docs }], [], [elixir_tree_helpers:abstract_syntax(Docs)] };

docs_clause(Line, _Module, _) ->
  { clause, Line, [{ atom, Line, docs }], [], [{ atom, Line, nil }] }.

moduledoc_clause(Line, Module, true) ->
  Docs = 'Elixir.Module':get_attribute(Module, moduledoc),
  { clause, Line, [{ atom, Line, moduledoc }], [], [elixir_tree_helpers:abstract_syntax({ Line, Docs })] };

moduledoc_clause(Line, _Module, _) ->
  { clause, Line, [{ atom, Line, moduledoc }], [], [{ atom, Line, nil }] }.

else_clause(Line) ->
  Info = { call, Line, { atom, Line, module_info }, [{ var, Line, atom }] },
  { clause, Line, [{ var, Line, atom }], [], [Info] }.

% HELPERS

eval_callbacks(Line, Module, Name, Args, RawS) ->
  S         = RawS#elixir_scope{check_clauses=false},
  Binding   = binding_for_eval(Module, []),
  Callbacks = lists:reverse(ets:lookup_element(data_table(Module), Name, 2)),
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
        Info = { M, F, Args, [{ file, binary_to_list(S#elixir_scope.file) }, { line, Line }] },
        erlang:raise(Kind, Reason, [Info|erlang:get_stacktrace()])
    end
  end, Callbacks).

% ERROR HANDLING

format_error({ internal_function_overridden, { Name, Arity } }) ->
  io_lib:format("function ~s/~B is internal and should not be overridden", [Name, Arity]);

format_error({ invalid_module, Module}) ->
  io_lib:format("invalid module name: ~p", [Module]);

format_error({ module_defined, Module }) ->
  io_lib:format("redefining module ~s", [elixir_errors:inspect(Module)]);

format_error({ module_in_definition, Module }) ->
  io_lib:format("cannot define module ~s because it is currently being defined",
    [elixir_errors:inspect(Module)]).
