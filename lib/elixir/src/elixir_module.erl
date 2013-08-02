-module(elixir_module).
-export([translate/4, compile/5, data_table/1, docs_table/1,
         eval_quoted/4, format_error/1, eval_callbacks/5]).
-include("elixir.hrl").

-define(docs_attr, '__docs_table').
-define(acc_attr, '__acc_attributes').
-define(persisted_attr, '__persisted_attributes').

eval_quoted(Module, Quoted, RawBinding, Opts) ->
  Binding = elixir_scope:binding_for_eval(RawBinding, Module),
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

%% TABLE METHODS

data_table(Module) ->
  Module.

docs_table(Module) ->
  ets:lookup_element(Module, ?docs_attr, 2).

%% TRANSFORMATION FUNCTIONS

%% Transformation of args and scope into a compiled erlang call.
%% The abstract form for extra arguments may be given and they
%% will be passed to the invoked function.

translate(Meta, Ref, Block, S) ->
  Line            = ?line(Meta),
  MetaBlock       = elixir_tree_helpers:elixir_to_erl(Block),
  { MetaS, Vars } = elixir_scope:serialize_with_vars(Line, S),

  Args = [{integer, Line, Line}, Ref, MetaBlock, Vars, MetaS],
  ?wrap_call(Line, ?MODULE, compile, Args).

%% The compilation hook.

compile(Line, Module, Block, Vars, #elixir_scope{context_modules=FileModules} = RawS) when is_atom(Module) ->
  C = elixir_compiler:get_opts(),
  S = case lists:member(Module, FileModules) of
    true  -> RawS;
    false -> RawS#elixir_scope{context_modules=[Module|FileModules]}
  end,

  File = S#elixir_scope.file,
  FileList = binary_to_list(File),

  check_module_availability(Line, File, Module, C),
  build(Line, File, Module),

  try
    Result = eval_form(Line, Module, Block, Vars, S),
    { Base, Export, Private, Def, Defmacro, Functions } = elixir_def:unwrap_stored_definitions(FileList, Module),

    { All, Forms0 } = functions_form(Line, File, Module, Base, Export, Def, Defmacro, Functions, C),
    Forms1          = specs_form(Line, Module, Private, Defmacro, Forms0, C),
    Forms2          = attributes_form(Line, File, Module, Forms1),

    case ets:lookup(data_table(Module), 'on_load') of
      [] -> ok;
      [{on_load,OnLoad}] ->
        [elixir_tracker:record_local(Tuple, Module) || Tuple <- OnLoad]
    end,

    AllFunctions = Def ++ [T || { T, defp, _, _, _ } <- Private],
    elixir_tracker:ensure_no_function_conflict(Line, File, Module, AllFunctions),
    elixir_tracker:ensure_all_imports_used(Line, File, Module),
    elixir_tracker:warn_unused_local(File, Module, Private),
    warn_unused_docs(Line, File, Module, All),

    Final = [
      { attribute, Line, file, { FileList, Line } },
      { attribute, Line, module, Module } | Forms2
    ],

    Binary = load_form(Line, Final, S),
    { module, Module, Binary, Result }
  after
    elixir_tracker:cleanup(Module),
    elixir_def:cleanup(Module),
    ets:delete(docs_table(Module)),
    ets:delete(data_table(Module))
  end;

compile(Line, Other, _Block, _Vars, #elixir_scope{file=File}) ->
  elixir_errors:form_error(Line, File, ?MODULE, { invalid_module, Other });

compile(Line, Module, Block, Vars, RawS) ->
  Dict = [{ { Name, Kind }, Value } || { Name, Kind, Value, _ } <- Vars],
  S = elixir_scope:deserialize_with_vars(RawS, Dict),
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
  ets:insert(DataTable, { ?acc_attr, [before_compile,after_compile,on_definition|Attributes] }),
  ets:insert(DataTable, { ?persisted_attr, [vsn|Attributes] }),
  ets:insert(DataTable, { ?docs_attr, ets:new(DataTable, [ordered_set, public]) }),

  %% Setup other modules
  elixir_def:setup(Module),
  elixir_tracker:setup(Module).

%% Receives the module representation and evaluates it.

eval_form(Line, Module, Block, Vars, RawS) ->
  S = scope_for_eval(Module, RawS),
  { Value, NewS } = elixir_compiler:eval_forms([Block], Line, Vars, S),
  elixir_def_overridable:store_pending(Module),
  Env = elixir_scope:to_ex_env({ Line, S }),
  eval_callbacks(Line, Module, before_compile, [Env], NewS),
  elixir_def_overridable:store_pending(Module),
  Value.

%% Return the form with exports and function declarations.

functions_form(Line, File, Module, BaseAll, BaseExport, Def, Defmacro, RawFunctions, C) ->
  BaseFunctions = case elixir_compiler:get_opt(internal, C) of
    true  -> RawFunctions;
    false -> record_rewrite_functions(Module, RawFunctions)
  end,

  Info = add_info_function(Line, File, Module, BaseExport, Def, Defmacro, C),

  All       = [{ '__info__', 1 }|BaseAll],
  Export    = [{ '__info__', 1 }|BaseExport],
  Functions = [Info|BaseFunctions],

  { All, [
    { attribute, Line, export, lists:sort(Export) } | Functions
  ] }.

record_rewrite_functions(Module, Functions) ->
  lists:map(fun
    ({ function, Line, Name, Arity, Clauses }) ->
      Rewriten = [begin
        { C, _, _ } = 'Elixir.Kernel.RecordRewriter':optimize_clause(Module, Clause),
        C
      end || Clause <- Clauses],
      { function, Line, Name, Arity, Rewriten };
    (Other) -> Other
  end, Functions).

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

specs_form(Line, Module, Private, Defmacro, Forms, C) ->
  Defmacrop = [Tuple || { Tuple, defmacrop, _, _, _ } <- Private],
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
  Keys = lists:foldl(fun({ Tuple, Value }, Acc) ->
                       lists:keystore(Tuple, 1, Acc, { Tuple, Value } )
                     end, [], Specs),
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
          { { ?elixir_macro(Name), Arity + 1 }, spec_for_macro(Rest) };
        false ->
          { Spec, Rest }
      end
  end.

spec_for_macro({ type, Line, 'fun', [{ type, _, product, Args }|T] }) ->
  NewArgs = [{type,Line,term,[]}|Args],
  { type, Line, 'fun', [{ type, Line, product, NewArgs }|T] };

spec_for_macro(Else) -> Else.

%% Loads the form into the code server.

load_form(Line, Forms, #elixir_scope{file=File} = S) ->
  elixir_compiler:module(Forms, File, fun(Module, Binary) ->
    EvalS = scope_for_eval(Module, S),
    Env = elixir_scope:to_ex_env({ Line, EvalS }),
    eval_callbacks(Line, Module, after_compile, [Env, Binary], EvalS),

    case get(elixir_compiled) of
      Current when is_list(Current) ->
        put(elixir_compiled, [{Module,Binary}|Current]),

        case get(elixir_compiler_pid) of
          undefined -> [];
          PID ->
            PID ! { module_available, self(), File, Module, Binary },
            receive { PID, ack } -> ok end
        end;
      _ ->
        []
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

warn_unused_docs(_Line, _File, 'Elixir.Kernel', _All) -> ok;
warn_unused_docs(_Line, _File, 'Elixir.Kernel.SpecialForms', _All) -> ok;
warn_unused_docs(_Line, File, Module, All) ->
  ets:foldl(fun
    ({ Tuple, Line, _, _, _ }, Acc) ->
      case lists:member(Tuple, All) of
        true  -> Acc;
        false ->
          elixir_errors:handle_file_warning(File, { Line, ?MODULE, { invalid_doc, Tuple } })
      end
  end, ok, docs_table(Module)).

% EXTRA FUNCTIONS

add_info_function(Line, File, Module, Export, Def, Defmacro, C) ->
  Pair = { '__info__', 1 },
  case lists:member(Pair, Export) of
    true  ->
      elixir_errors:form_error(Line, File, ?MODULE, {internal_function_overridden, Pair});
    false ->
      Docs = elixir_compiler:get_opt(docs, C),
      { function, 0, '__info__', 1, [
        functions_clause(Def),
        macros_clause(Module, Def, Defmacro),
        docs_clause(Module, Docs),
        moduledoc_clause(Line, Module, Docs),
        module_clause(Module),
        else_clause()
      ] }
  end.

functions_clause(Def) ->
  { clause, 0, [{ atom, 0, functions }], [], [elixir_tree_helpers:elixir_to_erl(Def)] }.

macros_clause(Module, Def, Defmacro) ->
  All = handle_builtin_macros(Module, Def, Defmacro),
  { clause, 0, [{ atom, 0, macros }], [], [elixir_tree_helpers:elixir_to_erl(All)] }.

handle_builtin_macros('Elixir.Kernel', Def, Defmacro) ->
  ordsets:subtract(ordsets:union(Defmacro,
    elixir_dispatch:in_erlang_macros()), Def);
handle_builtin_macros(_, _Def, Defmacro) -> Defmacro.

module_clause(Module) ->
  { clause, 0, [{ atom, 0, module }], [], [{ atom, 0, Module }] }.

docs_clause(Module, true) ->
  Docs = ordsets:from_list(ets:tab2list(docs_table(Module))),
  { clause, 0, [{ atom, 0, docs }], [], [elixir_tree_helpers:elixir_to_erl(Docs)] };

docs_clause(_Module, _) ->
  { clause, 0, [{ atom, 0, docs }], [], [{ atom, 0, nil }] }.

moduledoc_clause(Line, Module, true) ->
  Docs = 'Elixir.Module':get_attribute(Module, moduledoc),
  { clause, 0, [{ atom, 0, moduledoc }], [], [elixir_tree_helpers:elixir_to_erl({ Line, Docs })] };

moduledoc_clause(_Line, _Module, _) ->
  { clause, 0, [{ atom, 0, moduledoc }], [], [{ atom, 0, nil }] }.

else_clause() ->
  Info = { call, 0, { atom, 0, module_info }, [{ var, 0, atom }] },
  { clause, 0, [{ var, 0, atom }], [], [Info] }.

% HELPERS

eval_callbacks(Line, Module, Name, Args, RawS) ->
  Binding   = elixir_scope:binding_for_eval([], Module),
  S         = elixir_scope:vars_from_binding(RawS, Binding),
  Callbacks = lists:reverse(ets:lookup_element(data_table(Module), Name, 2)),
  Meta      = [{line,Line},{require,false}],

  lists:foreach(fun({M,F}) ->
    { Tree, _ } = elixir_dispatch:dispatch_require(Meta, M, F, Args, S, fun() ->
      apply(M, F, Args),
      { { atom, 0, nil }, S }
    end),

    case Tree of
      { atom, _, Atom } ->
        Atom;
      _ ->
        try
          erl_eval:exprs([Tree], Binding)
        catch
          Kind:Reason ->
            Info = { M, F, length(Args), [{ file, binary_to_list(S#elixir_scope.file) }, { line, Line }] },
            erlang:raise(Kind, Reason, munge_stacktrace(Info, erlang:get_stacktrace()))
        end
    end
  end, Callbacks).

%% We've reached the elixir_dispatch internals, skip it with the rest
munge_stacktrace(Info, [{ elixir_module, _, _, _ }|_]) ->
  [Info];

munge_stacktrace(Info, [H|T]) ->
  [H|munge_stacktrace(Info, T)];

munge_stacktrace(Info, []) ->
  [Info].

% ERROR HANDLING

format_error({ invalid_doc, { Name, Arity } }) ->
  io_lib:format("docs provided for nonexistent function or macro ~ts/~B", [Name, Arity]);

format_error({ internal_function_overridden, { Name, Arity } }) ->
  io_lib:format("function ~ts/~B is internal and should not be overridden", [Name, Arity]);

format_error({ invalid_module, Module}) ->
  io_lib:format("invalid module name: ~p", [Module]);

format_error({ module_defined, Module }) ->
  io_lib:format("redefining module ~ts", [elixir_errors:inspect(Module)]);

format_error({ module_in_definition, Module }) ->
  io_lib:format("cannot define module ~ts because it is currently being defined",
    [elixir_errors:inspect(Module)]).
