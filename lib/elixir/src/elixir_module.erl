-module(elixir_module).
-export([compile/4, data_table/1, docs_table/1, is_open/1,
         format_error/1, eval_callbacks/5]).
-include("elixir.hrl").

-define(acc_attr, '__acc_attributes').
-define(docs_attr, '__docs_table').
-define(lexical_attr, '__lexical_tracker').
-define(persisted_attr, '__persisted_attributes').
-define(overridable_attr, '__overridable').

%% TABLE METHODS

data_table(Module) ->
  Module.

docs_table(Module) ->
  ets:lookup_element(Module, ?docs_attr, 2).

is_open(Module) ->
  Module == ets:info(Module, name).

%% Compilation hook

compile(Module, Block, Vars, #elixir_env{line=Line} = Env) when is_atom(Module) ->
  %% In case we are generating a module from inside a function,
  %% we get rid of the lexical tracker information as, at this
  %% point, the lexical tracker process is long gone.
  LexEnv = case Env#elixir_env.function of
    nil -> Env#elixir_env{module=Module, local=nil};
    _   -> Env#elixir_env{lexical_tracker=nil, function=nil, module=Module, local=nil}
  end,

  case LexEnv#elixir_env.lexical_tracker of
    nil ->
      elixir_lexical:run(LexEnv#elixir_env.file, fun(Pid) ->
        do_compile(Line, Module, Block, Vars, LexEnv#elixir_env{lexical_tracker=Pid})
      end);
    _ ->
      do_compile(Line, Module, Block, Vars, LexEnv)
  end;

compile(Module, _Block, _Vars, #elixir_env{line=Line,file=File}) ->
  elixir_errors:form_error(Line, File, ?MODULE, { invalid_module, Module });

compile(Module, Block, Vars, ExEnv) ->
  compile(Module, Block, Vars, elixir_env:ex_to_env(ExEnv)).

do_compile(Line, Module, Block, Vars, E) ->
  File = E#elixir_env.file,
  check_module_availability(Line, File, Module),
  build(Line, File, Module, E#elixir_env.lexical_tracker),

  try
    { Result, NE } = eval_form(Line, Module, Block, Vars, E),
    { Base, Export, Private, Def, Defmacro, Functions } = elixir_def:unwrap_definitions(Module),

    { All, Forms0 } = functions_form(Line, File, Module, Base, Export, Def, Defmacro, Functions),
    Forms1          = specs_form(Module, Private, Defmacro, Forms0),
    Forms2          = attributes_form(Line, File, Module, Forms1),
    Forms3          = typedocs_form(Module, Forms2),

    case ets:lookup(data_table(Module), 'on_load') of
      [] -> ok;
      [{on_load,OnLoad}] ->
        [elixir_locals:record_local(Tuple, Module) || Tuple <- OnLoad]
    end,

    AllFunctions = Def ++ [T || { T, defp, _, _, _ } <- Private],
    elixir_locals:ensure_no_function_conflict(Line, File, Module, AllFunctions),
    elixir_locals:warn_unused_local(File, Module, Private),
    warn_invalid_clauses(Line, File, Module, All),
    warn_unused_docs(Line, File, Module),

    Location = { elixir_utils:relative_to_cwd(elixir_utils:characters_to_list(File)), Line },

    Final = [
      { attribute, Line, file, Location },
      { attribute, Line, module, Module } | Forms3
    ],

    Binary = load_form(Line, Final, compile_opts(Module), NE),
    { module, Module, Binary, Result }
  after
    elixir_locals:cleanup(Module),
    elixir_def:cleanup(Module),
    ets:delete(docs_table(Module)),
    ets:delete(data_table(Module))
  end.

%% Hook that builds both attribute and functions and set up common hooks.

build(Line, File, Module, Lexical) ->
  %% Table with meta information about the module.
  DataTable = data_table(Module),

  case ets:info(DataTable, name) == DataTable of
    true  -> elixir_errors:form_error(Line, File, ?MODULE, { module_in_definition, Module });
    false -> []
  end,

  ets:new(DataTable, [set, named_table, public]),
  ets:insert(DataTable, { before_compile, [] }),
  ets:insert(DataTable, { after_compile, [] }),

  case elixir_compiler:get_opt(docs) of
    true -> ets:insert(DataTable, { on_definition, [{ 'Elixir.Module', compile_doc }] });
    _    -> ets:insert(DataTable, { on_definition, [] })
  end,

  Attributes = [behaviour, on_load, spec, type, export_type, opaque, callback, compile],
  ets:insert(DataTable, { ?acc_attr, [before_compile, after_compile, on_definition|Attributes] }),
  ets:insert(DataTable, { ?persisted_attr, [vsn|Attributes] }),
  ets:insert(DataTable, { ?docs_attr, ets:new(DataTable, [ordered_set, public]) }),
  ets:insert(DataTable, { ?lexical_attr, Lexical }),
  ets:insert(DataTable, { ?overridable_attr, [] }),

  %% Setup other modules
  elixir_def:setup(Module),
  elixir_locals:setup(Module).

%% Receives the module representation and evaluates it.

eval_form(Line, Module, Block, Vars, E) ->
  { Value, EE } = elixir_compiler:eval_forms(Block, Vars, E),
  elixir_def_overridable:store_pending(Module),
  EC = eval_callbacks(Line, Module, before_compile, [elixir_env:env_to_ex({ Line, EE })], EE),
  elixir_def_overridable:store_pending(Module),
  { Value, EC }.

%% Return the form with exports and function declarations.

functions_form(Line, File, Module, BaseAll, BaseExport, Def, Defmacro, BaseFunctions) ->
  Info = add_info_function(Line, File, Module, BaseExport, Def, Defmacro),

  All       = [{ '__info__', 1 }|BaseAll],
  Export    = [{ '__info__', 1 }|BaseExport],
  Functions = [Info|BaseFunctions],

  { All, [
    { attribute, Line, export, lists:sort(Export) } | Functions
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

%% Add typedocs to the form
typedocs_form(Module, Current) ->
  Table = docs_table(Module),
  Transform = fun({ Tuple, Line, Kind, _Sig, Doc }, Acc) ->
    case Kind of
      type      -> [{ attribute, Line, typedoc, { Tuple, Doc } } | Acc];
      opaque    -> [{ attribute, Line, typedoc, { Tuple, Doc } } | Acc];
      _         -> Acc
    end
  end,
  ets:foldl(Transform, Current, Table).

%% Specs

specs_form(Module, Private, Defmacro, Forms) ->
  Defmacrop = [Tuple || { Tuple, defmacrop, _, _, _ } <- Private],
  case code:ensure_loaded('Elixir.Kernel.Typespec') of
    { module, 'Elixir.Kernel.Typespec' } ->
      Callbacks = 'Elixir.Module':get_attribute(Module, callback),
      Specs     = [translate_spec(Spec, Defmacro, Defmacrop) ||
                    Spec <- 'Elixir.Module':get_attribute(Module, spec)],

      'Elixir.Module':delete_attribute(Module, spec),
      'Elixir.Module':delete_attribute(Module, callback),

      Temp = specs_attributes(spec, Forms, Specs),
      specs_attributes(callback, Temp, Callbacks);
    { error, _ } ->
      Forms
  end.

specs_attributes(Type, Forms, Specs) ->
  Keys = lists:foldl(fun({ Tuple, Value }, Acc) ->
                       lists:keystore(Tuple, 1, Acc, { Tuple, Value })
                     end, [], Specs),
  lists:foldl(fun({ Tuple, _ }, Acc) ->
    Values = [V || { K, V } <- Specs, K == Tuple],
    { type, Line, _, _ } = hd(Values),
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

compile_opts(Module) ->
  case ets:lookup(data_table(Module), compile) of
    [{compile,Opts}] when is_list(Opts) -> Opts;
    [] -> []
  end.

load_form(Line, Forms, Opts, #elixir_env{file=File} = E) ->
  elixir_compiler:module(Forms, File, Opts, fun(Module, Binary) ->
    Env = elixir_env:env_to_ex({ Line, E }),
    eval_callbacks(Line, Module, after_compile, [Env, Binary], E),

    case get(elixir_compiled) of
      Current when is_list(Current) ->
        put(elixir_compiled, [{Module,Binary}|Current]),

        case get(elixir_compiler_pid) of
          undefined -> [];
          PID ->
            Ref = make_ref(),
            PID ! { module_available, self(), Ref, File, Module, Binary },
            receive { Ref, ack } -> ok end
        end;
      _ ->
        []
    end,

    Binary
  end).

check_module_availability(Line, File, Module) ->
  Reserved = ['Elixir.Atom', 'Elixir.BitString', 'Elixir.Function',
              'Elixir.PID', 'Elixir.Reference', 'Elixir.Any'],

  case lists:member(Module, Reserved) of
    true  -> elixir_errors:handle_file_error(File, { Line, ?MODULE, { module_reserved, Module } });
    false -> ok
  end,

  case elixir_compiler:get_opt(ignore_module_conflict) of
    false ->
      case code:ensure_loaded(Module) of
        { module, _ } ->
          elixir_errors:handle_file_warning(File, { Line, ?MODULE, { module_defined, Module } });
        { error, _ }  ->
          ok
      end;
    true ->
      ok
  end.

warn_invalid_clauses(_Line, _File, 'Elixir.Kernel.SpecialForms', _All) -> ok;
warn_invalid_clauses(_Line, File, Module, All) ->
  ets:foldl(fun
    ({ _, _, Kind, _, _ }, _) when Kind == type; Kind == opaque ->
      ok;
    ({ Tuple, Line, _, _, _ }, _) ->
      case lists:member(Tuple, All) of
        false ->
          elixir_errors:handle_file_warning(File, { Line, ?MODULE, { invalid_clause, Tuple } });
        true ->
          ok
      end
  end, ok, docs_table(Module)).

warn_unused_docs(Line, File, Module) ->
  lists:foreach(fun(Attribute) ->
    case ets:member(data_table(Module), Attribute) of
      true ->
        elixir_errors:handle_file_warning(File, { Line, ?MODULE, { unused_doc, Attribute } });
      _ ->
        ok
    end
  end, [typedoc]).

% EXTRA FUNCTIONS

add_info_function(Line, File, Module, Export, Def, Defmacro) ->
  Pair = { '__info__', 1 },
  case lists:member(Pair, Export) of
    true  ->
      elixir_errors:form_error(Line, File, ?MODULE, {internal_function_overridden, Pair});
    false ->
      Docs = elixir_compiler:get_opt(docs),
      { function, 0, '__info__', 1, [
        functions_clause(Def),
        macros_clause(Defmacro),
        docs_clause(Module, Docs),
        moduledoc_clause(Line, Module, Docs),
        module_clause(Module),
        else_clause()
      ] }
  end.

functions_clause(Def) ->
  { clause, 0, [{ atom, 0, functions }], [], [elixir_utils:elixir_to_erl(Def)] }.

macros_clause(Defmacro) ->
  { clause, 0, [{ atom, 0, macros }], [], [elixir_utils:elixir_to_erl(Defmacro)] }.

module_clause(Module) ->
  { clause, 0, [{ atom, 0, module }], [], [{ atom, 0, Module }] }.

docs_clause(Module, true) ->
  Docs = ordsets:from_list(
    [{Tuple, Line, Kind, Sig, Doc} ||
     {Tuple, Line, Kind, Sig, Doc} <- ets:tab2list(docs_table(Module)),
     Kind =/= type, Kind =/= opaque]),
  { clause, 0, [{ atom, 0, docs }], [], [elixir_utils:elixir_to_erl(Docs)] };

docs_clause(_Module, _) ->
  { clause, 0, [{ atom, 0, docs }], [], [{ atom, 0, nil }] }.

moduledoc_clause(Line, Module, true) ->
  Docs = 'Elixir.Module':get_attribute(Module, moduledoc),
  { clause, 0, [{ atom, 0, moduledoc }], [], [elixir_utils:elixir_to_erl({ Line, Docs })] };

moduledoc_clause(_Line, _Module, _) ->
  { clause, 0, [{ atom, 0, moduledoc }], [], [{ atom, 0, nil }] }.

else_clause() ->
  Info = { call, 0, { atom, 0, module_info }, [{ var, 0, atom }] },
  { clause, 0, [{ var, 0, atom }], [], [Info] }.

% HELPERS

eval_callbacks(Line, Module, Name, Args, E) ->
  Callbacks = lists:reverse(ets:lookup_element(data_table(Module), Name, 2)),
  Meta      = [{line,Line},{require,false}],

  lists:foldl(fun({M,F}, Acc) ->
    { Expr, ET } = elixir_dispatch:dispatch_require(Meta, M, F, Args, Acc, fun(AM, AF, AA) ->
      apply(AM, AF, AA),
      { nil, Acc }
    end),

    if
      is_atom(Expr) ->
        ET;
      true ->
        try
          { _Value, _Binding, EE, _S } = elixir:eval_forms(Expr, [], ET),
          EE
        catch
          Kind:Reason ->
            Info = { M, F, length(Args), location(Line, E) },
            erlang:raise(Kind, Reason, prune_stacktrace(Info, erlang:get_stacktrace()))
        end
    end
  end, E, Callbacks).

location(Line, E) ->
  [{ file, elixir_utils:characters_to_list(E#elixir_env.file) }, { line, Line }].

%% We've reached the elixir_module or eval internals, skip it with the rest
prune_stacktrace(Info, [{ elixir, eval_forms, _, _ }|_]) ->
  [Info];
prune_stacktrace(Info, [{ elixir_module, _, _, _ }|_]) ->
  [Info];
prune_stacktrace(Info, [H|T]) ->
  [H|prune_stacktrace(Info, T)];
prune_stacktrace(Info, []) ->
  [Info].

% ERROR HANDLING

format_error({ invalid_clause, { Name, Arity } }) ->
  io_lib:format("empty clause provided for nonexistent function or macro ~ts/~B", [Name, Arity]);
format_error({ unused_doc, typedoc }) ->
  "@typedoc provided but no type follows it";
format_error({ unused_doc, doc }) ->
  "@doc provided but no definition follows it";
format_error({ internal_function_overridden, { Name, Arity } }) ->
  io_lib:format("function ~ts/~B is internal and should not be overridden", [Name, Arity]);
format_error({ invalid_module, Module}) ->
  io_lib:format("invalid module name: ~p", [Module]);
format_error({ module_defined, Module }) ->
  io_lib:format("redefining module ~ts", [elixir_aliases:inspect(Module)]);
format_error({ module_reserved, Module }) ->
  io_lib:format("module ~ts is reserved and cannot be defined", [elixir_aliases:inspect(Module)]);
format_error({ module_in_definition, Module }) ->
  io_lib:format("cannot define module ~ts because it is currently being defined",
    [elixir_aliases:inspect(Module)]).
