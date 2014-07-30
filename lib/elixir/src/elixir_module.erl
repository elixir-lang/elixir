-module(elixir_module).
-export([compile/4, data_table/1, docs_table/1, is_open/1,
         expand_callback/6, add_beam_chunk/3, format_error/1]).
-include("elixir.hrl").

-define(acc_attr, '__acc_attributes').
-define(docs_attr, '__docs_table').
-define(lexical_attr, '__lexical_tracker').
-define(persisted_attr, '__persisted_attributes').
-define(overridable_attr, '__overridable').
-define(location_attr, '__location').

%% TABLE METHODS

data_table(Module) ->
  Module.

docs_table(Module) ->
  ets:lookup_element(Module, ?docs_attr, 2).

is_open(Module) ->
  Module == ets:info(Module, name).

%% Compilation hook

compile(Module, Block, Vars, #{line := Line} = Env) when is_atom(Module) ->
  %% In case we are generating a module from inside a function,
  %% we get rid of the lexical tracker information as, at this
  %% point, the lexical tracker process is long gone.
  LexEnv = case ?m(Env, function) of
    nil -> Env#{module := Module, local := nil};
    _   -> Env#{lexical_tracker := nil, function := nil, module := Module, local := nil}
  end,

  case ?m(LexEnv, lexical_tracker) of
    nil ->
      elixir_lexical:run(?m(LexEnv, file), fun(Pid) ->
        do_compile(Line, Module, Block, Vars, LexEnv#{lexical_tracker := Pid})
      end);
    _ ->
      do_compile(Line, Module, Block, Vars, LexEnv)
  end;

compile(Module, _Block, _Vars, #{line := Line, file := File}) ->
  elixir_errors:form_error([{line, Line}], File, ?MODULE, {invalid_module, Module}).

do_compile(Line, Module, Block, Vars, E) ->
  File = ?m(E, file),
  check_module_availability(Line, File, Module),

  Docs = elixir_compiler:get_opt(docs),
  build(Line, File, Module, Docs, ?m(E, lexical_tracker)),

  try
    {Result, NE} = eval_form(Line, Module, Block, Vars, E),

    _ = case ets:lookup(data_table(Module), 'on_load') of
      [] -> ok;
      [{on_load,OnLoad}] ->
        [elixir_locals:record_local(Tuple, Module) || Tuple <- OnLoad]
    end,

    {Def, Defp, Defmacro, Defmacrop, Exports, Functions} =
      elixir_def:unwrap_definitions(File, Module),

    {All, Forms0} = functions_form(Line, File, Module, Def, Defp,
                                   Defmacro, Defmacrop, Exports, Functions),
    Forms1        = specs_form(Module, Defmacro, Defmacrop, Forms0),
    Forms2        = types_form(Line, File, Module, Forms1),
    Forms3        = attributes_form(Line, File, Module, Forms2),

    elixir_locals:ensure_no_import_conflict(Line, File, Module, All),

    case Docs of
      true  -> warn_unused_docs(Line, File, Module, doc);
      false -> false
    end,

    Location = {elixir_utils:characters_to_list(elixir_utils:relative_to_cwd(File)), Line},

    Final = [
      {attribute, Line, file, Location},
      {attribute, Line, module, Module} | Forms3
    ],

    Binary = load_form(Line, Final, compile_opts(Module), NE),
    {module, Module, Binary, Result}
  after
    elixir_locals:cleanup(Module),
    elixir_def:cleanup(Module),
    ets:delete(docs_table(Module)),
    ets:delete(data_table(Module))
  end.

%% Hook that builds both attribute and functions and set up common hooks.

build(Line, File, Module, Docs, Lexical) ->
  %% Table with meta information about the module.
  DataTable = data_table(Module),

  OldTable = ets:info(DataTable, name),
  case OldTable == DataTable of
    true ->
      [{OldFile, OldLine}] = ets:lookup_element(OldTable, ?location_attr, 2),
      Error = {module_in_definition, Module, OldFile, OldLine},
      elixir_errors:form_error([{line, Line}], File, ?MODULE, Error);
    false ->
      []
  end,

  DataTable = ets:new(DataTable, [set, named_table, public]),
  ets:insert(DataTable, {before_compile, []}),
  ets:insert(DataTable, {after_compile, []}),

  case Docs of
    true -> ets:insert(DataTable, {on_definition, [{'Elixir.Module', compile_doc}]});
    _    -> ets:insert(DataTable, {on_definition, []})
  end,

  Attributes = [behaviour, on_load, spec, type, typep, opaque, callback, compile, external_resource],
  ets:insert(DataTable, {?acc_attr, [before_compile, after_compile, on_definition, derive|Attributes]}),
  ets:insert(DataTable, {?persisted_attr, [vsn|Attributes]}),
  ets:insert(DataTable, {?docs_attr, ets:new(DataTable, [ordered_set, public])}),
  ets:insert(DataTable, {?lexical_attr, Lexical}),
  ets:insert(DataTable, {?overridable_attr, []}),
  ets:insert(DataTable, {?location_attr, [{File, Line}]}),

  %% Setup other modules
  elixir_def:setup(Module),
  elixir_locals:setup(Module),
  ok.

%% Receives the module representation and evaluates it.

eval_form(Line, Module, Block, Vars, E) ->
  {Value, EE} = elixir_compiler:eval_forms(Block, Vars, E),
  elixir_def_overridable:store_pending(Module),
  EV = elixir_env:linify({Line, EE#{vars := [], export_vars := nil}}),
  EC = eval_callbacks(Line, Module, before_compile, [EV], EV),
  elixir_def_overridable:store_pending(Module),
  {Value, EC}.

eval_callbacks(Line, Module, Name, Args, E) ->
  Callbacks = lists:reverse(ets:lookup_element(data_table(Module), Name, 2)),

  lists:foldl(fun({M,F}, Acc) ->
    expand_callback(Line, M, F, Args, Acc#{vars := [], export_vars := nil},
                    fun(AM, AF, AA) -> apply(AM, AF, AA) end)
  end, E, Callbacks).

%% Return the form with exports and function declarations.

functions_form(Line, File, Module, Def, Defp, Defmacro, Defmacrop, Exports, Body) ->
  All = Def ++ Defmacro ++ Defp ++ Defmacrop,
  {Spec, Info} = add_info_function(Line, File, Module, All, Def, Defmacro),

  {[{'__info__',1}|All],
   [{attribute, Line, export, lists:sort([{'__info__',1}|Exports])},
    Spec, Info | Body]}.

%% Add attributes handling to the form

attributes_form(Line, File, Module, Current) ->
  Table = data_table(Module),

  AccAttrs = ets:lookup_element(Table, '__acc_attributes', 2),
  PersistedAttrs = ets:lookup_element(Table, '__persisted_attributes', 2),

  Transform = fun({Key, Value}, Acc) ->
    case lists:member(Key, PersistedAttrs) of
      false -> Acc;
      true  ->
        Values =
          case lists:member(Key, AccAttrs) of
            true  -> Value;
            false -> [Value]
          end,

        lists:foldl(fun(X, Final) ->
          [{attribute, Line, Key, X}|Final]
        end, Acc, process_attribute(Line, File, Key, Values))
    end
  end,

  ets:foldl(Transform, Current, Table).

process_attribute(Line, File, external_resource, Values) ->
  lists:usort([process_external_resource(Line, File, Value) || Value <- Values]);
process_attribute(_Line, _File, _Key, Values) ->
  Values.

process_external_resource(_Line, _File, Value) when is_binary(Value) ->
  Value;
process_external_resource(Line, File, Value) ->
  elixir_errors:form_error([{line, Line}], File,
    ?MODULE, {invalid_external_resource, Value}).

%% Types

types_form(Line, File, Module, Forms0) ->
  case code:ensure_loaded('Elixir.Kernel.Typespec') of
    {module, 'Elixir.Kernel.Typespec'} ->
      Types0 = 'Elixir.Module':get_attribute(Module, type) ++
               'Elixir.Module':get_attribute(Module, typep) ++
               'Elixir.Module':get_attribute(Module, opaque),

      Types1 = ['Elixir.Kernel.Typespec':translate_type(Kind, Expr, Doc, Caller) ||
                {Kind, Expr, Doc, Caller} <- Types0],

      'Elixir.Module':delete_attribute(Module, type),
      'Elixir.Module':delete_attribute(Module, typep),
      'Elixir.Module':delete_attribute(Module, opaque),

      warn_unused_docs(Line, File, Module, typedoc),

      Forms1 = types_attributes(Types1, Forms0),
      Forms2 = export_types_attributes(Types1, Forms1),
      typedocs_attributes(Types1, Forms2);

    {error, _} ->
      Forms0
  end.

types_attributes(Types, Forms) ->
  Fun = fun({{Kind, _NameArity, Expr}, Line, _Export, _Doc}, Acc) ->
    [{attribute, Line, Kind, Expr}|Acc]
  end,
  lists:foldl(Fun, Forms, Types).

export_types_attributes(Types, Forms) ->
  Fun = fun
    ({{_Kind, NameArity, _Expr}, Line, true, _Doc}, Acc) ->
      [{attribute, Line, export_type, [NameArity]}|Acc];
    ({_Type, _Line, false, _Doc}, Acc) ->
      Acc
  end,
  lists:foldl(Fun, Forms, Types).

typedocs_attributes(Types, Forms) ->
  Fun = fun
    ({{_Kind, NameArity, _Expr}, Line, true, Doc}, Acc) when Doc =/= nil ->
      [{attribute, Line, typedoc, {NameArity, Doc}}|Acc];
    ({_Type, _Line, _Export, _Doc}, Acc) ->
      Acc
  end,
  lists:foldl(Fun, Forms, Types).

%% Specs

specs_form(Module, Defmacro, Defmacrop, Forms) ->
  case code:ensure_loaded('Elixir.Kernel.Typespec') of
    {module, 'Elixir.Kernel.Typespec'} ->
      Specs0 = 'Elixir.Module':get_attribute(Module, spec) ++
               'Elixir.Module':get_attribute(Module, callback),

      Specs1 = ['Elixir.Kernel.Typespec':translate_spec(Kind, Expr, Caller) ||
                {Kind, Expr, Caller} <- Specs0],
      Specs2 = lists:flatmap(fun(Spec) ->
                               translate_macro_spec(Spec, Defmacro, Defmacrop)
                             end, Specs1),

      'Elixir.Module':delete_attribute(Module, spec),
      'Elixir.Module':delete_attribute(Module, callback),
      specs_attributes(Forms, Specs2);

    {error, _} ->
      Forms
  end.

specs_attributes(Forms, Specs) ->
  Dict = lists:foldl(fun({{Kind, NameArity, Spec}, Line}, Acc) ->
                       dict:append({Kind, NameArity}, {Spec, Line}, Acc)
                     end, dict:new(), Specs),
  dict:fold(fun({Kind, NameArity}, ExprsLines, Acc) ->
    {Exprs, Lines} = lists:unzip(ExprsLines),
    Line = lists:min(Lines),
    [{attribute, Line, Kind, {NameArity, Exprs}}|Acc]
  end, Forms, Dict).

translate_macro_spec({{spec, NameArity, Spec}, Line}, Defmacro, Defmacrop) ->
  case lists:member(NameArity, Defmacrop) of
    true  -> [];
    false ->
      case lists:member(NameArity, Defmacro) of
        true ->
          {Name, Arity} = NameArity,
          [{{spec, {elixir_utils:macro_name(Name), Arity + 1}, spec_for_macro(Spec)}, Line}];
        false ->
          [{{spec, NameArity, Spec}, Line}]
      end
  end;

translate_macro_spec({{callback, NameArity, Spec}, Line}, _Defmacro, _Defmacrop) ->
  [{{callback, NameArity, Spec}, Line}].

spec_for_macro({type, Line, 'fun', [{type, _, product, Args}|T]}) ->
  NewArgs = [{type, Line, term, []}|Args],
  {type, Line, 'fun', [{type, Line, product, NewArgs}|T]};

spec_for_macro(Else) -> Else.

%% Loads the form into the code server.

compile_opts(Module) ->
  case ets:lookup(data_table(Module), compile) of
    [{compile,Opts}] when is_list(Opts) -> Opts;
    [] -> []
  end.

load_form(Line, Forms, Opts, #{file := File} = E) ->
  elixir_compiler:module(Forms, File, Opts, fun(Module, Binary0) ->
    Docs = elixir_compiler:get_opt(docs),
    Binary = add_docs_chunk(Binary0, Module, Line, Docs),
    eval_callbacks(Line, Module, after_compile, [E, Binary], E),

    case get(elixir_compiled) of
      Current when is_list(Current) ->
        put(elixir_compiled, [{Module,Binary}|Current]),

        case get(elixir_compiler_pid) of
          undefined -> ok;
          PID ->
            Ref = make_ref(),
            PID ! {module_available, self(), Ref, File, Module, Binary},
            receive {Ref, ack} -> ok end
        end;
      _ ->
        ok
    end,

    Binary
  end).

add_docs_chunk(Bin, Module, Line, true) ->
  ChunkData = term_to_binary({elixir_docs_v1, [
        {docs, get_docs(Module)},
        {moduledoc, get_moduledoc(Line, Module)}
    ]}),
  add_beam_chunk(Bin, "ExDc", ChunkData);

add_docs_chunk(Bin, _, _, _) -> Bin.

get_docs(Module) ->
  ordsets:from_list(
    [{Tuple, Line, Kind, Sig, Doc} ||
     {Tuple, Line, Kind, Sig, Doc} <- ets:tab2list(docs_table(Module)),
     Kind =/= type, Kind =/= opaque]).

get_moduledoc(Line, Module) ->
  {Line, 'Elixir.Module':get_attribute(Module, moduledoc)}.

check_module_availability(Line, File, Module) ->
  Reserved = ['Elixir.Any', 'Elixir.BitString', 'Elixir.Function', 'Elixir.PID',
              'Elixir.Reference', 'Elixir.Elixir', 'Elixir'],

  case lists:member(Module, Reserved) of
    true  -> elixir_errors:form_error([{line, Line}], File, ?MODULE, {module_reserved, Module});
    false -> ok
  end,

  case elixir_compiler:get_opt(ignore_module_conflict) of
    false ->
      case code:ensure_loaded(Module) of
        {module, _} ->
          elixir_errors:form_warn([{line, Line}], File, ?MODULE, {module_defined, Module});
        {error, _}  ->
          ok
      end;
    true ->
      ok
  end.

warn_unused_docs(Line, File, Module, Attribute) ->
  case ets:member(data_table(Module), Attribute) of
    true ->
      elixir_errors:form_warn([{line, Line}], File, ?MODULE, {unused_doc, Attribute});
    _ ->
      ok
  end.

% __INFO__

add_info_function(Line, File, Module, All, Def, Defmacro) ->
  Pair = {'__info__', 1},
  case lists:member(Pair, All) of
    true  ->
      elixir_errors:form_error([{line, Line}], File, ?MODULE, {internal_function_overridden, Pair});
    false ->
      Spec =
        {attribute, Line, spec, {Pair,
          [{type, Line, 'fun', [
            {type, Line, product, [
              {type, Line, union, [
                {atom, Line, attributes},
                {atom, Line, compile},
                {atom, Line, exports},
                {atom, Line, functions},
                {atom, Line, imports},
                {atom, Line, macros},
                {atom, Line, module}
              ]}
            ]},
            {type, Line, union, [
              {type, Line, atom, []},
              {type, Line, list, [
                {type, Line, tuple, [
                  {type, Line, atom, []},
                  {type, Line, any, []}
                ]}
              ]}
            ]}
          ]}]
        }},

      Info =
        {function, 0, '__info__', 1, [
          functions_clause(Def),
          macros_clause(Defmacro),
          module_clause(Module),
          else_clause()
        ]},

      {Spec, Info}
  end.

functions_clause(Def) ->
  {clause, 0, [{atom, 0, functions}], [], [elixir_utils:elixir_to_erl(lists:sort(Def))]}.

macros_clause(Defmacro) ->
  {clause, 0, [{atom, 0, macros}], [], [elixir_utils:elixir_to_erl(lists:sort(Defmacro))]}.

module_clause(Module) ->
  {clause, 0, [{atom, 0, module}], [], [{atom, 0, Module}]}.

else_clause() ->
  Info = {call, 0, {atom, 0, module_info}, [{var, 0, atom}]},
  {clause, 0, [{var, 0, atom}], [], [Info]}.

% HELPERS

%% Adds custom chunk to a .beam binary
add_beam_chunk(Bin, Id, ChunkData)
        when is_binary(Bin), is_list(Id), is_binary(ChunkData) ->
  {ok, _, Chunks0} = beam_lib:all_chunks(Bin),
  NewChunk = {Id, ChunkData},
  Chunks = [NewChunk|Chunks0],
  {ok, NewBin} = beam_lib:build_module(Chunks),
  NewBin.

%% Expands a callback given by M:F(Args). In case
%% the callback can't be expanded, invokes the given
%% fun passing a possibly expanded AM:AF(Args).
expand_callback(Line, M, F, Args, E, Fun) ->
  Meta = [{line,Line},{require,false}],

  {EE, ET} = elixir_dispatch:dispatch_require(Meta, M, F, Args, E, fun(AM, AF, AA) ->
    Fun(AM, AF, AA),
    {ok, E}
  end),

  if
    is_atom(EE) ->
      ET;
    true ->
      try
        {_Value, _Binding, EF, _S} = elixir:eval_forms(EE, [], ET),
        EF
      catch
        Kind:Reason ->
          Info = {M, F, length(Args), location(Line, E)},
          erlang:raise(Kind, Reason, prune_stacktrace(Info, erlang:get_stacktrace()))
      end
  end.

location(Line, E) ->
  [{file, elixir_utils:characters_to_list(?m(E, file))}, {line, Line}].

%% We've reached the elixir_module or eval internals, skip it with the rest
prune_stacktrace(Info, [{elixir, eval_forms, _, _}|_]) ->
  [Info];
prune_stacktrace(Info, [{elixir_module, _, _, _}|_]) ->
  [Info];
prune_stacktrace(Info, [H|T]) ->
  [H|prune_stacktrace(Info, T)];
prune_stacktrace(Info, []) ->
  [Info].

% ERROR HANDLING

format_error({invalid_external_resource, Value}) ->
  io_lib:format("expected a string value for @external_resource, got: ~p",
    ['Elixir.Kernel':inspect(Value)]);
format_error({unused_doc, typedoc}) ->
  "@typedoc provided but no type follows it";
format_error({unused_doc, doc}) ->
  "@doc provided but no definition follows it";
format_error({internal_function_overridden, {Name, Arity}}) ->
  io_lib:format("function ~ts/~B is internal and should not be overridden", [Name, Arity]);
format_error({invalid_module, Module}) ->
  io_lib:format("invalid module name: ~ts", ['Elixir.Kernel':inspect(Module)]);
format_error({module_defined, Module}) ->
  io_lib:format("redefining module ~ts", [elixir_aliases:inspect(Module)]);
format_error({module_reserved, Module}) ->
  io_lib:format("module ~ts is reserved and cannot be defined", [elixir_aliases:inspect(Module)]);
format_error({module_in_definition, Module, File, Line}) ->
  io_lib:format("cannot define module ~ts because it is currently being defined in ~ts:~B",
    [elixir_aliases:inspect(Module), 'Elixir.Path':relative_to_cwd(File), Line]).
