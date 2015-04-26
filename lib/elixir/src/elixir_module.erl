-module(elixir_module).
-export([data_table/1, defs_table/1, clas_table/1, is_open/1,
         compile/4, expand_callback/6, add_beam_chunk/3, format_error/1]).
-include("elixir.hrl").

-define(acc_attr, {elixir, acc_attributes}).
-define(lexical_attr, {elixir, lexical_tracker}).
-define(persisted_attr, {elixir, persisted_attributes}).
-define(overridable_attr, {elixir, overridable}).
-define(location_attr, {elixir, location}).

%% TABLE METHODS

data_table(Module) ->
  ets:lookup_element(elixir_modules, Module, 2).

defs_table(Module) ->
  ets:lookup_element(elixir_modules, Module, 3).

clas_table(Module) ->
  ets:lookup_element(elixir_modules, Module, 4).

is_open(Module) ->
  ets:lookup(elixir_modules, Module) /= [].

%% Compilation hook

compile(Module, Block, Vars, #{line := Line} = Env) when is_atom(Module) ->
  %% In case we are generating a module from inside a function,
  %% we get rid of the lexical tracker information as, at this
  %% point, the lexical tracker process is long gone.
  LexEnv = case ?m(Env, function) of
    nil -> Env#{module := Module};
    _   -> Env#{lexical_tracker := nil, function := nil, module := Module}
  end,

  case ?m(LexEnv, lexical_tracker) of
    nil ->
      elixir_lexical:run(?m(LexEnv, file), nil, fun(Pid) ->
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
  {Data, Defs, Clas, Ref} = build(Line, File, Module, Docs, ?m(E, lexical_tracker)),

  try
    {Result, NE} = eval_form(Line, Module, Data, Block, Vars, E),

    _ = case ets:lookup(Data, 'on_load') of
      [] -> ok;
      [{on_load, OnLoad}] ->
        [elixir_locals:record_local(Tuple, Module) || Tuple <- OnLoad]
    end,

    {Def, Defp, Defmacro, Defmacrop, Exports, Functions, Unreachable} =
      elixir_def:unwrap_definitions(File, Module),

    {All, Forms0} = functions_form(Line, File, Module, Def, Defp,
                                   Defmacro, Defmacrop, Exports, Functions),
    Forms1 = specs_form(Data, Defmacro, Defmacrop, Unreachable, Forms0),
    Forms2 = types_form(Line, File, Data, Forms1),
    Forms3 = attributes_form(Line, File, Data, Forms2),

    elixir_locals:ensure_no_import_conflict(Line, File, Module, All),

    case Docs of
      true  -> warn_unused_docs(Line, File, Data, doc);
      false -> false
    end,

    Location = {elixir_utils:characters_to_list(elixir_utils:relative_to_cwd(File)), Line},

    Final = [
      {attribute, Line, file, Location},
      {attribute, Line, module, Module} | Forms3
    ],

    Binary = load_form(Line, Data, Final, compile_opts(Module), NE),
    {module, Module, Binary, Result}
  catch
    error:undef ->
      case erlang:get_stacktrace() of
        [{Module, Fun, Args, _Info} | _] = Stack when is_list(Args) ->
          compile_undef(Module, Fun, length(Args), Stack);
        [{Module, Fun, Arity, _Info} | _] = Stack ->
          compile_undef(Module, Fun, Arity, Stack);
        Stack ->
          erlang:raise(error, undef, Stack)
      end
  after
    elixir_locals:cleanup(Module),
    ets:delete(Data),
    ets:delete(Defs),
    ets:delete(Clas),
    elixir_code_server:call({undefmodule, Ref})
  end.

%% An undef error for a function in the module being compiled might result in an
%% exception message suggesting the current module is not loaded. This is
%% misleading so use a custom reason.
compile_undef(Module, Fun, Arity, Stack) ->
  ExMod = 'Elixir.UndefinedFunctionError',
  case code:is_loaded(ExMod) of
    false ->
      erlang:raise(error, undef, Stack);
    _ ->
      Opts = [{module, Module}, {function, Fun}, {arity, Arity},
              {reason, 'function not available'}],
      Exception = 'Elixir.UndefinedFunctionError':exception(Opts),
      erlang:raise(error, Exception, Stack)
  end.

%% Hook that builds both attribute and functions and set up common hooks.

build(Line, File, Module, Docs, Lexical) ->
  case ets:lookup(elixir_modules, Module) of
    [{Module, _, _, _, OldLine, OldFile}] ->
      Error = {module_in_definition, Module, OldFile, OldLine},
      elixir_errors:form_error([{line, Line}], File, ?MODULE, Error);
    _ ->
      []
  end,

  Data = ets:new(Module, [set, public]),
  Defs = ets:new(Module, [set, public]),
  Clas = ets:new(Module, [bag, public]),

  Ref = elixir_code_server:call({defmodule, self(),
                                 {Module, Data, Defs, Clas, Line, File}}),

  ets:insert(Data, {before_compile, []}),
  ets:insert(Data, {after_compile, []}),
  ets:insert(Data, {moduledoc, nil}),
  ets:insert(Data, {behaviour_docs, []}),

  OnDefinition =
    case Docs of
      true -> [{'Elixir.Module', compile_doc}];
      _    -> []
    end,
  ets:insert(Data, {on_definition, OnDefinition}),

  Attributes = [behaviour, on_load, compile, external_resource],
  ets:insert(Data, {?acc_attr, [before_compile, after_compile, on_definition, derive,
                                spec, type, typep, opaque, callback|Attributes]}),
  ets:insert(Data, {?persisted_attr, [vsn|Attributes]}),
  ets:insert(Data, {?lexical_attr, Lexical}),

  %% Setup definition related modules
  elixir_def:setup(Module),
  elixir_locals:setup(Module),
  elixir_def_overridable:setup(Module),

  {Data, Defs, Clas, Ref}.

%% Receives the module representation and evaluates it.

eval_form(Line, Module, Data, Block, Vars, E) ->
  {Value, EE} = elixir_compiler:eval_forms(Block, Vars, E),
  elixir_def_overridable:store_pending(Module),
  EV = elixir_env:linify({Line, EE#{vars := [], export_vars := nil}}),
  EC = eval_callbacks(Line, Data, before_compile, [EV], EV),
  elixir_def_overridable:store_pending(Module),
  {Value, EC}.

eval_callbacks(Line, Data, Name, Args, E) ->
  Callbacks = lists:reverse(ets:lookup_element(Data, Name, 2)),

  lists:foldl(fun({M, F}, Acc) ->
    expand_callback(Line, M, F, Args, Acc#{vars := [], export_vars := nil},
                    fun(AM, AF, AA) -> apply(AM, AF, AA) end)
  end, E, Callbacks).

%% Return the form with exports and function declarations.

functions_form(Line, File, Module, Def, Defp, Defmacro, Defmacrop, Exports, Body) ->
  All = Def ++ Defmacro ++ Defp ++ Defmacrop,
  {Spec, Info} = add_info_function(Line, File, Module, All, Def, Defmacro),

  {[{'__info__', 1}|All],
   [{attribute, Line, export, lists:sort([{'__info__', 1}|Exports])},
    Spec, Info | Body]}.

%% Add attributes handling to the form

attributes_form(Line, File, Data, Current) ->
  AccAttrs = ets:lookup_element(Data, ?acc_attr, 2),
  PersistedAttrs = ets:lookup_element(Data, ?persisted_attr, 2),

  Transform = fun({Key, Value}, Acc) when is_atom(Key) ->
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

  Results = ets:select(Data, [{{'$1', '_'}, [{is_atom, '$1'}], ['$_']}]),
  lists:foldl(Transform, Current, Results).

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

types_form(Line, File, Data, Forms0) ->
  case elixir_compiler:get_opt(internal) of
    false ->
      Types0 = get_typespec(Data, type) ++ get_typespec(Data, typep)
                                        ++ get_typespec(Data, opaque),

      Types1 = ['Elixir.Kernel.Typespec':translate_type(Kind, Expr, Doc, Caller) ||
                {Kind, Expr, Doc, Caller} <- Types0],

      warn_unused_docs(Line, File, Data, typedoc),
      Forms1 = types_attributes(Types1, Forms0),
      Forms2 = export_types_attributes(Types1, Forms1),
      typedocs_attributes(Types1, Forms2);

    true ->
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

specs_form(Data, Defmacro, Defmacrop, Unreachable, Forms) ->
  case elixir_compiler:get_opt(internal) of
    false ->
      Specs0 = get_typespec(Data, spec) ++ get_typespec(Data, callback),
      Specs1 = ['Elixir.Kernel.Typespec':translate_spec(Kind, Expr, Caller) ||
                {Kind, Expr, Caller} <- Specs0],
      Specs2 = lists:flatmap(fun(Spec) ->
                               translate_macro_spec(Spec, Defmacro, Defmacrop)
                             end, Specs1),
      Specs3 = lists:filter(fun({{_Kind, NameArity, _Spec}, _Line}) ->
                                not lists:member(NameArity, Unreachable)
                            end, Specs2),
      specs_attributes(Forms, Specs3);
    true ->
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
    [{compile, Opts}] when is_list(Opts) -> Opts;
    [] -> []
  end.

load_form(Line, Data, Forms, Opts, E) ->
  elixir_compiler:module(Forms, Opts, E, fun(Module, Binary0) ->
    Docs = elixir_compiler:get_opt(docs),
    Binary = add_docs_chunk(Binary0, Data, Line, Docs),
    eval_callbacks(Line, Data, after_compile, [E, Binary], E),

    case get(elixir_compiled) of
      Current when is_list(Current) ->
        put(elixir_compiled, [{Module, Binary}|Current]),

        case get(elixir_compiler_pid) of
          undefined -> ok;
          PID ->
            Ref = make_ref(),
            PID ! {module_available, self(), Ref, ?m(E, file), Module, Binary},
            receive {Ref, ack} -> ok end
        end;
      _ ->
        ok
    end,

    Binary
  end).

add_docs_chunk(Bin, Data, Line, true) ->
  ChunkData = term_to_binary({elixir_docs_v1, [
    {docs, get_docs(Data)},
    {moduledoc, get_moduledoc(Line, Data)},
    {behaviour_docs, get_behaviour_docs(Data)}
  ]}),
  add_beam_chunk(Bin, "ExDc", ChunkData);

add_docs_chunk(Bin, _, _, _) -> Bin.

get_docs(Data) ->
  Match = ets:match(Data, {{doc, '$1'}, '$2', '$3', '$4', '$5'}),
  lists:usort(
    [{Tuple, Line, Kind, Sig, Doc} ||
     [Tuple, Line, Kind, Sig, Doc] <- Match,
      Kind =/= type, Kind =/= opaque]).

get_moduledoc(Line, Data) ->
  {Line, ets:lookup_element(Data, moduledoc, 2)}.

get_behaviour_docs(Data) ->
  lists:usort(ets:lookup_element(Data, behaviour_docs, 2)).

get_typespec(Data, Key) ->
  case ets:lookup(Data, Key) of
    [{Key, Value}] -> Value;
    [] -> []
  end.

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

warn_unused_docs(Line, File, Data, Attribute) ->
  case ets:member(Data, Attribute) of
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
                {atom, Line, functions},
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
          info_clause(Module, attributes),
          info_clause(Module, compile)
        ]},

      {Spec, Info}
  end.

functions_clause(Def) ->
  {clause, 0, [{atom, 0, functions}], [], [elixir_utils:elixir_to_erl(lists:sort(Def))]}.

macros_clause(Defmacro) ->
  {clause, 0, [{atom, 0, macros}], [], [elixir_utils:elixir_to_erl(lists:sort(Defmacro))]}.

module_clause(Module) ->
  {clause, 0, [{atom, 0, module}], [], [{atom, 0, Module}]}.

info_clause(Module, Atom) ->
  Info = {call, 0,
            {remote, 0, {atom, 0, erlang}, {atom, 0, get_module_info}},
            [{atom, 0, Module}, {atom, 0, Atom}]},
  {clause, 0, [{atom, 0, Atom}], [], [Info]}.

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
  Meta = [{line, Line}, {require, false}],

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
