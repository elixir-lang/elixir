%% Compiler backend to Erlang.
-module(elixir_erl).
-export([elixir_to_erl/1, definition_to_anonymous/4, compile/1, consolidate/3,
         get_ann/1, debug_info/4, scope/2, format_error/1]).
-include("elixir.hrl").
-define(typespecs, 'Elixir.Kernel.Typespec').

%% debug_info callback

debug_info(elixir_v1, _Module, none, _Opts) ->
  {error, missing};
debug_info(elixir_v1, _Module, {elixir_v1, Map, _Specs}, _Opts) ->
  {ok, Map};
debug_info(erlang_v1, _Module, {elixir_v1, Map, Specs}, _Opts) ->
  {Prefix, Forms, _, _, _, _} = dynamic_form(Map),
  {ok, Prefix ++ Specs ++ Forms};
debug_info(core_v1, _Module, {elixir_v1, Map, Specs}, Opts) ->
  {Prefix, Forms, _, _, _, _} = dynamic_form(Map),
  #{compile_opts := CompileOpts} = Map,
  AllOpts = CompileOpts ++ Opts,

  %% Do not rely on elixir_erl_compiler because we don't
  %% warnings nor the other functionality provided there.
  case elixir_erl_compiler:erl_to_core(Prefix ++ Specs ++ Forms, AllOpts) of
    {ok, CoreForms, _} ->
      try compile:noenv_forms(CoreForms, [?NO_SPAWN_COMPILER_PROCESS, from_core, core, return | AllOpts]) of
        {ok, _, Core, _} -> {ok, Core};
        _What -> {error, failed_conversion}
      catch
        error:_ -> {error, failed_conversion}
      end;
    _ ->
      {error, failed_conversion}
  end;
debug_info(_, _, _, _) ->
  {error, unknown_format}.

%% Builds Erlang AST annotation.

get_ann(Opts) when is_list(Opts) ->
  get_ann(Opts, false, 0).

get_ann([{generated, true} | T], _, Line) -> get_ann(T, true, Line);
get_ann([{line, Line} | T], Gen, _) when is_integer(Line) -> get_ann(T, Gen, Line);
get_ann([_ | T], Gen, Line) -> get_ann(T, Gen, Line);
get_ann([], Gen, Line) -> erl_anno:set_generated(Gen, erl_anno:new(Line)).

%% Converts an Elixir definition to an anonymous function.

definition_to_anonymous(Module, Kind, Meta, Clauses) ->
  ErlClauses = [translate_clause(Kind, Clause, true) || Clause <- Clauses],
  Fun = {'fun', ?ann(Meta), {clauses, ErlClauses}},
  LocalHandler = fun(LocalName, LocalArgs) -> invoke_local(Module, LocalName, LocalArgs) end,
  {value, Result, _Binding} = erl_eval:expr(Fun, [], {value, LocalHandler}),
  Result.

invoke_local(Module, ErlName, Args) ->
  {Name, Arity} = elixir_utils:erl_fa_to_elixir_fa(ErlName, length(Args)),

  case elixir_def:local_for(Module, Name, Arity, all) of
    false ->
      {current_stacktrace, [_ | T]} = erlang:process_info(self(), current_stacktrace),
      erlang:raise(error, undef, [{Module, Name, Arity, []} | T]);
    Fun ->
      apply(Fun, Args)
  end.

%% Converts Elixir quoted literals to Erlang AST.
elixir_to_erl(Tree) ->
  elixir_to_erl(Tree, erl_anno:new(0)).

elixir_to_erl(Tree, Ann) when is_tuple(Tree) ->
  {tuple, Ann, [elixir_to_erl(X, Ann) || X <- tuple_to_list(Tree)]};
elixir_to_erl([], Ann) ->
  {nil, Ann};
elixir_to_erl(<<>>, Ann) ->
  {bin, Ann, []};
elixir_to_erl(Tree, Ann) when is_list(Tree) ->
  elixir_to_erl_cons(Tree, Ann);
elixir_to_erl(Tree, Ann) when is_atom(Tree) ->
  {atom, Ann, Tree};
elixir_to_erl(Tree, Ann) when is_integer(Tree) ->
  {integer, Ann, Tree};
elixir_to_erl(Tree, Ann) when is_float(Tree) ->
  {float, Ann, Tree};
elixir_to_erl(Tree, Ann) when is_binary(Tree) ->
  %% Note that our binaries are UTF-8 encoded and we are converting
  %% to a list using binary_to_list. The reason for this is that Erlang
  %% considers a string in a binary to be encoded in latin1, so the bytes
  %% are not changed in any fashion.
  {bin, Ann, [{bin_element, Ann, {string, Ann, binary_to_list(Tree)}, default, default}]};
elixir_to_erl(Pid, Ann) when is_pid(Pid) ->
  ?remote(Ann, erlang, binary_to_term, [elixir_to_erl(term_to_binary(Pid), Ann)]);
elixir_to_erl(_Other, _Ann) ->
  error(badarg).

elixir_to_erl_cons([H | T], Ann) -> {cons, Ann, elixir_to_erl(H, Ann), elixir_to_erl_cons(T, Ann)};
elixir_to_erl_cons(T, Ann) -> elixir_to_erl(T, Ann).

%% Returns a scope for translation.

scope(_Meta, ExpandCaptures) ->
  #elixir_erl{expand_captures=ExpandCaptures}.

%% Static compilation hook, used in protocol consolidation

consolidate(Map, TypeSpecs, Chunks) ->
  {Prefix, Forms, _Def, _Defmacro, _Macros, _NoWarnUndefined} = dynamic_form(Map),
  load_form(Map, Prefix, Forms, TypeSpecs, Chunks).

%% Dynamic compilation hook, used in regular compiler

compile(Map) ->
  elixir_erl_compiler:spawn(fun spawned_compile/1, [Map]).

spawned_compile(#{module := Module, line := Line} = Map) ->
  {Set, Bag} = elixir_module:data_tables(Module),

  TranslatedTypespecs =
    case elixir_config:get(bootstrap) andalso
          (code:ensure_loaded(?typespecs) /= {module, ?typespecs}) of
      true -> {[], [], [], [], []};
      false -> ?typespecs:translate_typespecs_for_module(Set, Bag)
    end,

  {Prefix, Forms, Def, Defmacro, Macros, NoWarnUndefined} = dynamic_form(Map),
  {Types, Callbacks, TypeSpecs} = typespecs_form(Map, TranslatedTypespecs, Macros),

  DocsChunk = docs_chunk(Set, Module, Line, Def, Defmacro, Types, Callbacks),
  CheckerChunk = checker_chunk(Map, NoWarnUndefined),
  load_form(Map, Prefix, Forms, TypeSpecs, DocsChunk ++ CheckerChunk).

dynamic_form(#{module := Module, line := Line, relative_file := RelativeFile,
               attributes := Attributes, definitions := Definitions, unreachable := Unreachable,
               deprecated := Deprecated, compile_opts := Opts}) ->
  {Def, Defmacro, Macros, Exports, Functions} =
    split_definition(Definitions, Unreachable, [], [], [], [], {[], []}),

  {NoWarnUndefined, FilteredOpts} = split_no_warn_undefined(Opts, [], []),
  Location = {elixir_utils:characters_to_list(RelativeFile), Line},
  Prefix = [{attribute, Line, file, Location},
            {attribute, Line, module, Module},
            {attribute, Line, compile, [no_auto_import | FilteredOpts]}],

  Forms0 = functions_form(Line, Module, Def, Defmacro, Exports, Functions, Deprecated),
  Forms1 = attributes_form(Line, Attributes, Forms0),
  {Prefix, Forms1, Def, Defmacro, Macros, NoWarnUndefined}.

% Definitions

split_definition([{Tuple, Kind, Meta, Clauses} | T], Unreachable,
                 Def, Defmacro, Macros, Exports, Functions) ->
  case lists:member(Tuple, Unreachable) of
    false ->
      split_definition(Tuple, Kind, Meta, Clauses, T, Unreachable,
                       Def, Defmacro, Macros, Exports, Functions);
    true ->
      split_definition(T, Unreachable, Def, Defmacro, Macros, Exports, Functions)
  end;
split_definition([], _Unreachable, Def, Defmacro, Macros, Exports, {Head, Tail}) ->
  {Def, Defmacro, Macros, Exports, Head ++ Tail}.

split_definition(Tuple, def, Meta, Clauses, T, Unreachable,
                 Def, Defmacro, Macros, Exports, Functions) ->
  {_, _, N, A, _} = Entry = translate_definition(def, Meta, Tuple, Clauses),
  split_definition(T, Unreachable, [Tuple | Def], Defmacro, Macros, [{N, A} | Exports],
                   add_definition(Meta, Entry, Functions));

split_definition(Tuple, defp, Meta, Clauses, T, Unreachable,
                 Def, Defmacro, Macros, Exports, Functions) ->
  Entry = translate_definition(defp, Meta, Tuple, Clauses),
  split_definition(T, Unreachable, Def, Defmacro, Macros, Exports,
                   add_definition(Meta, Entry, Functions));

split_definition(Tuple, defmacro, Meta, Clauses, T, Unreachable,
                 Def, Defmacro, Macros, Exports, Functions) ->
  {_, _, N, A, _} = Entry = translate_definition(defmacro, Meta, Tuple, Clauses),
  split_definition(T, Unreachable, Def, [Tuple | Defmacro], [Tuple | Macros], [{N, A} | Exports],
                   add_definition(Meta, Entry, Functions));

split_definition(Tuple, defmacrop, Meta, Clauses, T, Unreachable,
                 Def, Defmacro, Macros, Exports, Functions) ->
  Entry = translate_definition(defmacro, Meta, Tuple, Clauses),
  split_definition(T, Unreachable, Def, Defmacro, [Tuple | Macros], Exports,
                   add_definition(Meta, Entry, Functions)).

add_definition(Meta, Body, {Head, Tail}) ->
  case lists:keyfind(file, 1, Meta) of
    {file, {F, L}} ->
      %% Erlang's epp attempts to perform offsetting when generated is set to true
      %% and that causes cover to fail when processing modules. Therefore we never
      %% pass the generated annotation forward for file attributes. The function
      %% will still be marked as generated though if that's the case.
      FileMeta = erl_anno:set_generated(false, ?ann(Meta)),
      Attr = {attribute, FileMeta, file, {elixir_utils:characters_to_list(F), L}},
      {Head, [Attr, Body | Tail]};
    false ->
      {[Body | Head], Tail}
  end.

translate_definition(Kind, Meta, {Name, Arity}, Clauses) ->
  ErlClauses = [translate_clause(Kind, Clause, false) || Clause <- Clauses],

  case is_macro(Kind) of
    true -> {function, ?ann(Meta), elixir_utils:macro_name(Name), Arity + 1, ErlClauses};
    false -> {function, ?ann(Meta), Name, Arity, ErlClauses}
  end.

translate_clause(Kind, {Meta, Args, Guards, Body}, ExpandCaptures) ->
  S = scope(Meta, ExpandCaptures),

  {TClause, TS} = elixir_erl_clauses:clause(Meta,
                    fun elixir_erl_pass:translate_args/2, Args, Body, Guards, S),

  case is_macro(Kind) of
    true ->
      Ann = ?ann(Meta),
      FArgs = {var, Ann, '_@CALLER'},
      MClause = setelement(3, TClause, [FArgs | element(3, TClause)]),

      case TS#elixir_erl.caller of
        true  ->
          FBody = {'match', Ann,
            {'var', Ann, '__CALLER__'},
            ?remote(Ann, elixir_env, linify, [{var, Ann, '_@CALLER'}])
          },
          setelement(5, MClause, [FBody | element(5, TClause)]);
        false ->
          MClause
      end;
    false ->
      TClause
  end.

is_macro(defmacro)  -> true;
is_macro(defmacrop) -> true;
is_macro(_)         -> false.

% Functions

functions_form(Line, Module, Def, Defmacro, Exports, Body, Deprecated) ->
  {Spec, Info} = add_info_function(Line, Module, Def, Defmacro, Deprecated),
  [{attribute, Line, export, lists:sort([{'__info__', 1} | Exports])}, Spec, Info | Body].

add_info_function(Line, Module, Def, Defmacro, Deprecated) ->
  AllowedAttrs = [attributes, compile, functions, macros, md5, module, deprecated],
  AllowedArgs = lists:map(fun(Atom) -> {atom, Line, Atom} end, AllowedAttrs),

  Spec =
    {attribute, Line, spec, {{'__info__', 1},
      [{type, Line, 'fun', [
        {type, Line, product, [
          {type, Line, union, AllowedArgs}
        ]},
        {type, Line, any, []}
      ]}]
    }},

  Info =
    {function, 0, '__info__', 1, [
      direct_module_info(Module),
      functions_info(Def),
      macros_info(Defmacro),
      get_module_info(Module, attributes),
      get_module_info(Module, compile),
      get_module_info(Module, md5),
      deprecated_info(Deprecated)
    ]},

  {Spec, Info}.

direct_module_info(Module) ->
  {clause, 0, [{atom, 0, module}], [], [{atom, 0, Module}]}.

functions_info(Def) ->
  {clause, 0, [{atom, 0, functions}], [], [elixir_erl:elixir_to_erl(lists:sort(Def))]}.

macros_info(Defmacro) ->
  {clause, 0, [{atom, 0, macros}], [], [elixir_erl:elixir_to_erl(lists:sort(Defmacro))]}.

get_module_info(Module, Key) ->
  Call = ?remote(0, erlang, get_module_info, [{atom, 0, Module}, {var, 0, 'Key'}]),
  {clause, 0, [{match, 0, {var, 0, 'Key'}, {atom, 0, Key}}], [], [Call]}.

deprecated_info(Deprecated) ->
  {clause, 0, [{atom, 0, deprecated}], [], [elixir_erl:elixir_to_erl(Deprecated)]}.

% Typespecs

typespecs_form(Map, TranslatedTypespecs, MacroNames) ->
  {Types, Specs, Callbacks, MacroCallbacks, OptionalCallbacks} = TranslatedTypespecs,

  AllCallbacks = Callbacks ++ MacroCallbacks,
  MacroCallbackNames = [NameArity || {_, NameArity, _, _} <- MacroCallbacks],
  validate_behaviour_info_and_attributes(Map, AllCallbacks),
  validate_optional_callbacks(Map, AllCallbacks, OptionalCallbacks),

  Forms0 = [],
  Forms1 = types_form(Types, Forms0),
  Forms2 = callspecs_form(spec, Specs, [], MacroNames, Forms1, Map),
  Forms3 = callspecs_form(callback, AllCallbacks, OptionalCallbacks, MacroCallbackNames, Forms2, Map),

  AllCallbacksWithoutSpecs = lists:usort([
    {Kind, Name, Arity} || {Kind, {Name, Arity}, _Line, _Spec} <- AllCallbacks
  ]),

  {Types, AllCallbacksWithoutSpecs, Forms3}.

%% Types

types_form(Types, Forms) ->
  Fun = fun
    ({Kind, NameArity, Line, Expr, true}, Acc) ->
      [{attribute, Line, export_type, [NameArity]}, {attribute, Line, Kind, Expr} | Acc];
    ({Kind, _NameArity, Line, Expr, false}, Acc) ->
      [{attribute, Line, Kind, Expr} | Acc]
  end,

  lists:foldl(Fun, Forms, Types).

%% Specs and callbacks

validate_behaviour_info_and_attributes(#{definitions := Defs} = Map, AllCallbacks) ->
  case {lists:keyfind({behaviour_info, 1}, 1, Defs), AllCallbacks} of
    {false, _} ->
      ok;
    {_, [{Kind, {Name, Arity}, _, _} | _]} when Kind == callback; Kind == macrocallback ->
      form_error(Map, {callbacks_but_also_behaviour_info, {Kind, Name, Arity}});
    {_, _} ->
      ok
  end.

validate_optional_callbacks(Map, AllCallbacks, Optional) ->
  lists:foldl(fun(Callback, Acc) ->
    case Callback of
      {Name, Arity} when is_atom(Name) and is_integer(Arity) -> ok;
      _ -> form_error(Map, {ill_defined_optional_callback, Callback})
    end,

    case lists:keyfind(Callback, 2, AllCallbacks) of
      false -> form_error(Map, {unknown_callback, Callback});
      _ -> ok
    end,

    case Acc of
      #{Callback := _} -> form_error(Map, {duplicate_optional_callback, Callback});
      _ -> ok
    end,

    maps:put(Callback, true, Acc)
  end, #{}, Optional).

callspecs_form(_Kind, [], _Optional, _Macros, Forms, _ModuleMap) ->
  Forms;
callspecs_form(Kind, Entries, Optional, Macros, Forms, ModuleMap) ->
  #{unreachable := Unreachable} = ModuleMap,

  {SpecsMap, Signatures} =
    lists:foldl(fun({_, NameArity, Line, Spec}, {Acc, NA}) ->
      case Kind of
        spec -> validate_spec_for_existing_function(ModuleMap, NameArity, Line);
        _ -> ok
      end,

      case lists:member(NameArity, Unreachable) of
        false ->
          case Acc of
            #{NameArity := List} -> {Acc#{NameArity := [{Spec, Line} | List]}, NA};
            #{} -> {Acc#{NameArity => [{Spec, Line}]}, [NameArity | NA]}
          end;
        true ->
          {Acc, NA}
      end
    end, {#{}, []}, Entries),

  lists:foldl(fun(NameArity, Acc) ->
    #{NameArity := ExprsLines} = SpecsMap,
    {Exprs, Lines} = lists:unzip(ExprsLines),
    Line = lists:min(Lines),

    {Key, Value} =
      case lists:member(NameArity, Macros) of
        true ->
          {Name, Arity} = NameArity,
          {{elixir_utils:macro_name(Name), Arity + 1},
           lists:map(fun spec_for_macro/1, Exprs)};
        false ->
          {NameArity, Exprs}
      end,

    case lists:member(NameArity, Optional) of
      true ->
        [{attribute, Line, Kind, {Key, lists:reverse(Value)}},
         {attribute, Line, optional_callbacks, [Key]} | Acc];
      false ->
        [{attribute, Line, Kind, {Key, lists:reverse(Value)}} | Acc]
    end
  end, Forms, lists:sort(Signatures)).

spec_for_macro({type, Line, 'bounded_fun', [H | T]}) ->
  {type, Line, 'bounded_fun', [spec_for_macro(H) | T]};
spec_for_macro({type, Line, 'fun', [{type, _, product, Args} | T]}) ->
  {type, Line, 'fun', [{type, Line, product, [{type, Line, term, []} | Args]} | T]};
spec_for_macro(Else) ->
  Else.

validate_spec_for_existing_function(ModuleMap, NameAndArity, Line) ->
  #{definitions := Defs, file := File} = ModuleMap,

  case lists:keymember(NameAndArity, 1, Defs) of
    true -> ok;
    false -> form_error(#{line => Line, file => File}, {spec_for_undefined_function, NameAndArity})
  end.

% Attributes

attributes_form(Line, Attributes, Forms) ->
  Fun = fun({Key, Value}, Acc) -> [{attribute, Line, Key, Value} | Acc] end,
  lists:foldr(Fun, Forms, Attributes).

% Loading forms

load_form(#{file := File, compile_opts := Opts} = Map, Prefix, Forms, Specs, Chunks) ->
  CompileOpts = extra_chunks_opts(Chunks, debug_opts(Map, Specs, Opts)),
  {_, Binary} = elixir_erl_compiler:forms(Prefix ++ Specs ++ Forms, File, CompileOpts),
  Binary.

debug_opts(Map, Specs, Opts) ->
  case take_debug_opts(Opts) of
    {true, Rest} -> [{debug_info, {?MODULE, {elixir_v1, Map, Specs}}} | Rest];
    {false, Rest} -> [{debug_info, {?MODULE, none}} | Rest]
  end.

take_debug_opts(Opts) ->
  case proplists:get_value(debug_info, Opts) of
    true -> {true, proplists:delete(debug_info, Opts)};
    false -> {false, proplists:delete(debug_info, Opts)};
    undefined -> {elixir_config:get(debug_info), Opts}
  end.

extra_chunks_opts([], Opts) -> Opts;
extra_chunks_opts(Chunks, Opts) -> [{extra_chunks, Chunks} | Opts].

docs_chunk(Set, Module, Line, Def, Defmacro, Types, Callbacks) ->
  case elixir_config:get(docs) of
    true ->
      {ModuleDocLine, ModuleDoc} = get_moduledoc(Line, Set),
      ModuleDocMeta = get_moduledoc_meta(Set),
      FunctionDocs = get_docs(Set, Module, Def, function),
      MacroDocs = get_docs(Set, Module, Defmacro, macro),
      CallbackDocs = get_callback_docs(Set, Callbacks),
      TypeDocs = get_type_docs(Set, Types),

      DocsChunkData = term_to_binary({docs_v1,
        erl_anno:new(ModuleDocLine),
        elixir,
        <<"text/markdown">>,
        ModuleDoc,
        ModuleDocMeta,
        FunctionDocs ++ MacroDocs ++ CallbackDocs ++ TypeDocs
      }, [compressed]),

      [{<<"Docs">>, DocsChunkData}];

    false ->
      []
  end.

doc_value(Doc) ->
  case Doc of
    nil -> none;
    false -> hidden;
    Doc -> #{<<"en">> => Doc}
  end.

get_moduledoc(Line, Set) ->
  case ets:lookup_element(Set, moduledoc, 2) of
    nil -> {Line, none};
    {DocLine, Doc} -> {DocLine, doc_value(Doc)}
  end.

get_moduledoc_meta(Set) ->
  case ets:lookup(Set, {moduledoc, meta}) of
    [] -> #{};
    [{{moduledoc, meta}, Map, _}] when is_map(Map) -> Map
  end.

get_docs(Set, Module, Definitions, Kind) ->
  [{Key,
    erl_anno:new(Line),
    [signature_to_binary(Module, Name, Signature)],
    doc_value(Doc),
    Meta
   } || {Name, Arity} <- Definitions,
        {Key, _Ctx, Line, Signature, Doc, Meta} <- ets:lookup(Set, {Kind, Name, Arity})].

get_callback_docs(Set, Callbacks) ->
  [{Key,
    erl_anno:new(Line),
    [],
    doc_value(Doc),
    Meta
   } || Callback <- Callbacks, {Key, Line, Doc, Meta} <- ets:lookup(Set, Callback)].

get_type_docs(Set, Types) ->
  [{Key,
    erl_anno:new(Line),
    [],
    doc_value(Doc),
    Meta
   } || {_Kind, {Name, Arity}, _, _, true} <- Types,
        {Key, Line, Doc, Meta} <- ets:lookup(Set, {type, Name, Arity})].

signature_to_binary(_Module, Name, _Signature) when Name == '__aliases__'; Name == '__block__' ->
  <<(atom_to_binary(Name, utf8))/binary, "(args)">>;

signature_to_binary(_Module, fn, _Signature) ->
  <<"fn">>;

signature_to_binary(_Module, Name, _Signature)
    when Name == '__CALLER__'; Name == '__DIR__'; Name == '__ENV__';
         Name == '__MODULE__'; Name == '__STACKTRACE__'; Name == '%{}' ->
  atom_to_binary(Name, utf8);

signature_to_binary(_Module, '%', _) ->
  <<"%struct{}">>;

signature_to_binary(Module, '__struct__', []) ->
  <<"%", ('Elixir.Kernel':inspect(Module))/binary, "{}">>;

signature_to_binary(_, Name, Signature) ->
  'Elixir.Macro':to_string({Name, [], Signature}).

checker_chunk(#{definitions := Definitions, deprecated := Deprecated, is_behaviour := IsBehaviour}, NoWarnUndefined) ->
  DeprecatedMap = maps:from_list(Deprecated),

  Exports =
    lists:foldl(fun({Function, Kind, _Meta, _Clauses}, Acc) ->
      case Kind of
        _ when Kind == def orelse Kind == defmacro ->
          Reason = maps:get(Function, DeprecatedMap, nil),
          [{Function, #{kind => Kind, deprecated_reason => Reason}} | Acc];
        _ ->
          Acc
      end
    end, [], Definitions),

  Contents = #{
    exports => lists:sort(behaviour_info_exports(IsBehaviour) ++ Exports),
    no_warn_undefined => NoWarnUndefined
  },

  [{<<"ExCk">>, erlang:term_to_binary({elixir_checker_v1, Contents})}].

behaviour_info_exports(true) -> [{{behaviour_info, 1}, #{kind => def, deprecated_reason => nil}}];
behaviour_info_exports(false) -> [].

split_no_warn_undefined([{no_warn_undefined, NoWarnUndefined} | CompileOpts], AccNWU, AccCO) ->
  split_no_warn_undefined(CompileOpts, list_wrap(NoWarnUndefined) ++ AccNWU, AccCO);
split_no_warn_undefined([Opt | CompileOpts], AccNWU, AccCO) ->
  split_no_warn_undefined(CompileOpts, AccNWU, [Opt | AccCO]);
split_no_warn_undefined([], AccNWU, AccCO) ->
  {AccNWU, lists:reverse(AccCO)}.

list_wrap(List) when is_list(List) -> List;
list_wrap(Other) -> [Other].

%% Errors

form_error(#{line := Line, file := File}, Error) ->
  elixir_errors:form_error([{line, Line}], File, ?MODULE, Error).

format_error({ill_defined_optional_callback, Callback}) ->
  io_lib:format("invalid optional callback ~ts. @optional_callbacks expects a "
                "keyword list of callback names and arities", ['Elixir.Kernel':inspect(Callback)]);
format_error({unknown_callback, {Name, Arity}}) ->
  io_lib:format("unknown callback ~ts/~B given as optional callback", [Name, Arity]);
format_error({duplicate_optional_callback, {Name, Arity}}) ->
  io_lib:format("~ts/~B has been specified as optional callback more than once", [Name, Arity]);
format_error({callbacks_but_also_behaviour_info, {Type, Fun, Arity}}) ->
  io_lib:format("cannot define @~ts attribute for ~ts/~B when behaviour_info/1 is defined",
                [Type, Fun, Arity]);
format_error({spec_for_undefined_function, {Name, Arity}}) ->
  io_lib:format("spec for undefined function ~ts/~B", [Name, Arity]).
