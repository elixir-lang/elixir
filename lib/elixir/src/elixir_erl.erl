%% SPDX-License-Identifier: Apache-2.0
%% SPDX-FileCopyrightText: 2021 The Elixir Team
%% SPDX-FileCopyrightText: 2012 Plataformatec

%% Compiler backend to Erlang.

-module(elixir_erl).
-export([elixir_to_erl/1, elixir_to_erl/2, definition_to_anonymous/5, compile/2, consolidate/4,
         get_ann/1, debug_info/4, scope/2, checker_chunk/2, checker_version/0, format_error/1]).
-include("elixir.hrl").
-define(typespecs, 'Elixir.Kernel.Typespec').

checker_version() ->
  elixir_checker_v5.

%% debug_info callback

debug_info(elixir_v1, _Module, none, _Opts) ->
  {error, missing};
debug_info(elixir_v1, _Module, {elixir_v1, Map, _Specs}, _Opts) ->
  {ok, Map};
debug_info(erlang_v1, _Module, {elixir_v1, Map, Specs}, _Opts) ->
  {Prefix, Forms, _, _, _} = dynamic_form(Map, nil),
  {ok, Prefix ++ Specs ++ Forms};
debug_info(core_v1, _Module, {elixir_v1, Map, Specs}, Opts) ->
  {Prefix, Forms, _, _, _} = dynamic_form(Map, nil),
  #{compile_opts := CompileOpts} = Map,
  AllOpts = CompileOpts ++ Opts,

  %% Do not rely on elixir_erl_compiler because we don't warn
  %% warnings nor the other functionality provided there.
  case elixir_erl_compiler:erl_to_core(Prefix ++ Specs ++ Forms, AllOpts) of
    {ok, CoreForms, _} ->
      try compile:noenv_forms(CoreForms, [no_spawn_compiler_process, from_core, to_core, return | AllOpts]) of
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
  get_ann(Opts, false, 0, undefined).

get_ann([{generated, true} | T], _, Line, Column) -> get_ann(T, true, Line, Column);
get_ann([{line, Line} | T], Gen, _, Column) when is_integer(Line) -> get_ann(T, Gen, Line, Column);
get_ann([{column, Column} | T], Gen, Line, _) when is_integer(Column) -> get_ann(T, Gen, Line, Column);
get_ann([_ | T], Gen, Line, Column) -> get_ann(T, Gen, Line, Column);
get_ann([], Gen, Line, undefined) -> erl_anno:set_generated(Gen, erl_anno:new(Line));
get_ann([], Gen, Line, Column) -> erl_anno:set_generated(Gen, erl_anno:new({Line, Column})).

%% Converts an Elixir definition to an anonymous function.

definition_to_anonymous(Kind, Meta, Clauses, LocalHandler, ExternalHandler) ->
  ErlClauses = [translate_clause(Kind, 0, Clause, true) || Clause <- Clauses],
  Fun = {'fun', ?ann(Meta), {clauses, ErlClauses}},
  {value, Result, _Binding} = erl_eval:expr(Fun, [], LocalHandler, ExternalHandler),
  Result.

%% Converts Elixir quoted literals to Erlang AST.
elixir_to_erl(Tree) ->
  elixir_to_erl(Tree, erl_anno:new(0)).

elixir_to_erl(Tree, Ann) when is_tuple(Tree) ->
  {tuple, Ann, [elixir_to_erl(X, Ann) || X <- tuple_to_list(Tree)]};
elixir_to_erl([], Ann) ->
  {nil, Ann};
elixir_to_erl(<<>>, Ann) ->
  {bin, Ann, []};
elixir_to_erl(#{} = Map, Ann) ->
  Assocs = [{map_field_assoc, Ann, elixir_to_erl(K, Ann), elixir_to_erl(V, Ann)}
            || {K, V} <- lists:sort(maps:to_list(Map))],
  {map, Ann, Assocs};
elixir_to_erl(Tree, Ann) when is_list(Tree) ->
  elixir_to_erl_cons(Tree, Ann);
elixir_to_erl(Tree, Ann) when is_atom(Tree) ->
  {atom, Ann, Tree};
elixir_to_erl(Tree, Ann) when is_integer(Tree) ->
  {integer, Ann, Tree};
elixir_to_erl(Tree, Ann) when is_float(Tree), Tree == 0.0 ->
   % 0.0 needs to be rewritten as the AST for +0.0 in matches
   Op =
    case <<Tree/float>> of
        <<1:1,_:63>> -> '-';
        _ -> '+'
    end,
  {op, Ann, Op, {float, Ann, 0.0}};
elixir_to_erl(Tree, Ann) when is_float(Tree) ->
  {float, Ann, Tree};
elixir_to_erl(Tree, Ann) when is_binary(Tree) ->
  %% Note that our binaries are UTF-8 encoded and we are converting
  %% to a list using binary_to_list. The reason for this is that Erlang
  %% considers a string in a binary to be encoded in latin1, so the bytes
  %% are not changed in any fashion.
  {bin, Ann, [{bin_element, Ann, {string, Ann, binary_to_list(Tree)}, default, default}]};
elixir_to_erl(Tree, Ann) when is_bitstring(Tree) ->
  Segments = [elixir_to_erl_bitstring_segment(X, Ann) || X <- bitstring_to_list(Tree)],
  {bin, Ann, Segments};
elixir_to_erl(Tree, Ann) when is_function(Tree) ->
  case (erlang:fun_info(Tree, type) == {type, external}) andalso
       (erlang:fun_info(Tree, env) == {env, []}) of
    true ->
      {module, Module} = erlang:fun_info(Tree, module),
      {name, Name} = erlang:fun_info(Tree, name),
      {arity, Arity} = erlang:fun_info(Tree, arity),
      {'fun', Ann, {function, {atom, Ann, Module}, {atom, Ann, Name}, {integer, Ann, Arity}}};
    false ->
      error(badarg, [Tree, Ann])
  end;
elixir_to_erl(Tree, Ann) when is_pid(Tree); is_port(Tree); is_reference(Tree) ->
  ?remote(Ann, erlang, binary_to_term, [elixir_to_erl(term_to_binary(Tree), Ann)]);
elixir_to_erl(Tree, Ann) ->
  error(badarg, [Tree, Ann]).

elixir_to_erl_cons([H | T], Ann) -> {cons, Ann, elixir_to_erl(H, Ann), elixir_to_erl_cons(T, Ann)};
elixir_to_erl_cons(T, Ann) -> elixir_to_erl(T, Ann).

elixir_to_erl_bitstring_segment(Int, Ann) when is_integer(Int) ->
  {bin_element, Ann, {integer, Ann, Int}, default, [integer]};
elixir_to_erl_bitstring_segment(Rest, Ann) when is_bitstring(Rest) ->
  Size = bit_size(Rest),
  <<Int:Size>> = Rest,
  {bin_element, Ann, {integer, Ann, Int}, {integer, Ann, Size}, [integer]}.

%% Returns a scope for translation.

scope(_Meta, ExpandCaptures) ->
  #elixir_erl{expand_captures=ExpandCaptures}.

%% Static compilation hook, used in protocol consolidation

consolidate(Map, Checker, TypeSpecs, DocsChunk) ->
  {Prefix, Forms, _Def, _Defmacro, _Macros} = dynamic_form(Map, nil),
  CheckerChunk = checker_chunk(Checker, chunk_opts(Map)),
  load_form(Map, Prefix, Forms, TypeSpecs, DocsChunk ++ CheckerChunk).

%% Used for updating type checking chunks in Elixir

checker_chunk(nil, _ChunkOpts) ->
  [];
checker_chunk(Contents, ChunkOpts) ->
  [{<<"ExCk">>, term_to_binary({checker_version(), Contents}, ChunkOpts)}].

%% Dynamic compilation hook, used in regular compiler

compile(#{module := Module, anno := Anno} = BaseMap, Signatures) ->
  Map =
    case elixir_erl_compiler:env_compiler_options() of
      [] -> BaseMap;
      EnvOptions -> BaseMap#{compile_opts := ?key(BaseMap, compile_opts) ++ EnvOptions}
    end,

  {Set, Bag} = elixir_module:data_tables(Module),

  TranslatedTypespecs =
    case elixir_config:is_bootstrap() andalso
          (code:ensure_loaded(?typespecs) /= {module, ?typespecs}) of
      true -> {[], [], [], [], []};
      false -> ?typespecs:translate_typespecs_for_module(Set, Bag)
    end,

  MD5 = ets:lookup_element(Set, exports_md5, 2),
  {Prefix, Forms, Def, Defmacro, Macros} = dynamic_form(Map, MD5),
  {Types, Callbacks, TypeSpecs} = typespecs_form(Map, TranslatedTypespecs, Macros),

  ChunkOpts = chunk_opts(Map),
  DocsChunk = docs_chunk(Map, Set, Module, Anno, Def, Defmacro, Types, Callbacks, ChunkOpts),
  CheckerChunk = checker_chunk(Map, Def, Signatures, ChunkOpts),
  load_form(Map, Prefix, Forms, TypeSpecs, DocsChunk ++ CheckerChunk).

chunk_opts(Map) ->
  case lists:member(deterministic, ?key(Map, compile_opts)) of
    true -> [deterministic];
    false -> []
  end.

dynamic_form(#{module := Module, relative_file := RelativeFile,
               attributes := Attributes, definitions := Definitions, unreachable := Unreachable,
               deprecated := Deprecated, compile_opts := Opts} = Map, MD5) ->
  %% TODO: Match on anno directly in Elixir v1.22+
  Line = case Map of
    #{anno := AnnoValue} -> erl_anno:line(AnnoValue);
    #{line := LineValue} -> LineValue
  end,

  {Def, Defmacro, Macros, Exports, Functions} =
    split_definition(Definitions, Unreachable, Line, [], [], [], [], {[], []}),

  FilteredOpts = proplists:delete(debug_info, proplists:delete(no_warn_undefined, Opts)),
  Location = {elixir_utils:characters_to_list(RelativeFile), Line},

  Prefix = [{attribute, Line, file, Location},
            {attribute, Line, module, Module},
            {attribute, Line, compile, [no_auto_import | FilteredOpts]}],

  Struct = maps:get(struct, Map, nil),
  Forms0 = functions_form(Line, Module, Def, Defmacro, Exports, Functions, Deprecated, Struct, MD5),
  Forms1 = attributes_form(Line, Attributes, Forms0),
  {Prefix, Forms1, Def, Defmacro, Macros}.

% Definitions

split_definition([{Tuple, Kind, Meta, Clauses} | T], Unreachable, Line,
                 Def, Defmacro, Macros, Exports, Functions) ->
  case lists:member(Tuple, Unreachable) of
    false ->
      split_definition(Tuple, Kind, Meta, Clauses, T, Unreachable, Line,
                       Def, Defmacro, Macros, Exports, Functions);
    true ->
      split_definition(T, Unreachable, Line, Def, Defmacro, Macros, Exports, Functions)
  end;

split_definition([], _Unreachable, _Line, Def, Defmacro, Macros, Exports, {Head, Tail}) ->
  {lists:sort(Def), lists:sort(Defmacro), Macros, Exports, Head ++ Tail}.

split_definition(Tuple, def, Meta, Clauses, T, Unreachable, Line,
                 Def, Defmacro, Macros, Exports, Functions) ->
  {_, _, N, A, _} = Entry = translate_definition(def, Line, Meta, Tuple, Clauses),
  split_definition(T, Unreachable, Line, [{Tuple, Meta} | Def], Defmacro, Macros, [{N, A} | Exports],
                   add_definition(Meta, Entry, Functions));

split_definition(Tuple, defp, Meta, Clauses, T, Unreachable, Line,
                 Def, Defmacro, Macros, Exports, Functions) ->
  Entry = translate_definition(defp, Line, Meta, Tuple, Clauses),
  split_definition(T, Unreachable, Line, Def, Defmacro, Macros, Exports,
                   add_definition(Meta, Entry, Functions));

split_definition(Tuple, defmacro, Meta, Clauses, T, Unreachable, Line,
                 Def, Defmacro, Macros, Exports, Functions) ->
  {_, _, N, A, _} = Entry = translate_definition(defmacro, Line, Meta, Tuple, Clauses),
  split_definition(T, Unreachable, Line, Def, [{Tuple, Meta} | Defmacro], [Tuple | Macros], [{N, A} | Exports],
                   add_definition(Meta, Entry, Functions));

split_definition(Tuple, defmacrop, Meta, Clauses, T, Unreachable, Line,
                 Def, Defmacro, Macros, Exports, Functions) ->
  Entry = translate_definition(defmacro, Line, Meta, Tuple, Clauses),
  split_definition(T, Unreachable, Line, Def, Defmacro, [Tuple | Macros], Exports,
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

translate_definition(Kind, Line, Meta, {Name, Arity}, Clauses) ->
  ErlClauses = [translate_clause(Kind, Line, Clause, false) || Clause <- Clauses],

  case is_macro(Kind) of
    true -> {function, ?ann(Meta), elixir_utils:macro_name(Name), Arity + 1, ErlClauses};
    false -> {function, ?ann(Meta), Name, Arity, ErlClauses}
  end.

translate_clause(Kind, Line, {Meta, Args, Guards, Body}, ExpandCaptures) ->
  S = scope(Meta, ExpandCaptures),

  %% If the line matches the module line, then it is most likely an
  %% auto-generated function and we don't want to track its contents.
  Ann =
    case ?line(Meta) of
      Line -> erl_anno:set_generated(true, erl_anno:new(0));
      _ -> ?ann(Meta)
    end,

  {TClause, TS} =
    elixir_erl_clauses:clause(Ann, fun elixir_erl_pass:translate_args/3, Args, Body, Guards, S),

  case is_macro(Kind) of
    true ->
      FArgs = {var, Ann, '_@CALLER'},
      MClause = setelement(3, TClause, [FArgs | element(3, TClause)]),

      case TS#elixir_erl.caller of
        true  ->
          FBody = {'match', Ann,
            {'var', Ann, '__CALLER__'},
            ?remote(Ann, elixir_env, to_caller, [{var, Ann, '_@CALLER'}])
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

functions_form(Line, Module, Def, Defmacro, Exports, Body, Deprecated, Struct, MD5) ->
  {Spec, Info} = add_info_function(Line, Module, Def, Defmacro, Deprecated, Struct, MD5),
  [{attribute, Line, export, lists:usort([{'__info__', 1} | Exports])}, Spec, Info | Body].

add_info_function(Line, Module, Def, Defmacro, Deprecated, Struct, MD5) ->
  DefNA = [NA || {NA, _Meta} <- Def],
  DefmacroNA = [NA || {NA, _Meta} <- Defmacro],

  AllowedAttrs = [attributes, compile, functions, macros, md5, exports_md5, module, deprecated, struct],
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
      get_module_info(Module),
      functions_info(DefNA),
      macros_info(DefmacroNA),
      struct_info(Struct),
      exports_md5_info(MD5, DefNA, DefmacroNA, Struct),
      get_module_info(Module, attributes),
      get_module_info(Module, compile),
      get_module_info(Module, md5),
      deprecated_info(Deprecated)
    ]},

  {Spec, Info}.

get_module_info(Module) ->
  {clause, 0, [{atom, 0, module}], [], [{atom, 0, Module}]}.

exports_md5_info(MD5Attr, Def, Defmacro, Struct) ->
  MD5 = if
    is_binary(MD5Attr) -> MD5Attr;
    MD5Attr =:= nil -> elixir_module:exports_md5(Def, Defmacro, Struct)
  end,
  {clause, 0, [{atom, 0, exports_md5}], [], [elixir_to_erl(MD5)]}.

functions_info(Def) ->
  {clause, 0, [{atom, 0, functions}], [], [elixir_to_erl(Def)]}.

macros_info(Defmacro) ->
  {clause, 0, [{atom, 0, macros}], [], [elixir_to_erl(Defmacro)]}.

struct_info(nil) ->
  {clause, 0, [{atom, 0, struct}], [], [{atom, 0, nil}]};
struct_info(Fields) ->
  {clause, 0, [{atom, 0, struct}], [], [elixir_to_erl(Fields)]}.

get_module_info(Module, Key) ->
  Call = ?remote(0, erlang, get_module_info, [{atom, 0, Module}, {var, 0, 'Key'}]),
  {clause, 0, [{match, 0, {var, 0, 'Key'}, {atom, 0, Key}}], [], [Call]}.

deprecated_info(Deprecated) ->
  {clause, 0, [{atom, 0, deprecated}], [], [elixir_to_erl(Deprecated)]}.

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

  AllCallbacksWithoutSpecs = usort_callbacks([
    {{Kind, Name, Arity}, Meta} || {Kind, {Name, Arity}, Meta, _Spec} <- AllCallbacks
  ]),

  {Types, AllCallbacksWithoutSpecs, Forms3}.

usort_callbacks(Callbacks) ->
  % Sort and deduplicate callbacks. For duplicated callbacks we take
  % the one with earliest line.

  LineComparator = fun
    ({Callback1, Meta1}, {Callback1, Meta2}) -> ?line(Meta1) =< ?line(Meta2);
    ({Callback1, _Meta1}, {Callback2, _Meta2}) -> Callback1 =< Callback2
  end,

  UniqFun = fun({Callback, _Meta}) -> Callback end,

  lists:uniq(UniqFun, lists:sort(LineComparator, Callbacks)).

%% Types

types_form(Types, Forms) ->
  Fun = fun
    ({Kind, NameArity, Meta, Expr, true}, Acc) ->
      Line = ?line(Meta),
      [{attribute, Line, export_type, [NameArity]}, {attribute, Line, Kind, Expr} | Acc];
    ({Kind, _NameArity, Meta, Expr, false}, Acc) ->
      Line = ?line(Meta),
      [{attribute, Line, Kind, Expr} | Acc]
  end,

  lists:foldl(Fun, Forms, Types).

%% Specs and callbacks

validate_behaviour_info_and_attributes(#{definitions := Defs} = Map, AllCallbacks) ->
  case {lists:keyfind({behaviour_info, 1}, 1, Defs), AllCallbacks} of
    {false, _} ->
      ok;
    {_, [{Kind, {Name, Arity}, _, _} | _]} when Kind == callback; Kind == macrocallback ->
      file_error(Map, {callbacks_but_also_behaviour_info, {Kind, Name, Arity}});
    {_, _} ->
      ok
  end.

validate_optional_callbacks(Map, AllCallbacks, Optional) ->
  lists:foldl(fun(Callback, Acc) ->
    case Callback of
      {Name, Arity} when is_atom(Name) and is_integer(Arity) -> ok;
      _ -> file_error(Map, {ill_defined_optional_callback, Callback})
    end,

    case lists:keyfind(Callback, 2, AllCallbacks) of
      false -> file_error(Map, {unknown_callback, Callback});
      _ -> ok
    end,

    case Acc of
      #{Callback := _} -> file_error(Map, {duplicate_optional_callback, Callback});
      _ -> ok
    end,

    maps:put(Callback, true, Acc)
  end, #{}, Optional).

callspecs_form(_Kind, [], _Optional, _Macros, Forms, _ModuleMap) ->
  Forms;
callspecs_form(Kind, Entries, Optional, Macros, Forms, ModuleMap) ->
  #{unreachable := Unreachable} = ModuleMap,

  {SpecsMap, Signatures} =
    lists:foldl(fun({_, NameArity, Meta, Spec}, {Acc, NA}) ->
      Line = ?line(Meta),

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
  end, Forms, lists:usort(Signatures)).

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
    false -> file_error(#{anno => erl_anno:new(Line), file => File}, {spec_for_undefined_function, NameAndArity})
  end.

% Attributes

attributes_form(Line, Attributes, Forms) ->
  Fun = fun({Key, Value}, Acc) -> [{attribute, Line, Key, Value} | Acc] end,
  lists:foldr(Fun, Forms, Attributes).

% Loading forms

load_form(#{file := File, compile_opts := Opts} = Map, Prefix, Forms, Specs, Chunks) ->
  CompileOpts = extra_chunks_opts(Chunks, debug_opts(Map, Specs, Opts)),
  {_, Binary} = elixir_erl_compiler:noenv_forms(Prefix ++ Specs ++ Forms, File, CompileOpts),
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

docs_chunk(Map, Set, Module, Anno, Def, Defmacro, Types, Callbacks, ChunkOpts) ->
  #{file := File, attributes := Attributes} = Map,

  case elixir_config:get(docs) of
    true ->
      {ModuleDocLine, ModuleDoc} = get_moduledoc(erl_anno:line(Anno), Set),
      ModuleDocMeta = get_moduledoc_meta(Set),
      FunctionDocs = get_docs(Set, Module, Def, function),
      MacroDocs = get_docs(Set, Module, Defmacro, macro),
      CallbackDocs = get_callback_docs(Set, Callbacks),
      TypeDocs = get_type_docs(Set, Types),

      ModuleMeta = ModuleDocMeta#{
        source_path => elixir_utils:characters_to_list(File),
        source_annos => [Anno],
        behaviours => [Mod || {behaviour, Mod} <- Attributes]
      },

      DocsChunkData = term_to_binary({docs_v1,
        erl_anno:new(ModuleDocLine),
        elixir,
        <<"text/markdown">>,
        ModuleDoc,
        ModuleMeta,
        FunctionDocs ++ MacroDocs ++ CallbackDocs ++ TypeDocs
      }, [compressed | ChunkOpts]),

      [{<<"Docs">>, DocsChunkData}];

    false ->
      []
  end.

doc_value(Doc, Name) ->
  case Doc of
    false ->
      hidden;
    nil ->
      case erlang:atom_to_list(Name) of
        [$_ | _] -> hidden;
        _ -> none
      end;
    Doc ->
      #{<<"en">> => Doc}
  end.

get_moduledoc(Line, Set) ->
  case ets:lookup_element(Set, moduledoc, 2) of
    nil -> {Line, none};
    {DocLine, false} -> {DocLine, hidden};
    {DocLine, nil} -> {DocLine, none};
    {DocLine, Doc} -> {DocLine, #{<<"en">> => Doc}}
  end.

get_moduledoc_meta(Set) ->
  case ets:lookup(Set, {moduledoc, meta}) of
    [] -> #{};
    [{{moduledoc, meta}, Map}] when is_map(Map) -> Map
  end.

get_docs(Set, Module, Definitions, Kind) ->
  [{Key,
    erl_anno:new(Line),
    [signature_to_binary(Module, Name, Signature)],
    doc_value(Doc, Name),
    Meta#{source_annos => [?ann(DefinitionMeta)]}
   } || {{Name, Arity}, DefinitionMeta} <- Definitions,
        {Key, _Ctx, Line, Signature, Doc, Meta} <- ets:lookup(Set, {Kind, Name, Arity})].

get_callback_docs(Set, Callbacks) ->
  [{Key,
    erl_anno:new(Line),
    [],
    doc_value(Doc, Name),
    Meta#{source_annos => [?ann(DefinitionMeta)]}
   } || {{Kind, Name, Arity}, DefinitionMeta} <- Callbacks, {Key, Line, Doc, Meta} <- ets:lookup(Set, {Kind, Name, Arity})].

get_type_docs(Set, Types) ->
  [{Key,
    erl_anno:new(Line),
    [],
    doc_value(Doc, Name),
    Meta#{source_annos => [?ann(DefinitionMeta)]}
   } || {_Kind, {Name, Arity}, DefinitionMeta, _, true} <- Types,
        {Key, Line, Doc, Meta} <- ets:lookup(Set, {type, Name, Arity})].

signature_to_binary(_Module, Name, _Signature) when Name == '__aliases__'; Name == '__block__' ->
  <<(atom_to_binary(Name))/binary, "(args)">>;

signature_to_binary(_Module, fn, _Signature) ->
  <<"fn(clauses)">>;

signature_to_binary(_Module, Name, _Signature)
    when Name == '__CALLER__'; Name == '__DIR__'; Name == '__ENV__';
         Name == '__MODULE__'; Name == '__STACKTRACE__'; Name == '%{}' ->
  atom_to_binary(Name);

signature_to_binary(_Module, '%', _) ->
  <<"%struct{}">>;

signature_to_binary(Module, '__struct__', []) ->
  <<"%", ('Elixir.Kernel':inspect(Module))/binary, "{}">>;

signature_to_binary(_, Name, Signature) ->
  Quoted = {Name, [{closing, []}], Signature},
  Doc = 'Elixir.Inspect.Algebra':format('Elixir.Code':quoted_to_algebra(Quoted), infinity),
  'Elixir.IO':iodata_to_binary(Doc).

checker_chunk(Map, Def, Signatures, ChunkOpts) ->
  #{deprecated := Deprecated, defines_behaviour := DefinesBehaviour, attributes := Attributes} = Map,
  DeprecatedMap = maps:from_list(Deprecated),

  Exports =
    [begin
      Signature = maps:get(FA, Signatures, none),
      Info = case DeprecatedMap of
        #{FA := Reason} -> #{deprecated => Reason, sig => Signature};
        #{} -> #{sig => Signature}
      end,
      {FA, Info}
    end || {FA, _Meta} <- prepend_behaviour_info(DefinesBehaviour, Def)],

  Contents = #{
    exports => Exports,
    mode => case lists:keymember('__protocol__', 1, Attributes) of
      true -> protocol;
      false -> elixir
    end
  },

  checker_chunk(Contents, ChunkOpts).

prepend_behaviour_info(true, Def) -> [{{behaviour_info, 1}, []} | Def];
prepend_behaviour_info(false, Def) -> Def.

%% Errors

file_error(#{anno := Anno, file := File}, Error) ->
  Line = erl_anno:line(Anno),
  elixir_errors:file_error([{line, Line}], File, ?MODULE, Error).

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
