%% Compiler backend to Erlang.
-module(elixir_erl).
-export([elixir_to_erl/1, definition_to_anonymous/5, compile/1,
         get_ann/1, remote/4, add_beam_chunks/2, debug_info/4,
         definition_scope/2]).
-include("elixir.hrl").

%% TODO: Remove extra chunk functionality when OTP 20+.

add_beam_chunks(Bin, []) when is_binary(Bin) ->
  Bin;
add_beam_chunks(Bin, NewChunks) when is_binary(Bin), is_list(NewChunks) ->
  {ok, _, OldChunks} = beam_lib:all_chunks(Bin),
  Chunks = [{binary_to_list(K), V} || {K, V} <- NewChunks] ++ OldChunks,
  {ok, NewBin} = beam_lib:build_module(Chunks),
  NewBin.

%% debug_info callback

debug_info(elixir_v1, _Module, none, _Opts) ->
  {error, missing};
debug_info(elixir_v1, _Module, {elixir_v1, Map, _Specs}, _Opts) ->
  {ok, Map};
debug_info(erlang_v1, _Module, {elixir_v1, Map, Specs}, _Opts) ->
  {Prefix, Forms, _, _} = dynamic_form(Map),
  {ok, Prefix ++ Specs ++ Forms};
debug_info(core_v1, _Module, {elixir_v1, Map, Specs}, Opts) ->
  {Prefix, Forms, _, _} = dynamic_form(Map),
  #{compile_opts := CompileOpts} = Map,

  %% Do not rely on elixir_erl_compiler because we don't
  %% warnings nor the other functionality provided there.
  try compile:noenv_forms(Prefix ++ Specs ++ Forms, [core, return | CompileOpts] ++ Opts) of
    {ok, _, Core, _} -> {ok, Core};
    _What -> {error, failed_conversion}
  catch
    error:_ -> {error, failed_conversion}
  end;
debug_info(_, _, _, _) ->
  {error, unknown_format}.

%% Builds Erlang AST annotation.

get_ann(Opts) when is_list(Opts) ->
  get_ann(Opts, false, 0).

get_ann([{generated, true} | T], _, Line) -> get_ann(T, true, Line);
get_ann([{line, Line} | T], Gen, _) -> get_ann(T, Gen, Line);
get_ann([_ | T], Gen, Line) -> get_ann(T, Gen, Line);
get_ann([], Gen, Line) -> erl_anno:set_generated(Gen, Line).

%% Builds a remote call annotation.

remote(Ann, Module, Function, Args) when is_atom(Module), is_atom(Function), is_list(Args) ->
  {call, Ann,
    {remote, Ann, {atom, Ann, Module}, {atom, Ann, Function}},
    Args
  }.

%% Converts an Elixir definition to an anonymous function.

definition_to_anonymous(File, Module, Kind, Meta, Clauses) ->
  ErlClauses = [translate_clause(Kind, Clause, File) || Clause <- Clauses],
  Fun = {'fun', ?ann(Meta), {clauses, ErlClauses}},
  LocalHandler = fun(LocalName, LocalArgs) -> invoke_local(Module, LocalName, LocalArgs) end,
  {value, Result, _Binding} = erl_eval:expr(Fun, [], {value, LocalHandler}),
  Result.

invoke_local(Module, RawName, Args) ->
  %% If we have a macro, its arity in the table is
  %% actually one less than in the function call
  {Name, Arity} = case atom_to_list(RawName) of
    "MACRO-" ++ Rest -> {list_to_atom(Rest), length(Args) - 1};
    _ -> {RawName, length(Args)}
  end,

  case elixir_def:local_for(Module, Name, Arity, all) of
    false ->
      {current_stacktrace, [_ | T]} = erlang:process_info(self(), current_stacktrace),
      erlang:raise(error, undef, [{Module, Name, Arity, []} | T]);
    Fun ->
      apply(Fun, Args)
  end.

%% Converts Elixir quoted literals to Erlang AST.

elixir_to_erl(Tree) when is_tuple(Tree) ->
  {tuple, 0, [elixir_to_erl(X) || X <- tuple_to_list(Tree)]};
elixir_to_erl([]) ->
  {nil, 0};
elixir_to_erl(<<>>) ->
  {bin, 0, []};
elixir_to_erl(Tree) when is_list(Tree) ->
  elixir_to_erl_cons1(Tree, []);
elixir_to_erl(Tree) when is_atom(Tree) ->
  {atom, 0, Tree};
elixir_to_erl(Tree) when is_integer(Tree) ->
  {integer, 0, Tree};
elixir_to_erl(Tree) when is_float(Tree) ->
  {float, 0, Tree};
elixir_to_erl(Tree) when is_binary(Tree) ->
  %% Note that our binaries are UTF-8 encoded and we are converting
  %% to a list using binary_to_list. The reason for this is that Erlang
  %% considers a string in a binary to be encoded in latin1, so the bytes
  %% are not changed in any fashion.
  {bin, 0, [{bin_element, 0, {string, 0, binary_to_list(Tree)}, default, default}]};
elixir_to_erl(Function) when is_function(Function) ->
  case (erlang:fun_info(Function, type) == {type, external}) andalso
       (erlang:fun_info(Function, env) == {env, []}) of
    true ->
      {module, Module} = erlang:fun_info(Function, module),
      {name, Name}     = erlang:fun_info(Function, name),
      {arity, Arity}   = erlang:fun_info(Function, arity),

      {'fun', 0, {function,
        {atom, 0, Module},
        {atom, 0, Name},
        {integer, 0, Arity}}};
    false ->
      error(badarg)
  end;
elixir_to_erl(Pid) when is_pid(Pid) ->
  elixir_erl:remote(0, erlang, binary_to_term,
    [elixir_erl:elixir_to_erl(term_to_binary(Pid))]);
elixir_to_erl(_Other) ->
  error(badarg).

elixir_to_erl_cons1([H | T], Acc) -> elixir_to_erl_cons1(T, [H | Acc]);
elixir_to_erl_cons1(Other, Acc) -> elixir_to_erl_cons2(Acc, elixir_to_erl(Other)).

elixir_to_erl_cons2([H | T], Acc) ->
  elixir_to_erl_cons2(T, {cons, 0, elixir_to_erl(H), Acc});
elixir_to_erl_cons2([], Acc) ->
  Acc.

%% Returns a definition scope for translation.

definition_scope(Meta, File) ->
  %% TODO: We only need to do this dance because some
  %% warnings are raised in elixir_erl_pass. Once we remove
  %% all warnings from the Erlang pass, we can remove the
  %% file field from #elixir_erl and clean up the code.
  case lists:keyfind(location, 1, Meta) of
    {location, {F, _}} -> #elixir_erl{file=F};
    false -> #elixir_erl{file=File}
  end.

%% Compilation hook.

compile(#{module := Module} = Map) ->
  Data = elixir_module:data_table(Module),
  {Prefix, Forms, Defmacro, Unreachable} = dynamic_form(Map),
  Specs =
    case elixir_compiler:get_opt(internal) of
      true -> [];
      false -> specs_form(Data, Defmacro, Unreachable, types_form(Data, []))
    end,
  load_form(Map, Data, Prefix, Forms, Specs).

dynamic_form(#{module := Module, line := Line, file := File, attributes := Attributes,
               definitions := Definitions, unreachable := Unreachable}) ->
  {Def, Defmacro, Macros, Exports, Functions} =
    split_definition(Definitions, File, Unreachable, [], [], [], [], {[], []}),

  Location = {elixir_utils:characters_to_list(elixir_utils:relative_to_cwd(File)), Line},
  Prefix = [{attribute, Line, file, Location},
            {attribute, Line, module, Module},
            {attribute, Line, compile, no_auto_import}],

  Forms0 = functions_form(Line, Module, Def, Defmacro, Exports, Functions),
  Forms1 = attributes_form(Line, Attributes, Forms0),
  {Prefix, Forms1, Macros, Unreachable}.

% Definitions

split_definition([{Tuple, Kind, Meta, Clauses} | T], File, Unreachable,
                 Def, Defmacro, Macros, Exports, Functions) ->
  case lists:member(Tuple, Unreachable) of
    false ->
      split_definition(Tuple, Kind, Meta, Clauses, T, File, Unreachable,
                       Def, Defmacro, Macros, Exports, Functions);
    true ->
      split_definition(T, File, Unreachable, Def, Defmacro, Macros, Exports, Functions)
  end;
split_definition([], _File, _Unreachable, Def, Defmacro, Macros, Exports, {Head, Tail}) ->
  {Def, Defmacro, Macros, Exports, Head ++ Tail}.

split_definition(Tuple, def, Meta, Clauses, T, File, Unreachable,
                 Def, Defmacro, Macros, Exports, Functions) ->
  {_, _, N, A, _} = Entry = translate_definition(def, Meta, File, Tuple, Clauses),
  split_definition(T, File, Unreachable, [Tuple | Def], Defmacro, Macros, [{N, A} | Exports],
                   add_definition(Meta, Entry, Functions));

split_definition(Tuple, defp, Meta, Clauses, T, File, Unreachable,
                 Def, Defmacro, Macros, Exports, Functions) ->
  Entry = translate_definition(defp, Meta, File, Tuple, Clauses),
  split_definition(T, File, Unreachable, Def, Defmacro, Macros, Exports,
                   add_definition(Meta, Entry, Functions));

split_definition(Tuple, defmacro, Meta, Clauses, T, File, Unreachable,
                 Def, Defmacro, Macros, Exports, Functions) ->
  {_, _, N, A, _} = Entry = translate_definition(defmacro, Meta, File, Tuple, Clauses),
  split_definition(T, File, Unreachable, Def, [Tuple | Defmacro], [Tuple | Macros], [{N, A} | Exports],
                   add_definition(Meta, Entry, Functions));

split_definition(Tuple, defmacrop, Meta, Clauses, T, File, Unreachable,
                 Def, Defmacro, Macros, Exports, Functions) ->
  Entry = translate_definition(defmacro, Meta, File, Tuple, Clauses),
  split_definition(T, File, Unreachable, Def, Defmacro, [Tuple | Macros], Exports,
                   add_definition(Meta, Entry, Functions)).

add_definition(Meta, Body, {Head, Tail}) ->
  case lists:keyfind(location, 1, Meta) of
    {location, {F, L}} ->
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

translate_definition(Kind, Meta, File, {Name, Arity}, Clauses) ->
  ErlClauses = [translate_clause(Kind, Clause, File) || Clause <- Clauses],

  case is_macro(Kind) of
    true -> {function, ?ann(Meta), elixir_utils:macro_name(Name), Arity + 1, ErlClauses};
    false -> {function, ?ann(Meta), Name, Arity, ErlClauses}
  end.

translate_clause(Kind, {Meta, Args, Guards, Body}, File) ->
  S = definition_scope(Meta, File),

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
            elixir_erl:remote(Ann, elixir_env, linify, [{var, Ann, '_@CALLER'}])
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

functions_form(Line, Module, Def, Defmacro, Exports, Body) ->
  {Spec, Info} = add_info_function(Line, Module, Def, Defmacro),
  [{attribute, Line, export, lists:sort([{'__info__', 1} | Exports])}, Spec, Info | Body].

add_info_function(Line, Module, Def, Defmacro) ->
  AllowedArgs =
    lists:map(fun(Atom) -> {atom, Line, Atom} end,
              [attributes, compile, exports, functions, macros, md5, module]),

  Spec =
    {attribute, Line, spec, {{'__info__', 1},
      [{type, Line, 'fun', [
        {type, Line, product, [
          {type, Line, union, AllowedArgs}
        ]},
        {type, Line, union, [
          {type, Line, atom, []},
          {type, Line, list, [
            {type, Line, union, [
              {type, Line, tuple, [
                {type, Line, atom, []},
                {type, Line, any, []}
              ]},
              {type, Line, tuple, [
                {type, Line, atom, []},
                {type, Line, byte, []},
                {type, Line, integer, []}
              ]}
            ]}
          ]}
        ]}
      ]}]
    }},

  Info =
    {function, 0, '__info__', 1, [
      functions_info(Def),
      macros_info(Defmacro),
      others_info(Module)
    ]},

  {Spec, Info}.

functions_info(Def) ->
  {clause, 0, [{atom, 0, functions}], [], [elixir_erl:elixir_to_erl(lists:sort(Def))]}.

macros_info(Defmacro) ->
  {clause, 0, [{atom, 0, macros}], [], [elixir_erl:elixir_to_erl(lists:sort(Defmacro))]}.

others_info(Module) ->
  Info = {call, 0,
            {remote, 0, {atom, 0, erlang}, {atom, 0, get_module_info}},
            [{atom, 0, Module}, {var, 0, info}]},
  {clause, 0, [{var, 0, info}], [], [Info]}.

% Types

types_form(Data, Forms) ->
  ExTypes =
    take_type_spec(Data, type) ++ take_type_spec(Data, typep) ++ take_type_spec(Data, opaque),

  Types =
    ['Elixir.Kernel.Typespec':translate_type(Kind, Expr, Caller) || {Kind, Expr, Caller} <- ExTypes],

  Fun = fun
    ({{Kind, NameArity, Expr}, Line, true}, Acc) ->
      [{attribute, Line, export_type, [NameArity]}, {attribute, Line, Kind, Expr} | Acc];
    ({{Kind, _NameArity, Expr}, Line, false}, Acc) ->
      [{attribute, Line, Kind, Expr} | Acc]
  end,

  lists:foldl(Fun, Forms, Types).

% Specs

specs_form(Data, Defmacro, Unreachable, Forms) ->
  Specs =
    ['Elixir.Kernel.Typespec':translate_spec(Kind, Expr, Caller) ||
     {Kind, Expr, Caller} <- take_type_spec(Data, spec)],

  Callbacks =
    ['Elixir.Kernel.Typespec':translate_spec(Kind, Expr, Caller) ||
     {Kind, Expr, Caller} <- take_type_spec(Data, callback)],

  Macrocallbacks =
    ['Elixir.Kernel.Typespec':translate_spec(Kind, Expr, Caller) ||
     {Kind, Expr, Caller} <- take_type_spec(Data, macrocallback)],

  Optional = lists:flatten(take_type_spec(Data, optional_callbacks)),
  SpecsForms = specs_form(spec, Specs, Unreachable, [], Defmacro, Forms),
  specs_form(callback, Callbacks ++ Macrocallbacks, [], Optional,
             [NameArity || {{_, NameArity, _}, _} <- Macrocallbacks], SpecsForms).

specs_form(_Kind, [], _Unreacheable, _Optional, _Macros, Forms) ->
  Forms;
specs_form(Kind, Entries, Unreachable, Optional, Macros, Forms) ->
  Map =
    lists:foldl(fun({{_, NameArity, Spec}, Line}, Acc) ->
      case lists:member(NameArity, Unreachable) of
        false ->
          case Acc of
            #{NameArity := List} -> Acc#{NameArity := [{Spec, Line} | List]};
            #{} -> Acc#{NameArity => [{Spec, Line}]}
          end;
        true ->
          Acc
      end
    end, #{}, Entries),

  maps:fold(fun(NameArity, ExprsLines, Acc) ->
    {Exprs, Lines} = lists:unzip(lists:reverse(ExprsLines)),
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
        [{attribute, Line, Kind, {Key, Value}},
         {attribute, Line, optional_callbacks, [Key]} | Acc];
      false ->
        [{attribute, Line, Kind, {Key, Value}} | Acc]
    end
  end, Forms, Map).

spec_for_macro({type, Line, 'fun', [{type, _, product, Args} | T]}) ->
  NewArgs = [{type, Line, term, []} | Args],
  {type, Line, 'fun', [{type, Line, product, NewArgs} | T]};
spec_for_macro(Else) ->
  Else.

take_type_spec(Data, Key) ->
  case ets:take(Data, Key) of
    [{Key, Value, _, _}] -> Value;
    [] -> []
  end.

% Attributes

attributes_form(Line, Attributes, Forms) ->
  Fun = fun({Key, Value}, Acc) ->
    [{attribute, Line, Key, Value} | Acc]
  end,
  lists:foldl(Fun, Forms, Attributes).

% Loading forms

load_form(#{line := Line, file := File, compile_opts := Opts} = Map, Data, Prefix, Forms, Specs) ->
  {ExtraChunks, CompileOpts} = extra_chunks(Data, Line, debug_opts(Map, Specs, Opts)),
  {_, Binary} = elixir_erl_compiler:forms(Prefix ++ Specs ++ Forms, File, CompileOpts),
  add_beam_chunks(Binary, ExtraChunks).

debug_opts(Map, Specs, Opts) ->
  case {supports_debug_tuple(), include_debug_opts(Opts)} of
    {true, true} -> [{debug_info, {?MODULE, {elixir_v1, Map, Specs}}}];
    {true, false} -> [{debug_info, {?MODULE, none}}];
    {false, true} -> [debug_info];
    {false, false} -> []
  end.

include_debug_opts(Opts) ->
  case proplists:get_value(debug_info, Opts) of
    true -> true;
    false -> false;
    undefined -> elixir_compiler:get_opt(debug_info)
  end.

supports_debug_tuple() ->
  case erlang:system_info(otp_release) of
    "18" -> false;
    "19" -> false;
    _ -> true
  end.

extra_chunks(Data, Line, Opts) ->
  Supported = supports_extra_chunks_option(),
  case docs_chunk(Data, Line, elixir_compiler:get_opt(docs)) of
    [] -> {[], Opts};
    Chunks when Supported -> {[], [{extra_chunks, Chunks} | Opts]};
    Chunks -> {Chunks, Opts}
  end.

supports_extra_chunks_option() ->
  case erlang:system_info(otp_release) of
    "18" -> false;
    "19" -> false;
    _ -> true
  end.

docs_chunk(Data, Line, true) ->
  ChunkData = term_to_binary({elixir_docs_v1, [
    {docs, get_docs(Data)},
    {moduledoc, get_moduledoc(Line, Data)},
    {callback_docs, get_callback_docs(Data)},
    {type_docs, get_type_docs(Data)}
  ]}, [compressed]),
  [{<<"ExDc">>, ChunkData}];
docs_chunk(_, _, _) ->
  [].

get_moduledoc(Line, Data) ->
  case ets:lookup_element(Data, moduledoc, 2) of
    nil -> {Line, nil};
    {DocLine, Doc} -> {DocLine, Doc}
  end.

get_docs(Data) ->
  lists:usort(ets:select(Data, [{{{doc, '$1'}, '$2', '$3', '$4', '$5'},
                                 [], [{{'$1', '$2', '$3', '$4', '$5'}}]}])).

get_callback_docs(Data) ->
  lists:usort(ets:select(Data, [{{{callbackdoc, '$1'}, '$2', '$3', '$4'},
                                 [], [{{'$1', '$2', '$3', '$4'}}]}])).

get_type_docs(Data) ->
  lists:usort(ets:select(Data, [{{{typedoc, '$1'}, '$2', '$3', '$4'},
                                 [], [{{'$1', '$2', '$3', '$4'}}]}])).
