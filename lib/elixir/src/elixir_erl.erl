%% Compiler backend to Erlang.
-module(elixir_erl).
-export([elixir_to_erl/1, definition_to_anonymous/6, compile/7,
         get_ann/1, remote/4, add_beam_chunk/3, format_error/1]).
-include("elixir.hrl").

%% Adds custom chunk to a .beam binary

add_beam_chunk(Bin, Id, ChunkData)
        when is_binary(Bin), is_list(Id), is_binary(ChunkData) ->
  {ok, _, Chunks0} = beam_lib:all_chunks(Bin),
  NewChunk = {Id, ChunkData},
  Chunks = [NewChunk | Chunks0],
  {ok, NewBin} = beam_lib:build_module(Chunks),
  NewBin.

%% Builds Erlang AST annotation.

get_ann(Opts) when is_list(Opts) ->
  get_ann(Opts, false, 0).

get_ann([{generated, true} | T], _, Line) -> get_ann(T, true, Line);
get_ann([{line, Line} | T], Gen, _) -> get_ann(T, Gen, Line);
get_ann([_ | T], Gen, Line) -> get_ann(T, Gen, Line);
%% TODO: Remove next clause when we no longer support Erlang 18.
get_ann([], _, Line) when Line < 0 -> Line;
get_ann([], Gen, Line) -> erl_anno:set_generated(Gen, Line).

%% Builds a remote call annotation.

remote(Ann, Module, Function, Args) when is_atom(Module), is_atom(Function), is_list(Args) ->
  {call, Ann,
    {remote, Ann, {atom, Ann, Module}, {atom, Ann, Function}},
    Args
  }.

%% Converts an Elixir definition to an anonymous function.

definition_to_anonymous(File, Module, {Name, Arity}, Kind, Meta, Clauses) ->
  ErlClauses = [translate_clause(Kind, Name, Arity, Clause, File) || Clause <- Clauses],
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

%% Compilation hook.

compile(Line, File, Module, Attributes, Definitions, Unreachable, Opts) ->
  Data = elixir_module:data_table(Module),

  {Def, Defp, Defmacro, Defmacrop, Exports, Functions} =
    split_definition(Definitions, File, Unreachable, [], [], [], [], [], {[], []}),

  Forms0 =
    functions_form(Line, File, Module, Def, Defp, Defmacro, Defmacrop, Exports, Functions),
  Forms1 =
    attributes_form(Line, Attributes, Forms0),
  Forms2 =
    case elixir_compiler:get_opt(internal) of
      true -> Forms1;
      false -> specs_form(Data, Defmacro, Unreachable, types_form(Data, Forms1))
    end,

  Location = {elixir_utils:characters_to_list(elixir_utils:relative_to_cwd(File)), Line},

  Forms = [{attribute, Line, file, Location},
           {attribute, Line, module, Module} | Forms2],

  load_form(Line, File, Data, Forms, Opts).

% Definitions

split_definition([{Tuple, def, Meta, Clauses} | T], File, Unreachable,
                 Def, Defp, Defmacro, Defmacrop, Exports, Functions) ->
  {_, _, N, A, _} = Function = translate_definition(def, Meta, File, Tuple, Clauses),
  split_definition(T, File, Unreachable, [Tuple | Def], Defp, Defmacro, Defmacrop,
                   [{N, A} | Exports], add_definition(Meta, Function, Functions));

split_definition([{Tuple, defp, Meta, Clauses} | T], File, Unreachable,
                 Def, Defp, Defmacro, Defmacrop, Exports, Functions) ->
  Function = translate_definition(defp, Meta, File, Tuple, Clauses),
  case lists:member(Tuple, Unreachable) of
    false ->
      split_definition(T, File, Unreachable, Def, [Tuple | Defp], Defmacro, Defmacrop,
                       Exports, add_definition(Meta, Function, Functions));
    true ->
      split_definition(T, File, Unreachable, Def, [Tuple | Defp], Defmacro, Defmacrop,
                       Exports, Functions)
  end;

split_definition([{Tuple, defmacro, Meta, Clauses} | T], File, Unreachable,
                 Def, Defp, Defmacro, Defmacrop, Exports, Functions) ->
  {_, _, N, A, _} = Function = translate_definition(defmacro, Meta, File, Tuple, Clauses),
  split_definition(T, File, Unreachable, Def, Defp, [Tuple | Defmacro], Defmacrop,
                   [{N, A} | Exports], add_definition(Meta, Function, Functions));

split_definition([{Tuple, defmacrop, _Meta, _Clauses} | T], File, Unreachable,
                 Def, Defp, Defmacro, Defmacrop, Exports, Functions) ->
  split_definition(T, File, Unreachable, Def, [Tuple | Defp], Defmacro, Defmacrop,
                   Exports, Functions);

split_definition([], _File, _Unreachable, Def, Defp, Defmacro, Defmacrop, Exports, {Head, Tail}) ->
  {Def, Defp, Defmacro, Defmacrop, Exports, Head ++ Tail}.

add_definition(Meta, Body, {Head, Tail}) ->
  case lists:keyfind(location, 1, Meta) of
    {location, {F, L}} ->
      Attr = {attribute, ?ann(Meta), file, {elixir_utils:characters_to_list(F), L}},
      {Head, [Attr, Body | Tail]};
    false ->
      {[Body | Head], Tail}
  end.

translate_definition(Kind, Meta, File, {Name, Arity}, Clauses) ->
  ErlClauses = [translate_clause(Kind, Name, Arity, Clause, File) || Clause <- Clauses],

  case is_macro(Kind) of
    true -> {function, ?ann(Meta), elixir_utils:macro_name(Name), Arity + 1, ErlClauses};
    false -> {function, ?ann(Meta), Name, Arity, ErlClauses}
  end.

translate_clause(Kind, Name, Arity, {Meta, Args, Guards, Body}, File) ->
  S =
    %% TODO: We only need to do this dance because some
    %% warnings are raised in elixir_erl_pass. Once we remove
    %% all warnings from the Erlang pass, we can remove the
    %% file field from #elixir_erl and clean up the code.
    case lists:keyfind(location, 1, Meta) of
      {location, {F, _}} -> #elixir_erl{def = {Kind, Name, Arity}, file = F};
      false -> #elixir_erl{def = {Kind, Name, Arity}, file = File}
    end,

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

functions_form(Line, File, Module, Def, Defp, Defmacro, Defmacrop, Exports, Body) ->
  Pair = {'__info__', 1},
  case lists:member(Pair, Def) or lists:member(Pair, Defp) or
       lists:member(Pair, Defmacro) or lists:member(Pair, Defmacrop) of
    true  ->
      elixir_errors:form_error([{line, Line}], File, ?MODULE, {internal_function_overridden, Pair});
    false ->
      {Spec, Info} = add_info_function(Line, Module, Def, Defmacro),
      [{attribute, Line, export, lists:sort([{'__info__', 1} | Exports])}, Spec, Info | Body]
  end.

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
  Types0 = take_type_spec(Data, type) ++
           take_type_spec(Data, typep) ++
           take_type_spec(Data, opaque),
  Types1 = ['Elixir.Kernel.Typespec':translate_type(Kind, Expr, Caller) ||
            {Kind, Expr, Caller} <- Types0],

  Fun = fun
    ({{Kind, NameArity, Expr}, Line, true}, Acc) ->
      [{attribute, Line, export_type, [NameArity]}, {attribute, Line, Kind, Expr} | Acc];
    ({{Kind, _NameArity, Expr}, Line, false}, Acc) ->
      [{attribute, Line, Kind, Expr} | Acc]
  end,
  lists:foldl(Fun, Forms, Types1).

take_type_spec(Data, Key) ->
  case ets:take(Data, Key) of
    [{Key, Value, _, _}] -> Value;
    [] -> []
  end.

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

% Attributes

attributes_form(Line, Attributes, Forms) ->
  Fun = fun({Key, Value}, Acc) ->
    [{attribute, Line, Key, Value} | Acc]
  end,
  lists:foldl(Fun, Forms, Attributes).

% Loading forms

load_form(Line, File, Data, Forms, Opts) ->
  DebugInfo =
    case proplists:get_value(debug_info, Opts) of
      true -> [debug_info];
      false -> [];
      undefined ->
        case elixir_compiler:get_opt(debug_info) of
          true  -> [debug_info];
          false -> []
        end
    end,

  {_, Binary} = elixir_erl_compiler:forms(Forms, File, DebugInfo),
  Docs = elixir_compiler:get_opt(docs),
  add_docs_chunk(Binary, Data, Line, Docs).

add_docs_chunk(Bin, Data, Line, true) ->
  ChunkData = term_to_binary({elixir_docs_v1, [
    {docs, get_docs(Data)},
    {moduledoc, get_moduledoc(Line, Data)},
    {callback_docs, get_callback_docs(Data)},
    {type_docs, get_type_docs(Data)}
  ]}),
  add_beam_chunk(Bin, "ExDc", ChunkData);
add_docs_chunk(Bin, _, _, _) -> Bin.

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

%% Errors

format_error({internal_function_overridden, {Name, Arity}}) ->
  io_lib:format("function ~ts/~B is internal and should not be overridden", [Name, Arity]).
