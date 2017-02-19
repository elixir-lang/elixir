%% Compiler backend to Erlang.
-module(elixir_erl).
-export([elixir_to_erl/1, definition_to_anonymous/6, compile/4,
         get_ann/1, remote/4, add_beam_chunk/3]).
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
  get_ann(Opts, [], 0).

get_ann([{generated, Gen} | T], Acc, Line) -> get_ann(T, [{generated, Gen} | Acc], Line);
get_ann([{line, Line} | T], Acc, _) -> get_ann(T, Acc, Line);
get_ann([_ | T], Acc, Line) -> get_ann(T, Acc, Line);
get_ann([], [], Line) -> Line;
get_ann([], Acc, Line) -> [{location, Line} | Acc].

%% Builds a remote call annotation.

remote(Ann, Module, Function, Args) when is_atom(Module), is_atom(Function), is_list(Args) ->
  {call, Ann,
    {remote, Ann, {atom, Ann, Module}, {atom, Ann, Function}},
    Args
  }.

%% Retrieves an anonymous function.

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

compile(File, _Module, Functions, Unreachable) ->
  split_definition(Functions, File, Unreachable, [], [], [], [], [], {[], []}).

split_definition([{Tuple, def, Meta, Clauses} | T], File, Unreachable,
                 Def, Defp, Defmacro, Defmacrop, Exports, Functions) ->
  {_, _, N, A, _} = Function = function_form(def, Meta, File, Tuple, Clauses),
  split_definition(T, File, Unreachable, [Tuple | Def], Defp, Defmacro, Defmacrop,
                   [{N, A} | Exports],
                   add_definition(Meta, Function, Functions));

split_definition([{Tuple, defp, Meta, Clauses} | T], File, Unreachable,
                 Def, Defp, Defmacro, Defmacrop, Exports, Functions) ->
  Function = function_form(defp, Meta, File, Tuple, Clauses),
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
  {_, _, N, A, _} = Function = function_form(defmacro, Meta, File, Tuple, Clauses),
  split_definition(T, File, Unreachable, Def, Defp, [Tuple | Defmacro], Defmacrop,
                   [{N, A} | Exports],
                   add_definition(Meta, Function, Functions));

split_definition([{Tuple, defmacrop, Meta, Clauses} | T], File, Unreachable,
                 Def, Defp, Defmacro, Defmacrop, Exports, Functions) ->
  _ = function_form(defp, Meta, File, Tuple, Clauses),
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

function_form(Kind, Meta, File, {Name, Arity}, Clauses) ->
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
