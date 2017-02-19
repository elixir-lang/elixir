%% Compiler backend to Erlang.
-module(elixir_erl).
-export([definition_to_anonymous/6, split_definition/4]).
-include("elixir.hrl").

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

split_definition(File, _Module, Functions, Unreachable) ->
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
    %% warnings are raised in elixir_scope. Once we remove
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
            elixir_utils:erl_call(Ann, elixir_env, linify, [{var, Ann, '_@CALLER'}])
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