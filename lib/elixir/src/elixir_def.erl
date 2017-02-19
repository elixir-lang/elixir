% Holds the logic responsible for function definitions (def(p) and defmacro(p)).
-module(elixir_def).
-export([setup/1, reset_last/1, lookup_definition/2,
  take_definition/2, store_definition/6, unwrap_definitions/2,
  store_definition/9, local_for/4, format_error/1]).
-include("elixir.hrl").
-define(last_def, {elixir, last_def}).

setup(Module) ->
  reset_last(Module),
  ok.

reset_last(Module) ->
  ets:insert(elixir_module:data_table(Module), {?last_def, []}).

lookup_definition(Module, Tuple) ->
  Table = elixir_module:defs_table(Module),
  case ets:lookup(Table, {def, Tuple}) of
    [Result] ->
      {Result, [Clause || {_, Clause} <- ets:lookup(Table, {clauses, Tuple})]};
    _ ->
      false
  end.

take_definition(Module, Tuple) ->
  Table = elixir_module:defs_table(Module),
  case ets:take(Table, {def, Tuple}) of
    [Result] ->
      {Result, [Clause || {_, Clause} <- ets:take(Table, {clauses, Tuple})]};
    [] ->
      false
  end.

%% Temporarily here

local_for(Module, Name, Arity, Kinds) ->
  Tuple = {Name, Arity},
  try lookup_definition(Module, Tuple) of
    {{_, Kind, Meta, File, _, _}, Clauses} ->
      case (Kinds == all) orelse (lists:member(Kind, Kinds)) of
        true ->
          function_to_anonymous(
            Module, function_for_stored_definition(Kind, Meta, File, Tuple, Clauses)
          );
        false ->
          false
      end;
    false ->
      false
  catch
    error:badarg -> false
  end.

function_to_anonymous(Module, {function, Ann, _Name, _Arity, Clauses}) ->
  Fun = {'fun', Ann, {clauses, Clauses}},
  LocalHandler = {value, fun(Name, Args) -> invoke_local(Module, Name, Args) end},
  {value, Result, _Binding} = erl_eval:expr(Fun, [], LocalHandler),
  Result.

invoke_local(Module, RawName, Args) ->
  %% If we have a macro, its arity in the table is
  %% actually one less than in the function call
  {Name, Arity} = case atom_to_list(RawName) of
    "MACRO-" ++ Rest -> {list_to_atom(Rest), length(Args) - 1};
    _ -> {RawName, length(Args)}
  end,

  case local_for(Module, Name, Arity, all) of
    false ->
      {current_stacktrace, [_ | T]} = erlang:process_info(self(), current_stacktrace),
      erlang:raise(error, undef, [{Module, Name, Arity, []} | T]);
    Fun ->
      apply(Fun, Args)
  end.

%% Section for storing definitions

store_definition(Line, Kind, CheckClauses, Call, Body, Pos) when is_integer(Line) ->
  E = (elixir_locals:get_cached_env(Pos))#{line := Line},
  {NameAndArgs, Guards} = elixir_clauses:extract_guards(Call),

  {Name, Args} = case NameAndArgs of
    {N, _, A} when is_atom(N), is_atom(A) -> {N, []};
    {N, _, A} when is_atom(N), is_list(A) -> {N, A};
    _ -> elixir_errors:form_error([{line, Line}], ?m(E, file), ?MODULE,
                                  {invalid_def, Kind, NameAndArgs})
  end,

  %% Now that we have verified the call format,
  %% extract meta information like file and context.
  {_, Meta, _} = Call,
  DoCheckClauses = (not lists:keymember(context, 1, Meta)) andalso (CheckClauses),

  %% Check if there is a file information in the definition.
  %% If so, we assume this come from another source and
  %% we need to linify taking into account keep line numbers.
  %%
  %% Line and File will always point to the caller. __ENV__.line
  %% will always point to the quoted one and __ENV__.file will
  %% always point to the one at @file or the quoted one.
  {Location, Key} =
    case elixir_utils:meta_location(Meta) of
      {_, _} = Keep -> {Keep, keep};
      nil -> {nil, line}
    end,

  Arity        = length(Args),
  LinifyArgs   = elixir_quote:linify(Line, Key, Args),
  LinifyGuards = elixir_quote:linify(Line, Key, Guards),
  LinifyBody   = elixir_quote:linify(Line, Key, Body),

  {EL, MetaLocation} =
    case retrieve_location(Location, ?m(E, module)) of
      {F, L} ->
        {E#{file := F}, [{line, Line}, {location, {F, L}}, {generated, DoCheckClauses}]};
      nil ->
        {E, [{line, Line}, {generated, DoCheckClauses}]}
    end,

  assert_no_aliases_name(MetaLocation, Name, Args, EL),
  assert_valid_name(MetaLocation, Kind, Name, Args, EL),
  store_definition(MetaLocation, Kind, DoCheckClauses, Name, Arity,
                   LinifyArgs, LinifyGuards, LinifyBody, ?m(E, file), EL).

store_definition(Meta, Kind, CheckClauses, Name, Arity, DefaultsArgs, Guards, Body, File, ER) ->
  Module = ?m(ER, module),
  Tuple = {Name, Arity},
  E = ER#{function := Tuple},

  elixir_locals:record_definition(Tuple, Kind, Module),
  {Args, Defaults} = elixir_def_defaults:unpack(Kind, Meta, Name, DefaultsArgs, E),
  Clauses = [elixir_exp_clauses:def(Clause, E) ||
             Clause <- def_to_clauses(Kind, Meta, Args, Guards, Body, E)],

  DefaultsLength = length(Defaults),
  elixir_locals:record_defaults(Tuple, Kind, Module, DefaultsLength),
  check_previous_defaults(Meta, Module, Name, Arity, Kind, DefaultsLength, E),
  run_on_definition_callbacks(Kind, Module, Name, DefaultsArgs, Guards, Body, E),

  store_definition(CheckClauses, Kind, Meta, Name, Arity, File,
                   Module, DefaultsLength, Clauses),
  [store_definition(false, Kind, Meta, Name, length(DefaultArgs), File,
                    Module, 0, [Default]) || {_, DefaultArgs, _, _} = Default <- Defaults],
  Tuple.

retrieve_location(Location, Module) ->
  case ets:take(elixir_module:data_table(Module), file) of
    [] when is_tuple(Location) ->
      {File, Line} = Location,
      {elixir_utils:relative_to_cwd(File), Line};
    [] ->
      nil;
    [{file, File, _, _}] when is_binary(File) ->
      'Elixir.Module':delete_attribute(Module, file),
      {elixir_utils:relative_to_cwd(File), 0};
    [{file, {File, Line}, _, _}] when is_binary(File) andalso is_integer(Line) ->
      'Elixir.Module':delete_attribute(Module, file),
      {elixir_utils:relative_to_cwd(File), Line}
  end.

def_to_clauses(_Kind, Meta, Args, [], nil, E) ->
  check_args_for_bodyless_clause(Meta, Args, E),
  [];
def_to_clauses(Kind, Meta, _Args, _Guards, nil, E) ->
  elixir_errors:form_error(Meta, ?m(E, file), ?MODULE, {missing_do, Kind});
def_to_clauses(_Kind, Meta, Args, Guards, [{do, Body}], _E) ->
  [{Meta, Args, Guards, Body}];
def_to_clauses(_Kind, Meta, Args, Guards, Body, _E) ->
  [{Meta, Args, Guards, {'try', Meta, [Body]}}].

run_on_definition_callbacks(Kind, Module, Name, Args, Guards, Body, E) ->
  Callbacks = ets:lookup_element(elixir_module:data_table(Module), on_definition, 2),
  _ = [Mod:Fun(E, Kind, Name, Args, Guards, Body) || {Mod, Fun} <- Callbacks],
  ok.

%% Section for unwrapping definitions for module definition.

% Unwrap the functions stored in the functions table.
% It returns a list of all functions to be exported, plus the macros,
% and the body of all functions.
unwrap_definitions(File, Module) ->
  Table = elixir_module:defs_table(Module),
  Entries = ets:match(Table, {{def, '$1'}, '_', '_', '_', '_', '_'}),
  {All, Private} = unwrap_definition(lists:sort(Entries), File, Module, Table, [], []),
  Unreachable = elixir_locals:warn_unused_local(File, Module, Private),
  split_definition(All, Unreachable, [], [], [], [], [], {[], []}).

unwrap_definition([[Tuple] | T], File, Module, Table, All, Private) ->
  [{_, Kind, Meta, _, Check, {Defaults, _, _}}] = ets:lookup(Table, {def, Tuple}),

  try ets:lookup_element(Table, {clauses, Tuple}, 2) of
    Clauses ->
      Unwrapped = {Tuple, Kind, Meta,
                   function_for_stored_definition(Kind, Meta, File, Tuple, Clauses)},

      NewPrivate =
        if
          Kind == defp; Kind == defmacrop ->
            WarnMeta = case Check of true -> Meta; false -> false end,
            [{Tuple, Kind, WarnMeta, Defaults} | Private];
          true ->
            Private
        end,

      unwrap_definition(T, File, Module, Table, [Unwrapped | All], NewPrivate)
  catch
    error:badarg ->
      warn_bodyless_function(Check, Meta, File, Module, Kind, Tuple),
      unwrap_definition(T, File, Module, Table, All, Private)
  end;

unwrap_definition([], _File, _Module, _Table, All, Private) ->
  {All, Private}.

split_definition([{Tuple, def, Meta, {_, _, N, A, _} = Body} | T], Unreachable,
                 Def, Defp, Defmacro, Defmacrop, Exports, Functions) ->
  split_definition(T, Unreachable, [Tuple | Def], Defp, Defmacro, Defmacrop,
                   [{N, A} | Exports],
                   add_definition(Meta, Body, Functions));

split_definition([{Tuple, defp, Meta, Body} | T], Unreachable,
                 Def, Defp, Defmacro, Defmacrop, Exports, Functions) ->
  case lists:member(Tuple, Unreachable) of
    false ->
      split_definition(T, Unreachable, Def, [Tuple | Defp], Defmacro, Defmacrop,
                       Exports, add_definition(Meta, Body, Functions));
    true ->
      split_definition(T, Unreachable, Def, [Tuple | Defp], Defmacro, Defmacrop,
                       Exports, Functions)
  end;

split_definition([{Tuple, defmacro, Meta, {_, _, N, A, _} = Body} | T], Unreachable,
                 Def, Defp, Defmacro, Defmacrop, Exports, Functions) ->
  split_definition(T, Unreachable, Def, Defp, [Tuple | Defmacro], Defmacrop,
                   [{N, A} | Exports],
                   add_definition(Meta, Body, Functions));

split_definition([{Tuple, defmacrop, _Meta, _Body} | T], Unreachable,
                 Def, Defp, Defmacro, Defmacrop, Exports, Functions) ->
  split_definition(T, Unreachable, Def, [Tuple | Defp], Defmacro, Defmacrop,
                   Exports, Functions);

split_definition([], Unreachable, Def, Defp, Defmacro, Defmacrop, Exports, {Head, Tail}) ->
  {Def, Defp, Defmacro, Defmacrop, Exports, Head ++ Tail, Unreachable}.

add_definition(Meta, Body, {Head, Tail}) ->
  case lists:keyfind(location, 1, Meta) of
    {location, {F, L}} ->
      Attr = {attribute, ?ann(Meta), file, {elixir_utils:characters_to_list(F), L}},
      {Head, [Attr, Body | Tail]};
    false ->
      {[Body | Head], Tail}
  end.

function_for_stored_definition(Kind, Meta, File, {Name, Arity}, Clauses) ->
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
    %% file field from #elixir_scope and clean up the code.
    case lists:keyfind(location, 1, Meta) of
      {location, {F, _}} -> #elixir_scope{def = {Kind, Name, Arity}, file = F};
      false -> #elixir_scope{def = {Kind, Name, Arity}, file = File}
    end,

  {TClause, TS} = elixir_clauses:clause(Meta,
                    fun elixir_translator:translate_args/2, Args, Body, Guards, S),

  case is_macro(Kind) of
    true ->
      Ann = ?ann(Meta),
      FArgs = {var, Ann, '_@CALLER'},
      MClause = setelement(3, TClause, [FArgs | element(3, TClause)]),

      case TS#elixir_scope.caller of
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

%% Store each definition in the table.
%% This function also checks and emit warnings in case
%% the kind, of the visibility of the function changes.

store_definition(Check, Kind, Meta, Name, Arity, File, Module, Defaults, Clauses) ->
  Data = elixir_module:data_table(Module),
  Defs = elixir_module:defs_table(Module),

  Tuple   = {Name, Arity},
  HasBody = Clauses =/= [],

  MaxDefaults =
    case ets:take(Defs, {def, Tuple}) of
      [{_, StoredKind, StoredMeta, StoredFile, StoredCheck,
          {StoredDefaults, LastHasBody, LastDefaults}}] ->
        check_valid_kind(Meta, File, Name, Arity, Kind, StoredKind),
        (Check and StoredCheck) andalso
          check_valid_clause(Meta, File, Name, Arity, Kind, Data, StoredMeta, StoredFile),
        check_valid_defaults(Meta, File, Name, Arity, Kind, Defaults, StoredDefaults, LastDefaults, LastHasBody),
        {max(Defaults, StoredDefaults), HasBody, Defaults};
      [] ->
        {Defaults, HasBody, Defaults}
    end,

  Check andalso ets:insert(Data, {?last_def, Tuple}),
  ets:insert(Defs, [{{clauses, Tuple}, Clause} || Clause <- Clauses]),
  ets:insert(Defs, {{def, Tuple}, Kind, Meta, File, Check, MaxDefaults}).

%% Validations

check_valid_kind(_Meta, _File, _Name, _Arity, Kind, Kind) -> [];
check_valid_kind(Meta, File, Name, Arity, Kind, StoredKind) ->
  elixir_errors:form_error(Meta, File, ?MODULE,
    {changed_kind, {Name, Arity, StoredKind, Kind}}).

check_valid_clause(Meta, File, Name, Arity, Kind, Data, StoredMeta, StoredFile) ->
  case ets:lookup_element(Data, ?last_def, 2) of
    {Name, Arity} -> [];
    [] -> [];
    _ ->
      Relative = elixir_utils:relative_to_cwd(StoredFile),
      elixir_errors:form_warn(Meta, File, ?MODULE,
        {ungrouped_clause, {Kind, Name, Arity, ?line(StoredMeta), Relative}})
  end.

% Clause with defaults after clause with defaults
check_valid_defaults(Meta, File, Name, Arity, Kind, Defaults, StoredDefaults, _, _) when Defaults > 0, StoredDefaults > 0 ->
  elixir_errors:form_error(Meta, File, ?MODULE, {clauses_with_defaults, {Kind, Name, Arity}});
% Clause with defaults after clause(s) without defaults
check_valid_defaults(Meta, File, Name, Arity, Kind, Defaults, 0, 0, _) when Defaults > 0 ->
  elixir_errors:form_warn(Meta, File, ?MODULE, {clauses_with_defaults, {Kind, Name, Arity}});
% Clause without defaults directly after clause with defaults (body less does not count)
check_valid_defaults(Meta, File, Name, Arity, Kind, 0, _, LastDefaults, true) when LastDefaults > 0 ->
  elixir_errors:form_warn(Meta, File, ?MODULE, {clauses_with_defaults, {Kind, Name, Arity}});
% Clause without defaults
check_valid_defaults(_Meta, _File, _Name, _Arity, _Kind, 0, _, _, _) -> [].

warn_bodyless_function(Check, _Meta, _File, Module, _Kind, _Tuple)
    when Check == false; Module == 'Elixir.Module' ->
  ok;
warn_bodyless_function(_Check, Meta, File, _Module, Kind, Tuple) ->
  elixir_errors:form_warn(Meta, File, ?MODULE, {bodyless_clause, Kind, Tuple}),
  ok.

check_args_for_bodyless_clause(Meta, Args, E) ->
  [begin
     elixir_errors:form_error(Meta, ?m(E, file), ?MODULE, invalid_args_for_bodyless_clause)
   end || Arg <- Args, invalid_arg(Arg)].

invalid_arg({Name, _, Kind}) when is_atom(Name), is_atom(Kind) -> false;
invalid_arg(_) -> true.

check_previous_defaults(Meta, Module, Name, Arity, Kind, Defaults, E) ->
  Matches = ets:match(elixir_module:defs_table(Module),
                      {{def, {Name, '$2'}}, '$1', '_', '_', '_', {'$3', '_', '_'}}),
  [begin
     elixir_errors:form_error(Meta, ?m(E, file), ?MODULE,
       {defs_with_defaults, Name, {Kind, Arity}, {K, A}})
   end || [K, A, D] <- Matches, A /= Arity, D /= 0, defaults_conflict(A, D, Arity, Defaults)].

defaults_conflict(A, D, Arity, Defaults) ->
  ((Arity >= (A - D)) andalso (Arity < A)) orelse
    ((A >= (Arity - Defaults)) andalso (A < Arity)).

assert_no_aliases_name(Meta, '__aliases__', [Atom], #{file := File}) when is_atom(Atom) ->
  elixir_errors:form_error(Meta, File, ?MODULE, {no_alias, Atom});
assert_no_aliases_name(_Meta, _Aliases, _Args, _S) ->
  ok.

assert_valid_name(Meta, Kind, is_record, [_, _], #{file := File}) when Kind == defp; Kind == def ->
  elixir_errors:form_error(Meta, File, ?MODULE, {is_record, Kind});
assert_valid_name(_Meta, _Kind, _Name, _Args, _S) ->
  ok.

%% Format errors

format_error({bodyless_clause, Kind, {Name, Arity}}) ->
  io_lib:format("implementation not provided for predefined ~ts ~ts/~B", [Kind, Name, Arity]);

format_error({no_module, {Kind, Name, Arity}}) ->
  io_lib:format("cannot define function outside module, invalid scope for ~ts ~ts/~B", [Kind, Name, Arity]);

format_error({defs_with_defaults, Name, {Kind, Arity}, {K, A}}) when Arity > A ->
  io_lib:format("~ts ~ts/~B defaults conflicts with ~ts ~ts/~B",
    [Kind, Name, Arity, K, Name, A]);

format_error({defs_with_defaults, Name, {Kind, Arity}, {K, A}}) when Arity < A ->
  io_lib:format("~ts ~ts/~B conflicts with defaults from ~ts ~ts/~B",
    [Kind, Name, Arity, K, Name, A]);

format_error({clauses_with_defaults, {Kind, Name, Arity}}) ->
  io_lib:format(""
    "definitions with multiple clauses and default values require a header. Instead of:\n"
    "\n"
    "    def foo(:first_clause, b \\\\ :default) do ... end\n"
    "    def foo(:second_clause, b) do ... end\n"
    "\n"
    "one should write:\n"
    "\n"
    "    def foo(a, b \\\\ :default)\n"
    "    def foo(:first_clause, b) do ... end\n"
    "    def foo(:second_clause, b) do ... end\n"
    "\n"
   "~ts ~ts/~B has multiple clauses and defines defaults in one or more clauses", [Kind, Name, Arity]);

format_error({ungrouped_clause, {Kind, Name, Arity, OrigLine, OrigFile}}) ->
  io_lib:format("clauses for the same ~ts should be grouped together, ~ts ~ts/~B was previously defined (~ts:~B)",
    [Kind, Kind, Name, Arity, OrigFile, OrigLine]);

format_error({changed_kind, {Name, Arity, Previous, Current}}) ->
  io_lib:format("~ts ~ts/~B already defined as ~ts", [Current, Name, Arity, Previous]);

format_error({no_alias, Atom}) ->
  io_lib:format("function names should start with lowercase characters or underscore, invalid name ~ts", [Atom]);

format_error({invalid_def, Kind, NameAndArgs}) ->
  io_lib:format("invalid syntax in ~ts ~ts", [Kind, 'Elixir.Macro':to_string(NameAndArgs)]);

format_error(invalid_args_for_bodyless_clause) ->
  "only variables and \\\\ are allowed as arguments in definition header.\n"
  "\n"
  "If you did not intend to define a header, make sure your function "
  "definition has the proper syntax by wrapping the arguments in parentheses "
  "and ensuring there is no space between the function name and arguments";

format_error({is_record, Kind}) ->
  io_lib:format("cannot define function named ~ts is_record/2 due to compatibility "
                "issues with the Erlang compiler (it is a known bug)", [Kind]);

format_error({missing_do, Kind}) ->
  io_lib:format("missing do keyword in ~ts", [Kind]).
