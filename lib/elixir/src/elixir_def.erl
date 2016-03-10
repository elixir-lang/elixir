% Holds the logic responsible for function definitions (def(p) and defmacro(p)).
-module(elixir_def).
-export([setup/1, reset_last/1, lookup_definition/2,
  delete_definition/2, store_definition/6, unwrap_definitions/2,
  store_each/7, format_error/1]).
-include("elixir.hrl").

-define(last_def, {elixir, last_def}).
-define(attr, {elixir, def_table}).
-define(clauses_attr, {elixir, clauses_table}).

%% Table management functions. Called internally.

setup(Module) ->
  reset_last(Module),
  ok.

reset_last(Module) ->
  ets:insert(elixir_module:data_table(Module), {?last_def, []}).

lookup_definition(Module, Tuple) ->
  case ets:lookup(elixir_module:defs_table(Module), Tuple) of
    [Result] ->
      CTable = elixir_module:clas_table(Module),
      {Result, [Clause || {_, Clause} <- ets:lookup(CTable, Tuple)]};
    _ ->
      false
  end.

delete_definition(Module, Tuple) ->
  ets:delete(elixir_module:defs_table(Module), Tuple),
  ets:delete(elixir_module:clas_table(Module), Tuple).

% Invoked by the wrap definition with the function abstract tree.
% Each function is then added to the function table.

store_definition(Line, Kind, CheckClauses, Call, Body, Pos) when is_integer(Line) ->
  E = (elixir_locals:get_cached_env(Pos))#{line := Line},
  {NameAndArgs, Guards} = elixir_clauses:extract_guards(Call),

  {Name, Args} = case NameAndArgs of
    {N, _, A} when is_atom(N), is_atom(A) -> {N, []};
    {N, _, A} when is_atom(N), is_list(A) -> {N, A};
    _ -> elixir_errors:form_error([{line, Line}], ?m(E, file), ?MODULE, {invalid_def, Kind, NameAndArgs})
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
      {_, _} = KeepLocation -> {KeepLocation, keep};
      nil -> {nil, line}
    end,

  LinifyArgs   = elixir_quote:linify(Line, Key, Args),
  LinifyGuards = elixir_quote:linify(Line, Key, Guards),
  LinifyBody   = elixir_quote:linify(Line, Key, Body),

  assert_no_aliases_name(Line, Name, Args, E),
  assert_valid_name(Line, Kind, Name, Args, E),
  store_definition(Line, Kind, DoCheckClauses, Name,
                   LinifyArgs, LinifyGuards, LinifyBody, Location, E).

store_definition(Line, Kind, CheckClauses, Name, Args, Guards, Body, KeepLocation, #{module := Module} = ER) ->
  Arity = length(Args),
  Tuple = {Name, Arity},
  Location = retrieve_location(KeepLocation, Module),

  E = case Location of
    {F, _} -> ER#{function := Tuple, file := elixir_utils:characters_to_binary(F)};
    nil    -> ER#{function := Tuple}
  end,

  elixir_locals:record_definition(Tuple, Kind, Module),

  {Function, Defaults, Super} = translate_definition(Kind, Line, Name, Args, Guards, Body, E),
  run_on_definition_callbacks(Kind, Line, Module, Name, Args, Guards, expr_from_body(Line, Body), E),

  DefaultsLength = length(Defaults),
  elixir_locals:record_defaults(Tuple, Kind, Module, DefaultsLength),

  compile_super(Module, Super, E),
  check_previous_defaults(Line, Module, Name, Arity, Kind, DefaultsLength, E),

  %% Retrieve the file before we changed it based on @file
  File = ?m(ER, file),
  store_each(CheckClauses, Kind, File, Location, Module, DefaultsLength, Function),
  [store_each(false, Kind, File, Location, Module, 0,
    default_function_for(Kind, Name, Default)) || Default <- Defaults],

  make_struct_available(Kind, Module, Name, Args),
  {Name, Arity}.

%% @on_definition

run_on_definition_callbacks(Kind, Line, Module, Name, Args, Guards, Expr, E) ->
  Env = elixir_env:linify({Line, E}),
  Callbacks = elixir_module:get_attribute(Module, on_definition),
  _ = [Mod:Fun(Env, Kind, Name, Args, Guards, Expr) || {Mod, Fun} <- Callbacks],
  ok.

make_struct_available(def, Module, '__struct__', []) ->
  case erlang:get(elixir_compiler_pid) of
    undefined -> ok;
    Pid ->
      Pid ! {struct_available, Module},
      ok
  end;
make_struct_available(_, _, _, _) ->
  ok.

%% Retrieve location from meta file (if Key == keep)
%% or @file, otherwise nil

retrieve_location(Location, Module) ->
  case get_location_attribute(Module) of
    nil when is_tuple(Location) ->
      {File, Line} = Location,
      {normalize_location(File), Line};
    nil ->
      nil;
    File when is_binary(File) ->
      'Elixir.Module':delete_attribute(Module, file),
      {normalize_location(File), 0};
    {File, Line} when is_binary(File) andalso is_integer(Line) ->
      'Elixir.Module':delete_attribute(Module, file),
      {normalize_location(File), Line}
  end.

get_location_attribute(Module) ->
  elixir_module:get_attribute(Module, file).

normalize_location(File) ->
  elixir_utils:characters_to_list(elixir_utils:relative_to_cwd(File)).

%% Compile super

compile_super(Module, true, #{function := Function}) ->
  elixir_def_overridable:super(Module, Function);
compile_super(_Module, _, _E) -> ok.

%% Translate the given call and expression given
%% and then store it in memory.

translate_definition(Kind, Line, Name, Args, Guards, Body, E) when is_integer(Line) ->
  Arity = length(Args),

  {EArgs, EGuards, EBody, _} = elixir_exp_clauses:def(fun elixir_def_defaults:expand/2,
                                Args, Guards, expr_from_body(Line, Body), E),

  case Body of
    nil -> check_args_for_bodyless_clause(Line, EArgs, E);
    _ -> ok
  end,

  S = elixir_env:env_to_scope(E),
  {Unpacked, Defaults} = elixir_def_defaults:unpack(Kind, Name, EArgs, S),
  {Clauses, Super} = translate_clause(Body, Line, Kind, Unpacked, EGuards, EBody, S),

  Function = {function, Line, Name, Arity, Clauses},
  {Function, Defaults, Super}.

translate_clause(nil, _Line, _Kind, _Args, [], _Body, _S) ->
  {[], false};
translate_clause(nil, Line, Kind, _Args, _Guards, _Body, #elixir_scope{file=File}) ->
  elixir_errors:form_error([{line, Line}], File, ?MODULE, {missing_do, Kind});
translate_clause(_, Line, Kind, Args, Guards, Body, S) ->
  {TClause, TS} = elixir_clauses:clause([{line, Line}],
                    fun elixir_translator:translate_args/2, Args, Body, Guards, S),

  FClause = case is_macro(Kind) of
    true ->
      FArgs = {var, Line, '_@CALLER'},
      MClause = setelement(3, TClause, [FArgs|element(3, TClause)]),

      case TS#elixir_scope.caller of
        true  ->
          FBody = {'match', Line,
            {'var', Line, '__CALLER__'},
            elixir_utils:erl_call(Line, elixir_env, linify, [{var, Line, '_@CALLER'}])
          },
          setelement(5, MClause, [FBody|element(5, TClause)]);
        false ->
          MClause
      end;
    false ->
      TClause
  end,

  {[FClause], TS#elixir_scope.super}.

expr_from_body(_Line, nil)          -> nil;
expr_from_body(_Line, [{do, Expr}]) -> Expr;
expr_from_body(Line, Else)          -> {'try', [{line, Line}], [Else]}.

is_macro(defmacro)  -> true;
is_macro(defmacrop) -> true;
is_macro(_)         -> false.

% Unwrap the functions stored in the functions table.
% It returns a list of all functions to be exported, plus the macros,
% and the body of all functions.
unwrap_definitions(File, Module) ->
  Table  = elixir_module:defs_table(Module),
  CTable = elixir_module:clas_table(Module),

  {All, Private} = unwrap_definition(ets:tab2list(Table), File, Module, CTable, [], []),
  Unreachable = elixir_locals:warn_unused_local(File, Module, Private),
  split_definition(All, Unreachable, [], [], [], [], [], {[], []}).

unwrap_definition([Fun|T], File, Module, CTable, All, Private) ->
  {Tuple, Kind, Line, _, Check, Location, {Defaults, _, _}} = Fun,
  Export = export(Kind, Tuple),

  case [Clause || {_, Clause} <- ets:lookup(CTable, Tuple)] of
    [] ->
      warn_bodyless_function(Line, File, Module, Kind, Tuple),
      unwrap_definition(T, File, Module, CTable, All, Private);
    Clauses ->
      Unwrapped = {Tuple, Kind, Line, Location,
                   function_for_stored_definition(Line, Export, Clauses)},

      NewPrivate =
        if
          Kind == defp; Kind == defmacrop ->
            [{Tuple, Kind, Line, Check, Defaults}|Private];
          true ->
            Private
        end,

      unwrap_definition(T, File, Module, CTable, [Unwrapped|All], NewPrivate)
  end;

unwrap_definition([], _File, _Module, _CTable, All, Private) ->
  {All, Private}.

split_definition([{Tuple, def, Line, Location, Body}|T], Unreachable,
                 Def, Defp, Defmacro, Defmacrop, Exports, Functions) ->
  split_definition(T, Unreachable, [Tuple|Def], Defp, Defmacro, Defmacrop,
                   [export(def, Tuple)|Exports],
                   add_definition(Line, Location, Body, Functions));

split_definition([{Tuple, defp, Line, Location, Body}|T], Unreachable,
                 Def, Defp, Defmacro, Defmacrop, Exports, Functions) ->
  case lists:member(Tuple, Unreachable) of
    false ->
      split_definition(T, Unreachable, Def, [Tuple|Defp], Defmacro, Defmacrop,
                       Exports, add_definition(Line, Location, Body, Functions));
    true ->
      split_definition(T, Unreachable, Def, [Tuple|Defp], Defmacro, Defmacrop,
                       Exports, Functions)
  end;

split_definition([{Tuple, defmacro, Line, Location, Body}|T], Unreachable,
                 Def, Defp, Defmacro, Defmacrop, Exports, Functions) ->
  split_definition(T, Unreachable, Def, Defp, [Tuple|Defmacro], Defmacrop,
                   [export(defmacro, Tuple)|Exports],
                   add_definition(Line, Location, Body, Functions));

split_definition([{Tuple, defmacrop, _Line, _Location, _Body}|T], Unreachable,
                 Def, Defp, Defmacro, Defmacrop, Exports, Functions) ->
  split_definition(T, Unreachable, Def, Defp, Defmacro, [Tuple|Defmacrop],
                   Exports, Functions);

split_definition([], Unreachable, Def, Defp, Defmacro, Defmacrop, Exports, {Head, Tail}) ->
  {Def, Defp, Defmacro, Defmacrop, Exports, Head ++ Tail, Unreachable}.

%% Helpers

export(Kind, {Name, Arity}) when Kind == defmacro; Kind == defmacrop ->
  {elixir_utils:macro_name(Name), Arity + 1};
export(Kind, {Name, Arity}) when Kind == def; Kind == defp ->
  {Name, Arity}.

function_for_stored_definition(Line, {Name, Arity}, Clauses) ->
  {function, Line, Name, Arity, Clauses}.

add_definition(_Line, nil, Body, {Head, Tail}) ->
  {[Body|Head], Tail};
add_definition(Line, Location, Body, {Head, Tail}) ->
  {Head,
   [{attribute, Line, file, Location}, Body|Tail]}.

default_function_for(Kind, Name, {clause, Line, Args, _Guards, _Exprs} = Clause)
    when Kind == defmacro; Kind == defmacrop ->
  {function, Line, Name, length(Args) - 1, [Clause]};
default_function_for(_, Name, {clause, Line, Args, _Guards, _Exprs} = Clause) ->
  {function, Line, Name, length(Args), [Clause]}.

warn_bodyless_function(_Line, _File, 'Elixir.Module', _Kind, _Tuple) ->
  ok; %% Documentation for __info__
warn_bodyless_function(Line, File, _Module, Kind, Tuple) ->
  elixir_errors:form_warn([{line, Line}], File, ?MODULE, {bodyless_clause, Kind, Tuple}),
  ok.

%% Store each definition in the table.
%% This function also checks and emit warnings in case
%% the kind, of the visibility of the function changes.

store_each(Check, Kind, File, Location, Module, Defaults, {function, Line, Name, Arity, Clauses}) ->
  Data = elixir_module:data_table(Module),
  Defs = elixir_module:defs_table(Module),
  Clas = elixir_module:clas_table(Module),

  Tuple   = {Name, Arity},
  HasBody = Clauses =/= [],

  case ets:lookup(Defs, Tuple) of
    [{Tuple, StoredKind, StoredLine, StoredFile, StoredCheck,
        StoredLocation, {StoredDefaults, LastHasBody, LastDefaults}}] ->
      FinalLine = StoredLine,
      FinalLocation = StoredLocation,
      FinalDefaults = {max(Defaults, StoredDefaults), HasBody, Defaults},
      check_valid_kind(Line, File, Name, Arity, Kind, StoredKind),
      (Check and StoredCheck) andalso
        check_valid_clause(Line, File, Name, Arity, Kind, Data, StoredLine, StoredFile),
      check_valid_defaults(Line, File, Name, Arity, Kind, Defaults, StoredDefaults, LastDefaults, LastHasBody);
    [] ->
      FinalLine = Line,
      FinalLocation = Location,
      FinalDefaults = {Defaults, HasBody, Defaults}
  end,

  Check andalso ets:insert(Data, {?last_def, {Name, Arity}}),
  ets:insert(Clas, [{Tuple, Clause} || Clause <- Clauses]),
  ets:insert(Defs, {Tuple, Kind, FinalLine, File, Check, FinalLocation, FinalDefaults}).

%% Validations

check_valid_kind(_Line, _File, _Name, _Arity, Kind, Kind) -> [];
check_valid_kind(Line, File, Name, Arity, Kind, StoredKind) ->
  elixir_errors:form_error([{line, Line}], File, ?MODULE,
    {changed_kind, {Name, Arity, StoredKind, Kind}}).

check_valid_clause(Line, File, Name, Arity, Kind, Data, StoredLine, StoredFile) ->
  case ets:lookup_element(Data, ?last_def, 2) of
    {Name, Arity} -> [];
    [] -> [];
    _ ->
      Relative = elixir_utils:relative_to_cwd(elixir_utils:relative_to_cwd(StoredFile)),
      elixir_errors:form_warn([{line, Line}], File, ?MODULE,
        {ungrouped_clause, {Kind, Name, Arity, StoredLine, Relative}})
  end.

% Clause with defaults after clause with defaults
check_valid_defaults(Line, File, Name, Arity, Kind, Defaults, StoredDefaults, _, _) when Defaults > 0, StoredDefaults > 0 ->
  elixir_errors:form_error([{line, Line}], File, ?MODULE,
    {clauses_with_defaults, {Kind, Name, Arity}});
% Clause with defaults after clause(s) without defaults
check_valid_defaults(Line, File, Name, Arity, Kind, Defaults, 0, 0, _) when Defaults > 0 ->
  elixir_errors:form_warn([{line, Line}], File, ?MODULE, {clauses_with_defaults, {Kind, Name, Arity}});
% Clause without defaults directly after clause with defaults (body less does not count)
check_valid_defaults(Line, File, Name, Arity, Kind, 0, _, LastDefaults, true) when LastDefaults > 0 ->
  elixir_errors:form_warn([{line, Line}], File, ?MODULE,
    {clauses_with_defaults, {Kind, Name, Arity}});
% Clause without defaults
check_valid_defaults(_Line, _File, _Name, _Arity, _Kind, 0, _, _, _) -> [].

check_previous_defaults(Line, Module, Name, Arity, Kind, Defaults, E) ->
  Matches = ets:match(elixir_module:defs_table(Module), {{Name, '$2'}, '$1', '_', '_', '_', '_', {'$3', '_', '_'}}),
  [ begin
      elixir_errors:form_error([{line, Line}], ?m(E, file), ?MODULE,
        {defs_with_defaults, Name, {Kind, Arity}, {K, A}})
    end || [K, A, D] <- Matches, A /= Arity, D /= 0, defaults_conflict(A, D, Arity, Defaults)].

defaults_conflict(A, D, Arity, Defaults) ->
  ((Arity >= (A - D)) andalso (Arity < A)) orelse
    ((A >= (Arity - Defaults)) andalso (A < Arity)).

check_args_for_bodyless_clause(Line, Args, E) ->
  [ begin
      elixir_errors:form_error([{line, Line}], ?m(E, file), ?MODULE,
        invalid_args_for_bodyless_clause)
    end || Arg <- Args, invalid_arg(Arg) ].

invalid_arg({Name, _, Kind}) when is_atom(Name), is_atom(Kind) ->
  false;
invalid_arg({'\\\\', _, [{Name, _, Kind}, _]}) when is_atom(Name), is_atom(Kind) ->
  false;
invalid_arg(_) ->
  true.

assert_no_aliases_name(Line, '__aliases__', [Atom], #{file := File}) when is_atom(Atom) ->
  elixir_errors:form_error([{line, Line}], File, ?MODULE, {no_alias, Atom});
assert_no_aliases_name(_Line, _Aliases, _Args, _S) ->
  ok.

assert_valid_name(Line, Kind, is_record, [_, _], #{file := File}) when Kind == defp; Kind == def ->
  elixir_errors:form_error([{line, Line}], File, ?MODULE, {is_record, Kind});
assert_valid_name(_Line, _Kind, _Name, _Args, _S) ->
  ok.

%% Format errors

format_error({bodyless_clause, Kind, {Name, Arity}}) ->
  io_lib:format("bodyless clause provided for nonexistent ~ts ~ts/~B", [Kind, Name, Arity]);

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
    "definitions with multiple clauses and default values require a function head. Instead of:\n"
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
   "~ts ~ts/~B has multiple clauses and defines defaults in a clause with a body", [Kind, Name, Arity]);

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
  "can use only variables and \\\\ as arguments in function heads";

format_error({is_record, Kind}) ->
  io_lib:format("cannot define function named ~ts is_record/2 due to compability "
                "issues with the Erlang compiler (it is a known bug)", [Kind]);

format_error({missing_do, Kind}) ->
  io_lib:format("missing do keyword in ~ts", [Kind]).
