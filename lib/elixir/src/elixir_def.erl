% Holds the logic responsible for function definitions (def(p) and defmacro(p)).
-module(elixir_def).
-export([setup/1, reset_last/1, local_for/5, external_for/5,
  take_definition/2, store_definition/3, store_definition/9,
  fetch_definitions/2, format_error/1]).
-include("elixir.hrl").
-define(last_def, {elixir, last_def}).

setup(DataTables) ->
  reset_last(DataTables),
  ok.

reset_last({DataSet, _DataBag}) ->
  ets:insert(DataSet, {?last_def, none});

reset_last(Module) when is_atom(Module) ->
  reset_last(elixir_module:data_tables(Module)).

%% Finds the local definition for the current module.
local_for(Meta, Name, Arity, Kinds, E) ->
  External = fun({Mod, Fun}, Args) ->
    invoke_external([{from_macro, true} | Meta], Mod, Fun, Args, E)
  end,
  fun_for(Meta, ?key(E, module), Name, Arity, Kinds, {value, External}).

%% Finds the local definition for an external module.
external_for(Meta, Module, Name, Arity, Kinds) ->
  fun_for(Meta, Module, Name, Arity, Kinds, none).

fun_for(Meta, Module, Name, Arity, Kinds, External) ->
  Tuple = {Name, Arity},

  try
    {Set, Bag} = elixir_module:data_tables(Module),
    {ets:lookup(Set, {def, Tuple}), ets:lookup(Bag, {clauses, Tuple})}
  of
    {[{_, Kind, LocalMeta, _, _, _}], ClausesPairs} ->
      case (Kinds == all) orelse (lists:member(Kind, Kinds)) of
        true ->
          Local = {value, fun(Fun, Args) -> invoke_local(Meta, Module, Fun, Args, External) end},
          Clauses = [Clause || {_, Clause} <- ClausesPairs],
          elixir_erl:definition_to_anonymous(Kind, LocalMeta, Clauses, Local, External);
        false ->
          false
      end;
    {[], _} ->
      false
  catch
    _:_ -> false
  end.

invoke_local(Meta, Module, ErlName, Args, External) ->
  {Name, Arity} = elixir_utils:erl_fa_to_elixir_fa(ErlName, length(Args)),

  case fun_for(Meta, Module, Name, Arity, all, External) of
    false ->
      {current_stacktrace, [_ | T]} = erlang:process_info(self(), current_stacktrace),
      erlang:raise(error, undef, [{Module, Name, Arity, []} | T]);
    Fun ->
      apply(Fun, Args)
  end.

invoke_external(Meta, Mod, Name, Args, E) ->
  is_map(E) andalso elixir_env:trace({require, Meta, Mod, []}, E),
  apply(Mod, Name, Args).

%% Take a definition out of the table

take_definition(Module, {Name, Arity} = Tuple) ->
  {Set, Bag} = elixir_module:data_tables(Module),
  case ets:take(Set, {def, Tuple}) of
    [{{def, Tuple}, _, _, _, _, {Defaults, _, _}} = Result] ->
      ets:delete_object(Bag, {defs, Tuple}),
      ets:delete_object(Bag, {{default, Name}, Arity, Defaults}),
      {Result, [Clause || {_, Clause} <- ets:take(Bag, {clauses, Tuple})]};
    [] ->
      false
  end.

%% Fetch all available definitions

fetch_definitions(Module, E) ->
  {Set, Bag} = elixir_module:data_tables(Module),

  Entries = try
    lists:sort(ets:lookup_element(Bag, defs, 2))
  catch
    error:badarg -> []
  end,

  fetch_definition(Entries, E, Module, Set, Bag, [], []).

fetch_definition([Tuple | T], E, Module, Set, Bag, All, Private) ->
  [{_, Kind, Meta, _, Check, {MaxDefaults, _, Defaults}}] = ets:lookup(Set, {def, Tuple}),

  try ets:lookup_element(Bag, {clauses, Tuple}, 2) of
    Clauses ->
      NewAll =
        [{Tuple, Kind, add_defaults_to_meta(MaxDefaults, Meta), Clauses} | All],
      NewPrivate =
        case (Kind == defp) orelse (Kind == defmacrop) of
          true ->
            Metas = head_and_definition_meta(Check, Meta, MaxDefaults - Defaults, All),
            [{Tuple, Kind, Metas, MaxDefaults} | Private];
          false ->
            Private
        end,
      fetch_definition(T, E, Module, Set, Bag, NewAll, NewPrivate)
  catch
    error:badarg ->
      elixir_errors:module_error(Meta, E, ?MODULE, {function_head, Kind, Tuple}),
      fetch_definition(T, E, Module, Set, Bag, All, Private)
  end;

fetch_definition([], _E, _Module, _Set, _Bag, All, Private) ->
  {All, Private}.

add_defaults_to_meta(0, Meta) -> Meta;
add_defaults_to_meta(Defaults, Meta) -> [{defaults, Defaults} | Meta].

head_and_definition_meta(none, _Meta, _HeadDefaults, _All) ->
  false;
head_and_definition_meta(_, Meta, 0, _All) ->
  Meta;
head_and_definition_meta(_, _Meta, _HeadDefaults, [{_, _, HeadMeta, _} | _]) ->
  HeadMeta.

%% Section for storing definitions

store_definition(Kind, {Call, Body}, Pos) ->
  E = elixir_locals:get_cached_env(Pos),
  store_definition(Kind, false, Call, Body, E);
store_definition(Kind, Key, Pos) ->
  #{module := Module} = E = elixir_locals:get_cached_env(Pos),
  {Call, Body} = elixir_module:read_cache(Module, Key),
  store_definition(Kind, true, Call, Body, E).

store_definition(Kind, HasNoUnquote, Call, Body, #{line := Line} = E) ->
  {NameAndArgs, Guards} = elixir_utils:extract_guards(Call),

  {Name, Meta, Args} = case NameAndArgs of
    {N, M, A} when is_atom(N), is_atom(A) -> {N, M, []};
    {N, M, A} when is_atom(N), is_list(A) -> {N, M, A};
    _ -> elixir_errors:file_error([{line, Line}], E, ?MODULE, {invalid_def, Kind, NameAndArgs})
  end,

  Context = case lists:keyfind(context, 1, Meta) of
    {context, _} = ContextPair -> [ContextPair];
    _ -> []
  end,

  Column = case lists:keyfind(column, 1, Meta) of
    {column, _} = ColumnPair -> [ColumnPair | Context];
    _ -> Context
  end,

  Generated = case lists:keyfind(generated, 1, Meta) of
    {generated, true} = GeneratedPair -> [GeneratedPair | Column];
    _ -> Column
  end,

  CheckClauses = if
    Context /= [] -> none;
    HasNoUnquote -> all;
    true -> unused_only
  end,

  %% Check if there is a file information in the definition.
  %% If so, we assume this come from another source and
  %% we need to linify taking into account keep line numbers.
  %%
  %% Line and File will always point to the caller. __ENV__.line
  %% will always point to the quoted one and __ENV__.file will
  %% always point to the one at @file or the quoted one.
  {Location, LinifyArgs, LinifyGuards, LinifyBody} =
    case elixir_utils:meta_keep(Meta) of
      {_, _} = MetaFile ->
        {MetaFile,
         elixir_quote:linify(Line, keep, Args),
         elixir_quote:linify(Line, keep, Guards),
         elixir_quote:linify(Line, keep, Body)};
      nil ->
        {nil, Args, Guards, Body}
    end,

  Arity = length(Args),

  {File, DefMeta} =
    case retrieve_location(Location, ?key(E, module)) of
      {AF, RF, L} ->
        {AF, [{line, Line}, {file, {RF, L}} | Generated]};
      nil ->
        {nil, [{line, Line} | Generated]}
    end,

  run_with_location_change(File, E, fun(EL) ->
    assert_no_aliases_name(DefMeta, Name, Args, EL),
    assert_valid_name(DefMeta, Kind, Name, Args, EL),
    store_definition(DefMeta, Kind, CheckClauses, Name, Arity,
                     LinifyArgs, LinifyGuards, LinifyBody, ?key(E, file), EL)
  end).

store_definition(Meta, Kind, CheckClauses, Name, Arity, DefaultsArgs, Guards, Body, File, ER) ->
  Module = ?key(ER, module),
  Tuple = {Name, Arity},
  {S, E} = env_for_expansion(Kind, Tuple, ER),

  {Args, Defaults} = unpack_defaults(Kind, Meta, Name, DefaultsArgs, S, E),
  Clauses = [elixir_clauses:def(Clause, S, E) ||
             Clause <- def_to_clauses(Kind, Meta, Args, Guards, Body, E)],

  DefaultsLength = length(Defaults),
  elixir_locals:record_defaults(Tuple, Kind, Module, DefaultsLength, Meta),
  check_previous_defaults(Meta, Module, Name, Arity, Kind, DefaultsLength, E),

  store_definition(CheckClauses, Kind, Meta, Name, Arity, File,
                   Module, DefaultsLength, Clauses),
  [store_definition(none, Kind, Meta, Name, length(DefaultArgs), File,
                    Module, 0, [Default]) || {_, DefaultArgs, _, _} = Default <- Defaults],

  run_on_definition_callbacks(Kind, Module, Name, DefaultsArgs, Guards, Body, E),
  Tuple.

env_for_expansion(Kind, Tuple, E) when Kind =:= defmacro; Kind =:= defmacrop ->
  S = elixir_env:env_to_ex(E),
  {S#elixir_ex{caller=true}, E#{function := Tuple}};
env_for_expansion(_Kind, Tuple, E) ->
  {elixir_env:env_to_ex(E), E#{function := Tuple}}.

retrieve_location(Location, Module) ->
  {Set, _} = elixir_module:data_tables(Module),
  case ets:take(Set, file) of
    [] when is_tuple(Location) ->
      {File, Line} = Location,
      {filename:absname(File), elixir_utils:relative_to_cwd(File), Line};
    [] ->
      nil;
    [{file, File, _, _}] when is_binary(File) ->
      'Elixir.Module':delete_attribute(Module, file),
      {filename:absname(File), elixir_utils:relative_to_cwd(File), 0};
    [{file, {File, Line}, _, _}] when is_binary(File) andalso is_integer(Line) ->
      'Elixir.Module':delete_attribute(Module, file),
      {filename:absname(File), elixir_utils:relative_to_cwd(File), Line}
  end.

run_with_location_change(nil, E, Callback) ->
  Callback(E);
run_with_location_change(File, #{file := File} = E, Callback) ->
  Callback(E);
run_with_location_change(File, E, Callback) ->
  elixir_lexical:with_file(File, E, Callback).

def_to_clauses(_Kind, Meta, Args, [], nil, E) ->
  check_args_for_function_head(Meta, Args, E),
  [];
def_to_clauses(_Kind, Meta, Args, Guards, [{do, Body}], _E) ->
  [{Meta, Args, Guards, Body}];
def_to_clauses(Kind, Meta, Args, Guards, Body, E) ->
  case is_list(Body) andalso lists:keyfind(do, 1, Body) of
    {do, _} ->
      [{Meta, Args, Guards, {'try', [{origin,  Kind} | Meta], [Body]}}];
    false ->
      elixir_errors:file_error(Meta, E, elixir_expand, {missing_option, Kind, [do]})
  end.

run_on_definition_callbacks(Kind, Module, Name, Args, Guards, Body, E) ->
  {_, Bag} = elixir_module:data_tables(Module),
  Callbacks = ets:lookup_element(Bag, {accumulate, on_definition}, 2),
  _ = [Mod:Fun(E, Kind, Name, Args, Guards, Body) || {Mod, Fun} <- lists:reverse(Callbacks)],
  ok.

store_definition(CheckClauses, Kind, Meta, Name, Arity, File, Module, Defaults, Clauses)
    when CheckClauses == all; CheckClauses == none; CheckClauses == unused_only ->
  {Set, Bag} = elixir_module:data_tables(Module),
  Tuple = {Name, Arity},
  HasBody = Clauses =/= [],
  CheckAll = CheckClauses == all,

  if
    Defaults > 0 ->
      ets:insert(Bag, {{default, Name}, Arity, Defaults});
    true ->
      ok
  end,

  {MaxDefaults, FirstMeta} =
    case ets:lookup(Set, {def, Tuple}) of
      [{_, StoredKind, StoredMeta, StoredFile, StoredCheck, {StoredDefaults, LastHasBody, LastDefaults}}] ->
        check_valid_kind(Meta, File, Name, Arity, Kind, StoredKind, StoredFile, StoredMeta),
        check_valid_defaults(Meta, File, Name, Arity, Kind, Defaults, StoredDefaults, LastDefaults, HasBody, LastHasBody),
        (CheckAll and (StoredCheck == all)) andalso
          check_valid_clause(Meta, File, Name, Arity, Kind, Set, StoredMeta, StoredFile, Clauses),

        {max(Defaults, StoredDefaults), StoredMeta};
      [] ->
        ets:insert(Bag, {defs, Tuple}),
        {Defaults, Meta}
    end,

  CheckAll andalso ets:insert(Set, {?last_def, Tuple}),
  ets:insert(Bag, [{{clauses, Tuple}, Clause} || Clause <- Clauses]),
  ets:insert(Set, {{def, Tuple}, Kind, FirstMeta, File, CheckClauses, {MaxDefaults, HasBody, Defaults}}).

%% Handling of defaults

unpack_defaults(Kind, Meta, Name, Args, S, E) ->
  {Expanded, #elixir_ex{unused={_, VersionOffset}}} = expand_defaults(Args, S, E#{context := nil}, []),
  unpack_expanded(Kind, Meta, Name, Expanded, VersionOffset, [], []).

unpack_expanded(Kind, Meta, Name, [{'\\\\', DefaultMeta, [Expr, _]} | T] = List, VersionOffset, Acc, Clauses) ->
  Base = match_defaults(Acc, length(Acc) + VersionOffset, []),
  {Args, Invoke} = extract_defaults(List, length(Base) + VersionOffset, [], []),
  Clause = {Meta, Base ++ Args, [], {super, [{super, {Kind, Name}} | DefaultMeta], Base ++ Invoke}},
  unpack_expanded(Kind, Meta, Name, T, VersionOffset, [Expr | Acc], [Clause | Clauses]);
unpack_expanded(Kind, Meta, Name, [H | T], VersionOffset, Acc, Clauses) ->
  unpack_expanded(Kind, Meta, Name, T, VersionOffset, [H | Acc], Clauses);
unpack_expanded(_Kind, _Meta, _Name, [], _VersionOffset, Acc, Clauses) ->
  {lists:reverse(Acc), lists:reverse(Clauses)}.

expand_defaults([{'\\\\', Meta, [Expr, Default]} | Args], S, E, Acc) ->
  {ExpandedDefault, SE, _} = elixir_expand:expand(Default, S, E),
  expand_defaults(Args, SE, E, [{'\\\\', Meta, [Expr, ExpandedDefault]} | Acc]);
expand_defaults([Arg | Args], S, E, Acc) ->
   expand_defaults(Args, S, E, [Arg | Acc]);
expand_defaults([], S, _E, Acc) ->
  {lists:reverse(Acc), S}.

extract_defaults([{'\\\\', _, [_Expr, Default]} | T], Counter, NewArgs, NewInvoke) ->
  extract_defaults(T, Counter, NewArgs, [Default | NewInvoke]);
extract_defaults([_ | T], Counter, NewArgs, NewInvoke) ->
  H = default_var(Counter),
  extract_defaults(T, Counter + 1, [H | NewArgs], [H | NewInvoke]);
extract_defaults([], _Counter, NewArgs, NewInvoke) ->
  {lists:reverse(NewArgs), lists:reverse(NewInvoke)}.

match_defaults([], _Counter, Acc) ->
  Acc;
match_defaults([_ | T], Counter, Acc) ->
  NewCounter = Counter - 1,
  match_defaults(T, NewCounter, [default_var(NewCounter) | Acc]).

default_var(Counter) ->
  {list_to_atom([$x | integer_to_list(Counter)]), [{generated, true}, {version, Counter}], ?var_context}.

%% Validations

check_valid_kind(_Meta, _File, _Name, _Arity, Kind, Kind, _StoredFile, _StoredMeta) -> ok;
check_valid_kind(Meta, File, Name, Arity, Kind, StoredKind, StoredFile, StoredMeta) ->
  elixir_errors:file_error(Meta, File, ?MODULE,
    {changed_kind, {Name, Arity, StoredKind, Kind, StoredFile, ?line(StoredMeta)}}).

check_valid_clause(Meta, File, Name, Arity, Kind, Set, StoredMeta, StoredFile, Clauses) ->
  case ets:lookup_element(Set, ?last_def, 2) of
    none ->
      ok;
    {Name, Arity} when Clauses == [] ->
      elixir_errors:file_warn(Meta, File, ?MODULE,
        {late_function_head, {Kind, Name, Arity}});
    {Name, Arity} ->
      ok;
    {Name, _} ->
      Relative = elixir_utils:relative_to_cwd(StoredFile),
      elixir_errors:file_warn(Meta, File, ?MODULE,
        {ungrouped_name, {Kind, Name, Arity, ?line(StoredMeta), Relative}});
    _ ->
      Relative = elixir_utils:relative_to_cwd(StoredFile),
      elixir_errors:file_warn(Meta, File, ?MODULE,
        {ungrouped_arity, {Kind, Name, Arity, ?line(StoredMeta), Relative}})
  end.

% Clause with defaults after clause with defaults
check_valid_defaults(Meta, File, Name, Arity, Kind, Defaults, StoredDefaults, _, _, _)
    when Defaults > 0, StoredDefaults > 0 ->
  elixir_errors:file_error(Meta, File, ?MODULE, {duplicate_defaults, {Kind, Name, Arity}});
% Clause with defaults after clause without defaults
check_valid_defaults(Meta, File, Name, Arity, Kind, Defaults, 0, _, _, _) when Defaults > 0 ->
  elixir_errors:file_warn(Meta, File, ?MODULE, {mixed_defaults, {Kind, Name, Arity}});
% Clause without defaults directly after clause with defaults (bodiless does not count)
check_valid_defaults(Meta, File, Name, Arity, Kind, 0, _, LastDefaults, true, true) when LastDefaults > 0 ->
  elixir_errors:file_warn(Meta, File, ?MODULE, {mixed_defaults, {Kind, Name, Arity}});
% Clause without defaults
check_valid_defaults(_Meta, _File, _Name, _Arity, _Kind, 0, _StoredDefaults, _LastDefaults, _HasBody, _LastHasBody) ->
  ok.

check_args_for_function_head(Meta, Args, E) ->
  [begin
     elixir_errors:module_error(Meta, E, ?MODULE, invalid_args_for_function_head)
   end || Arg <- Args, invalid_arg(Arg)].

invalid_arg({Name, _, Kind}) when is_atom(Name), is_atom(Kind) -> false;
invalid_arg(_) -> true.

check_previous_defaults(Meta, Module, Name, Arity, Kind, Defaults, E) ->
  {_Set, Bag} = elixir_module:data_tables(Module),
  Matches = ets:lookup(Bag, {default, Name}),
  [begin
     elixir_errors:file_error(Meta, E, ?MODULE,
       {defs_with_defaults, Kind, Name, Arity, A})
   end || {_, A, D} <- Matches, A /= Arity, D /= 0, defaults_conflict(A, D, Arity, Defaults)].

defaults_conflict(A, D, Arity, Defaults) ->
  ((Arity >= (A - D)) andalso (Arity < A)) orelse
    ((A >= (Arity - Defaults)) andalso (A < Arity)).

assert_no_aliases_name(Meta, '__aliases__', [Atom], #{file := File}) when is_atom(Atom) ->
  elixir_errors:file_error(Meta, File, ?MODULE, {no_alias, Atom});
assert_no_aliases_name(_Meta, _Aliases, _Args, _S) ->
  ok.

assert_valid_name(Meta, Kind, '__info__', [_], #{file := File}) ->
  elixir_errors:file_error(Meta, File, ?MODULE, {'__info__', Kind});
assert_valid_name(Meta, Kind, 'module_info', [], #{file := File}) ->
  elixir_errors:file_error(Meta, File, ?MODULE, {module_info, Kind, 0});
assert_valid_name(Meta, Kind, 'module_info', [_], #{file := File}) ->
  elixir_errors:file_error(Meta, File, ?MODULE, {module_info, Kind, 1});
assert_valid_name(Meta, Kind, is_record, [_, _], #{file := File}) when Kind == defp; Kind == def ->
  elixir_errors:file_error(Meta, File, ?MODULE, {is_record, Kind});
assert_valid_name(_Meta, _Kind, _Name, _Args, _S) ->
  ok.

%% Format errors

format_error({function_head, Kind, {Name, Arity}}) ->
  io_lib:format("implementation not provided for predefined ~ts ~ts/~B", [Kind, Name, Arity]);

format_error({no_module, {Kind, Name, Arity}}) ->
  io_lib:format("cannot define function outside module, invalid scope for ~ts ~ts/~B", [Kind, Name, Arity]);

format_error({defs_with_defaults, Kind, Name, Arity, A}) when Arity > A ->
  io_lib:format("~ts ~ts/~B defaults conflicts with ~ts/~B",
    [Kind, Name, Arity, Name, A]);

format_error({defs_with_defaults, Kind, Name, Arity, A}) when Arity < A ->
  io_lib:format("~ts ~ts/~B conflicts with defaults from ~ts/~B",
    [Kind, Name, Arity, Name, A]);

format_error({duplicate_defaults, {Kind, Name, Arity}}) ->
  io_lib:format(
    "~ts ~ts/~B defines defaults multiple times. "
    "Elixir allows defaults to be declared once per definition. Instead of:\n"
    "\n"
    "    def foo(:first_clause, b \\\\ :default) do ... end\n"
    "    def foo(:second_clause, b \\\\ :default) do ... end\n"
    "\n"
    "one should write:\n"
    "\n"
    "    def foo(a, b \\\\ :default)\n"
    "    def foo(:first_clause, b) do ... end\n"
    "    def foo(:second_clause, b) do ... end\n",
    [Kind, Name, Arity]);

format_error({mixed_defaults, {Kind, Name, Arity}}) ->
  io_lib:format(
    "~ts ~ts/~B has multiple clauses and also declares default values. "
    "In such cases, the default values should be defined in a header. Instead of:\n"
    "\n"
    "    def foo(:first_clause, b \\\\ :default) do ... end\n"
    "    def foo(:second_clause, b) do ... end\n"
    "\n"
    "one should write:\n"
    "\n"
    "    def foo(a, b \\\\ :default)\n"
    "    def foo(:first_clause, b) do ... end\n"
    "    def foo(:second_clause, b) do ... end\n",
    [Kind, Name, Arity]);

format_error({ungrouped_name, {Kind, Name, Arity, OrigLine, OrigFile}}) ->
   io_lib:format("clauses with the same name should be grouped together, \"~ts ~ts/~B\" was previously defined (~ts:~B)",
     [Kind, Name, Arity, OrigFile, OrigLine]);

format_error({ungrouped_arity, {Kind, Name, Arity, OrigLine, OrigFile}}) ->
  io_lib:format("clauses with the same name and arity (number of arguments) should be grouped together, \"~ts ~ts/~B\" was previously defined (~ts:~B)",
    [Kind, Name, Arity, OrigFile, OrigLine]);

format_error({late_function_head, {Kind, Name, Arity}}) ->
  io_lib:format("function head for ~ts ~ts/~B must come at the top of its direct implementation. Instead of:\n"
    "\n"
    "    def add(a, b), do: a + b\n"
    "    def add(a, b)\n"
    "\n"
    "one should write:\n"
    "\n"
    "    def add(a, b)\n"
    "    def add(a, b), do: a + b\n",
    [Kind, Name, Arity]);

format_error({changed_kind, {Name, Arity, Previous, Current, OrigFile, OrigLine}}) ->
  OrigFileRelative = elixir_utils:relative_to_cwd(OrigFile),
  io_lib:format("~ts ~ts/~B already defined as ~ts in ~ts:~B", [Current, Name, Arity, Previous, OrigFileRelative, OrigLine]);

format_error({no_alias, Atom}) ->
  io_lib:format("function names should start with lowercase characters or underscore, invalid name ~ts", [Atom]);

format_error({invalid_def, Kind, NameAndArgs}) ->
  io_lib:format("invalid syntax in ~ts ~ts", [Kind, 'Elixir.Macro':to_string(NameAndArgs)]);

format_error(invalid_args_for_function_head) ->
  "only variables and \\\\ are allowed as arguments in function head.\n"
  "\n"
  "If you did not intend to define a function head, make sure your function "
  "definition has the proper syntax by wrapping the arguments in parentheses "
  "and using the do instruction accordingly:\n\n"
  "    def add(a, b), do: a + b\n\n"
  "    def add(a, b) do\n"
  "      a + b\n"
  "    end\n";

format_error({'__info__', Kind}) ->
  io_lib:format("cannot define ~ts __info__/1 as it is automatically defined by Elixir", [Kind]);

format_error({module_info, Kind, Arity}) ->
  io_lib:format("cannot define ~ts module_info/~B as it is automatically defined by Erlang", [Kind, Arity]);

format_error({is_record, Kind}) ->
  io_lib:format("cannot define ~ts is_record/2 due to compatibility "
                "issues with the Erlang compiler (it is a known limitation)", [Kind]).
