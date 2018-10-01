% Holds the logic responsible for function definitions (def(p) and defmacro(p)).
-module(elixir_def).
-export([setup/1, reset_last/1, local_for/4,
  take_definition/2, store_definition/5, store_definition/9,
  fetch_definitions/2, format_error/1]).
-include("elixir.hrl").
-define(last_def, {elixir, last_def}).

setup(DataTables) ->
  reset_last(DataTables),
  ok.

reset_last({DataSet, _DataBag}) ->
  ets:insert(DataSet, {?last_def, []});

reset_last(Module) when is_atom(Module) ->
  reset_last(elixir_module:data_tables(Module)).

local_for(Module, Name, Arity, Kinds) ->
  Tuple = {Name, Arity},

  try
    {Set, Bag} = elixir_module:data_tables(Module),
    {ets:lookup(Set, {def, Tuple}), ets:lookup(Bag, {clauses, Tuple})}
  of
    {[{_, Kind, Meta, _, _, _}], Clauses} ->
      case (Kinds == all) orelse (lists:member(Kind, Kinds)) of
        true -> elixir_erl:definition_to_anonymous(Module, Kind, Meta,
                                                   [Clause || {_, Clause} <- Clauses]);
        false -> false
      end;
    {[], _} ->
      false
  catch
    _:_ -> false
  end.

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

fetch_definitions(File, Module) ->
  {Set, Bag} = elixir_module:data_tables(Module),

  Entries = try
    lists:sort(ets:lookup_element(Bag, defs, 2))
  catch
    error:badarg -> []
  end,

  {All, Private} = fetch_definition(Entries, File, Module, Set, Bag, [], []),
  Unreachable = elixir_locals:warn_unused_local(File, Module, All, Private),
  elixir_locals:ensure_no_undefined_local(File, Module, All),
  elixir_locals:ensure_no_import_conflict(File, Module, All),
  {All, Unreachable}.

fetch_definition([Tuple | T], File, Module, Set, Bag, All, Private) ->
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
      fetch_definition(T, File, Module, Set, Bag, NewAll, NewPrivate)
  catch
    error:badarg ->
      warn_bodiless_function(Check, Meta, File, Module, Kind, Tuple),
      fetch_definition(T, File, Module, Set, Bag, All, Private)
  end;

fetch_definition([], _File, _Module, _Set, _Bag, All, Private) ->
  {All, Private}.

add_defaults_to_meta(0, Meta) -> Meta;
add_defaults_to_meta(Defaults, Meta) -> [{defaults, Defaults} | Meta].

head_and_definition_meta(true, Meta, 0, _All) ->
  Meta;
head_and_definition_meta(true, _Meta, _HeadDefaults, [{_, _, HeadMeta, _} | _]) ->
  HeadMeta;
head_and_definition_meta(false, _Meta, _HeadDefaults, _All) ->
  false.

%% Section for storing definitions

store_definition(Kind, CheckClauses, Call, Body, Pos) ->
  #{line := Line} = E = elixir_locals:get_cached_env(Pos),
  {NameAndArgs, Guards} = elixir_utils:extract_guards(Call),

  {Name, Args} = case NameAndArgs of
    {N, _, A} when is_atom(N), is_atom(A) -> {N, []};
    {N, _, A} when is_atom(N), is_list(A) -> {N, A};
    _ -> elixir_errors:form_error([{line, Line}], ?key(E, file), ?MODULE,
                                  {invalid_def, Kind, NameAndArgs})
  end,

  %% Now that we have verified the call format,
  %% extract meta information like file and context.
  {_, Meta, _} = Call,

  Context = case lists:keyfind(context, 1, Meta) of
    {context, _} = ContextPair -> [ContextPair];
    _ -> []
  end,

  Generated = case lists:keyfind(generated, 1, Meta) of
    {generated, true} -> ?generated(Context);
    _ -> Context
  end,

  DoCheckClauses = (Context == []) andalso (CheckClauses),

  %% Check if there is a file information in the definition.
  %% If so, we assume this come from another source and
  %% we need to linify taking into account keep line numbers.
  %%
  %% Line and File will always point to the caller. __ENV__.line
  %% will always point to the quoted one and __ENV__.file will
  %% always point to the one at @file or the quoted one.
  {Location, LinifyLine} =
    case elixir_utils:meta_keep(Meta) of
      {_, _} = MetaFile -> {MetaFile, Line};
      nil -> {nil, 0}
    end,

  Arity        = length(Args),
  LinifyArgs   = elixir_quote:linify(LinifyLine, keep, Args),
  LinifyGuards = elixir_quote:linify(LinifyLine, keep, Guards),
  LinifyBody   = elixir_quote:linify(LinifyLine, keep, Body),

  {File, DefMeta} =
    case retrieve_location(Location, ?key(E, module)) of
      {F, L} ->
        {F, [{line, Line}, {file, {F, L}} | Generated]};
      nil ->
        {nil, [{line, Line} | Generated]}
    end,

  run_with_location_change(File, E, fun(EL) ->
    assert_no_aliases_name(DefMeta, Name, Args, EL),
    assert_valid_name(DefMeta, Kind, Name, Args, EL),
    store_definition(DefMeta, Kind, DoCheckClauses, Name, Arity,
                     LinifyArgs, LinifyGuards, LinifyBody, ?key(E, file), EL)
  end).

store_definition(Meta, Kind, CheckClauses, Name, Arity, DefaultsArgs, Guards, Body, File, ER) ->
  Module = ?key(ER, module),
  Tuple = {Name, Arity},
  E = env_for_expansion(Kind, Tuple, ER),

  {Args, Defaults} = unpack_defaults(Kind, Meta, Name, DefaultsArgs, E),
  Clauses = [elixir_clauses:def(Clause, E) ||
             Clause <- def_to_clauses(Kind, Meta, Args, Guards, Body, E)],

  DefaultsLength = length(Defaults),
  elixir_locals:record_defaults(Tuple, Kind, Module, DefaultsLength, Meta),
  check_previous_defaults(Meta, Module, Name, Arity, Kind, DefaultsLength, E),

  store_definition(CheckClauses, Kind, Meta, Name, Arity, File,
                   Module, DefaultsLength, Clauses),
  [store_definition(false, Kind, Meta, Name, length(DefaultArgs), File,
                    Module, 0, [Default]) || {_, DefaultArgs, _, _} = Default <- Defaults],

  run_on_definition_callbacks(Kind, Module, Name, DefaultsArgs, Guards, Body, E),
  Tuple.

env_for_expansion(Kind, Tuple, E) when Kind =:= defmacro; Kind =:= defmacrop ->
  E#{function := Tuple, contextual_vars := ['__CALLER__']};
env_for_expansion(_Kind, Tuple, E) ->
  E#{function := Tuple, contextual_vars := []}.

retrieve_location(Location, Module) ->
  {Set, _} = elixir_module:data_tables(Module),
  case ets:take(Set, file) of
    [] when is_tuple(Location) ->
      {File, Line} = Location,
      {elixir_utils:relative_to_cwd(File), Line};
    [] ->
      nil;
    [{file, File, _}] when is_binary(File) ->
      'Elixir.Module':delete_attribute(Module, file),
      {elixir_utils:relative_to_cwd(File), 0};
    [{file, {File, Line}, _}] when is_binary(File) andalso is_integer(Line) ->
      'Elixir.Module':delete_attribute(Module, file),
      {elixir_utils:relative_to_cwd(File), Line}
  end.

run_with_location_change(nil, E, Callback) ->
  Callback(E);
run_with_location_change(File, #{file := File} = E, Callback) ->
  Callback(E);
run_with_location_change(File, E, Callback) ->
  EL = E#{file := File},
  Tracker = ?key(E, lexical_tracker),

  try
    elixir_lexical:set_file(File, Tracker),
    Callback(EL)
  after
    elixir_lexical:reset_file(Tracker)
  end.

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
      elixir_errors:form_error(Meta, ?key(E, file), elixir_expand, {missing_option, Kind, [do]})
  end.

run_on_definition_callbacks(Kind, Module, Name, Args, Guards, Body, E) ->
  {_, Bag} = elixir_module:data_tables(Module),
  Callbacks = ets:lookup_element(Bag, {accumulate, on_definition}, 2),
  _ = [Mod:Fun(E, Kind, Name, Args, Guards, Body) || {Mod, Fun} <- lists:reverse(Callbacks)],
  ok.

store_definition(Check, Kind, Meta, Name, Arity, File, Module, Defaults, Clauses) ->
  {Set, Bag} = elixir_module:data_tables(Module),
  Tuple = {Name, Arity},
  HasBody = Clauses =/= [],

  if
    Defaults > 0 ->
      ets:insert(Bag, {{default, Name}, Arity, Defaults});
    true ->
      ok
  end,

  {MaxDefaults, FirstMeta} =
    case ets:lookup(Set, {def, Tuple}) of
      [{_, StoredKind, StoredMeta, StoredFile, StoredCheck, {StoredDefaults, LastHasBody, LastDefaults}}] ->
        check_valid_kind(Meta, File, Name, Arity, Kind, StoredKind),
        (Check and StoredCheck) andalso
          check_valid_clause(Meta, File, Name, Arity, Kind, Set, StoredMeta, StoredFile),
        check_valid_defaults(Meta, File, Name, Arity, Kind, Defaults, StoredDefaults, LastDefaults, HasBody, LastHasBody),

        {max(Defaults, StoredDefaults), StoredMeta};
      [] ->
        ets:insert(Bag, {defs, Tuple}),
        {Defaults, Meta}
    end,

  Check andalso ets:insert(Set, {?last_def, Tuple}),
  ets:insert(Bag, [{{clauses, Tuple}, Clause} || Clause <- Clauses]),
  ets:insert(Set, {{def, Tuple}, Kind, FirstMeta, File, Check, {MaxDefaults, HasBody, Defaults}}).

%% Handling of defaults

unpack_defaults(Kind, Meta, Name, Args, E) ->
  Expanded = expand_defaults(Args, E#{context := nil}),
  unpack_defaults(Kind, Meta, Name, Expanded, [], []).

unpack_defaults(Kind, Meta, Name, [{'\\\\', DefaultMeta, [Expr, _]} | T] = List, Acc, Clauses) ->
  Base = match_defaults(Acc, length(Acc), []),
  {Args, Invoke} = extract_defaults(List, length(Base), [], []),
  Clause = {Meta, Base ++ Args, [], {super, DefaultMeta, [{Kind, Name} | Base] ++ Invoke}},
  unpack_defaults(Kind, Meta, Name, T, [Expr | Acc], [Clause | Clauses]);
unpack_defaults(Kind, Meta, Name, [H | T], Acc, Clauses) ->
  unpack_defaults(Kind, Meta, Name, T, [H | Acc], Clauses);
unpack_defaults(_Kind, _Meta, _Name, [], Acc, Clauses) ->
  {lists:reverse(Acc), lists:reverse(Clauses)}.

expand_defaults([{'\\\\', Meta, [Expr, Default]} | Args], E) ->
  {ExpandedDefault, _} = elixir_expand:expand(Default, E),
  [{'\\\\', Meta, [Expr, ExpandedDefault]} | expand_defaults(Args, E)];
expand_defaults([Arg | Args], E) ->
  [Arg | expand_defaults(Args, E)];
expand_defaults([], _E) ->
  [].

extract_defaults([{'\\\\', _, [_Expr, Default]} | T], Counter, NewArgs, NewInvoke) ->
  extract_defaults(T, Counter, NewArgs, [Default | NewInvoke]);
extract_defaults([_ | T], Counter, NewArgs, NewInvoke) ->
  H = default_var(Counter),
  extract_defaults(T, Counter + 1, [H | NewArgs], [H | NewInvoke]);
extract_defaults([], _Counter, NewArgs, NewInvoke) ->
  {lists:reverse(NewArgs), lists:reverse(NewInvoke)}.

match_defaults([], 0, Acc) ->
  Acc;
match_defaults([_ | T], Counter, Acc) ->
  NewCounter = Counter - 1,
  match_defaults(T, NewCounter, [default_var(NewCounter) | Acc]).

default_var(Counter) ->
  {list_to_atom([$x | integer_to_list(Counter)]), [{generated, true}], ?var_context}.

%% Validations

check_valid_kind(_Meta, _File, _Name, _Arity, Kind, Kind) -> ok;
check_valid_kind(Meta, File, Name, Arity, Kind, StoredKind) ->
  elixir_errors:form_error(Meta, File, ?MODULE,
    {changed_kind, {Name, Arity, StoredKind, Kind}}).

check_valid_clause(Meta, File, Name, Arity, Kind, Set, StoredMeta, StoredFile) ->
  case ets:lookup_element(Set, ?last_def, 2) of
    none -> ok;
    {Name, Arity} -> ok;
    {Name, _} ->
      Relative = elixir_utils:relative_to_cwd(StoredFile),
      elixir_errors:form_warn(Meta, File, ?MODULE,
        {ungrouped_name, {Kind, Name, Arity, ?line(StoredMeta), Relative}});
    _ ->
      Relative = elixir_utils:relative_to_cwd(StoredFile),
      elixir_errors:form_warn(Meta, File, ?MODULE,
        {ungrouped_arity, {Kind, Name, Arity, ?line(StoredMeta), Relative}})
  end.

% Clause with defaults after clause with defaults
check_valid_defaults(Meta, File, Name, Arity, Kind, Defaults, StoredDefaults, _, _, _)
    when Defaults > 0, StoredDefaults > 0 ->
  elixir_errors:form_error(Meta, File, ?MODULE, {duplicate_defaults, {Kind, Name, Arity}});
% Clause with defaults after clause without defaults
check_valid_defaults(Meta, File, Name, Arity, Kind, Defaults, 0, _, _, _) when Defaults > 0 ->
  elixir_errors:form_warn(Meta, File, ?MODULE, {mixed_defaults, {Kind, Name, Arity}});
% Clause without defaults directly after clause with defaults (bodiless does not count)
check_valid_defaults(Meta, File, Name, Arity, Kind, 0, _, LastDefaults, true, true) when LastDefaults > 0 ->
  elixir_errors:form_warn(Meta, File, ?MODULE, {mixed_defaults, {Kind, Name, Arity}});
% Clause without defaults
check_valid_defaults(_Meta, _File, _Name, _Arity, _Kind, 0, _StoredDefaults, _LastDefaults, _HasBody, _LastHasBody) ->
  ok.

warn_bodiless_function(Check, _Meta, _File, Module, _Kind, _Tuple)
    when Check == false; Module == 'Elixir.Module' ->
  ok;
warn_bodiless_function(_Check, Meta, File, _Module, Kind, Tuple) ->
  elixir_errors:form_warn(Meta, File, ?MODULE, {function_head, Kind, Tuple}),
  ok.

check_args_for_function_head(Meta, Args, E) ->
  [begin
     elixir_errors:form_error(Meta, ?key(E, file), ?MODULE, invalid_args_for_function_head)
   end || Arg <- Args, invalid_arg(Arg)].

invalid_arg({Name, _, Kind}) when is_atom(Name), is_atom(Kind) -> false;
invalid_arg(_) -> true.

check_previous_defaults(Meta, Module, Name, Arity, Kind, Defaults, E) ->
  {_Set, Bag} = elixir_module:data_tables(Module),
  Matches = ets:lookup(Bag, {default, Name}),
  [begin
     elixir_errors:form_error(Meta, ?key(E, file), ?MODULE,
       {defs_with_defaults, Kind, Name, Arity, A})
   end || {_, A, D} <- Matches, A /= Arity, D /= 0, defaults_conflict(A, D, Arity, Defaults)].

defaults_conflict(A, D, Arity, Defaults) ->
  ((Arity >= (A - D)) andalso (Arity < A)) orelse
    ((A >= (Arity - Defaults)) andalso (A < Arity)).

assert_no_aliases_name(Meta, '__aliases__', [Atom], #{file := File}) when is_atom(Atom) ->
  elixir_errors:form_error(Meta, File, ?MODULE, {no_alias, Atom});
assert_no_aliases_name(_Meta, _Aliases, _Args, _S) ->
  ok.

assert_valid_name(Meta, Kind, '__info__', [_], #{file := File, module := Module}) when Module /= 'Elixir.Module' ->
  elixir_errors:form_error(Meta, File, ?MODULE, {'__info__', Kind});
assert_valid_name(Meta, Kind, 'module_info', [], #{file := File}) ->
  elixir_errors:form_error(Meta, File, ?MODULE, {module_info, Kind, 0});
assert_valid_name(Meta, Kind, 'module_info', [_], #{file := File}) ->
  elixir_errors:form_error(Meta, File, ?MODULE, {module_info, Kind, 1});
assert_valid_name(Meta, Kind, is_record, [_, _], #{file := File}) when Kind == defp; Kind == def ->
  elixir_errors:form_error(Meta, File, ?MODULE, {is_record, Kind});
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

format_error({changed_kind, {Name, Arity, Previous, Current}}) ->
  io_lib:format("~ts ~ts/~B already defined as ~ts", [Current, Name, Arity, Previous]);

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
