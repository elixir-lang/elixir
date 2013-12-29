% Holds the logic responsible for functions definition (def(p) and defmacro(p)).
-module(elixir_def).
-export([table/1, clauses_table/1, setup/1,
  cleanup/1, reset_last/1, lookup_definition/2,
  delete_definition/2, store_definition/5, unwrap_definitions/2,
  store_each/8, format_error/1]).
-include("elixir.hrl").

-define(attr, '__def_table').
-define(clauses_attr, '__clauses_table').

%% Table management functions. Called internally.

table(Module) ->
  ets:lookup_element(Module, ?attr, 2).

clauses_table(Module) ->
  ets:lookup_element(Module, ?clauses_attr, 2).

setup(Module) ->
  ets:insert(Module, { ?attr, ets:new(Module, [set, public]) }),
  ets:insert(Module, { ?clauses_attr, ets:new(Module, [bag, public]) }),
  reset_last(Module),
  ok.

cleanup(Module) ->
  ets:delete(table(Module)),
  ets:delete(clauses_table(Module)).

%% Reset the last item. Useful when evaling code.
reset_last(Module) ->
  ets:insert(table(Module), { last, [] }).

%% Looks up a definition from the database.
lookup_definition(Module, Tuple) ->
  case ets:lookup(table(Module), Tuple) of
    [Result] ->
      CTable = clauses_table(Module),
      { Result, [Clause || { _, Clause } <- ets:lookup(CTable, Tuple)] };
    _ ->
      false
  end.

delete_definition(Module, Tuple) ->
  ets:delete(table(Module), Tuple),
  ets:delete(clauses_table(Module), Tuple).

% Invoked by the wrap definition with the function abstract tree.
% Each function is then added to the function table.

store_definition(Kind, CheckClauses, Call, Body, ExEnv) ->
  #elixir_env{line=Line} = E = elixir_env:ex_to_env(ExEnv),
  { NameAndArgs, Guards } = elixir_clauses:extract_guards(Call),

  { Name, Args } = case NameAndArgs of
    { N, _, A } when is_atom(N), is_atom(A) -> { N, [] };
    { N, _, A } when is_atom(N), is_list(A) -> { N, A };
    _ ->
      Format = [Kind, 'Elixir.Macro':to_string(NameAndArgs)],
      elixir_errors:syntax_error(Line, E#elixir_env.file, "invalid syntax in ~ts ~ts", Format)
  end,

  %% Now that we have verified the call format,
  %% extract meta information like file and context.
  { _, Meta, _ } = Call,
  DoCheckClauses = (not lists:keymember(context, 1, Meta)) andalso (CheckClauses),

  %% Check if there is a file information in the definition.
  %% If so, we assume this come from another source and we need
  %% to linify taking into account keep line numbers.
  { File, Key }  = case lists:keyfind(file, 1, Meta) of
    { file, Bin } when is_binary(Bin) -> { Bin, keep };
    _ -> { nil, line }
  end,

  LinifyArgs   = elixir_quote:linify(Line, Key, Args),
  LinifyGuards = elixir_quote:linify(Line, Key, Guards),
  LinifyBody   = elixir_quote:linify(Line, Key, Body),

  assert_no_aliases_name(Line, Name, Args, E),
  store_definition(Kind, Line, DoCheckClauses, Name,
                   LinifyArgs, LinifyGuards, LinifyBody, File, E).

store_definition(Kind, Line, CheckClauses, Name, Args, Guards, Body, MetaFile, #elixir_env{module=Module} = ER) ->
  Arity = length(Args),
  Tuple = { Name, Arity },
  E = ER#elixir_env{function=Tuple,vars=[]},
  elixir_locals:record_definition(Tuple, Kind, Module),

  Location = retrieve_file(Line, MetaFile, Module, E),
  run_on_definition_callbacks(Kind, Line, Module, Name, Args, Guards, Body, E),
  { Function, Defaults, Super } = translate_definition(Kind, Line, Name, Args, Guards, Body, E),

  DefaultsLength = length(Defaults),
  elixir_locals:record_defaults(Tuple, Kind, Module, DefaultsLength),

  File   = E#elixir_env.file,
  Table  = table(Module),
  CTable = clauses_table(Module),

  compile_super(Module, Super, E),
  check_previous_defaults(Table, Line, Name, Arity, Kind, DefaultsLength, E),

  store_each(CheckClauses, Kind, File, Location,
    Table, CTable, DefaultsLength, Function),
  [store_each(false, Kind, File, Location, Table, CTable, 0,
    default_function_for(Kind, Name, Default)) || Default <- Defaults],

  { Name, Arity }.

%% @on_definition

run_on_definition_callbacks(Kind, Line, Module, Name, Args, Guards, Expr, E) ->
  case elixir_compiler:get_opt(internal) of
    true ->
      ok;
    _ ->
      Env = elixir_env:env_to_ex({ Line, E }),
      Callbacks = 'Elixir.Module':get_attribute(Module, on_definition),
      [Mod:Fun(Env, Kind, Name, Args, Guards, Expr) || { Mod, Fun } <- Callbacks]
  end.

%% Retrieve meta file, @file or fallback to default

retrieve_file(Line, File, Module, E) ->
  case not(elixir_compiler:get_opt(internal)) andalso
       'Elixir.Module':get_attribute(Module, file) of
    X when X == nil; X == false ->
      { elixir_utils:characters_to_list(retrieve_file(File, E)), Line };
    X ->
      'Elixir.Module':delete_attribute(Module, file),
      { X, 0 }
  end.

retrieve_file(nil, E)   -> E#elixir_env.file;
retrieve_file(File, _E) -> File.

%% Compile super

compile_super(Module, true, #elixir_env{function=Function}) ->
  elixir_def_overridable:store(Module, Function, true);
compile_super(_Module, _, _E) -> ok.

%% Translate the given call and expression given
%% and then store it in memory.

translate_definition(Kind, Line, Name, Args, Guards, Body, E) when is_integer(Line) ->
  Arity = length(Args),

  %% Macros receive a special argument on invocation. Notice it does
  %% not affect the arity of the stored function, but the clause
  %% already contains it.
  AllArgs = case is_macro(Kind) of
    true  -> [{ '_@CALLER', [{line,Line}], nil }|Args];
    false -> Args
  end,

  { EArgs, EGuards, EBody, _ } = elixir_exp_clauses:def(fun elixir_def_defaults:expand/2,
                                   AllArgs, Guards, expr_from_body(Line, Body), E),

  S = elixir_env:env_to_scope(E),
  { Unpacked, Defaults } = elixir_def_defaults:unpack(Kind, Name, EArgs, S),
  { Clauses, Super } = translate_clause(Body, Line, Kind, Unpacked, EGuards, EBody, S),

  Function = { function, Line, Name, Arity, Clauses },
  { Function, Defaults, Super }.

translate_clause(nil, _Line, _Kind, _Args, [], _Body, _S) ->
  { [], false };
translate_clause(nil, Line, Kind, _Args, _Guards, _Body, #elixir_scope{file=File}) ->
  elixir_errors:compile_error(Line, File, "missing do keyword in ~ts", [Kind]);
translate_clause(_, Line, Kind, Args, Guards, Body, S) ->
  { TClause, TS } = elixir_clauses:assigns_block(Line,
    fun elixir_translator:translate/2, Args, [Body], Guards, S),

  %% Set __CALLER__ if used
  FClause = case is_macro(Kind) andalso TS#elixir_scope.caller of
    true  ->
      FBody = { 'match', Line,
        { 'var', Line, '__CALLER__' },
        ?wrap_call(Line, elixir_env, env_to_ex, [{ var, Line, '_@CALLER' }])
      },
      setelement(5, TClause, [FBody|element(5, TClause)]);
    false -> TClause
  end,

  { [FClause], TS#elixir_scope.super }.

expr_from_body(_Line, nil)            -> nil;
expr_from_body(_Line, [{ do, Expr }]) -> Expr;
expr_from_body(Line, Else)            -> { 'try', [{line,Line}], [Else] }.

is_macro(defmacro)  -> true;
is_macro(defmacrop) -> true;
is_macro(_)         -> false.

% Unwrap the functions stored in the functions table.
% It returns a list of all functions to be exported, plus the macros,
% and the body of all functions.
unwrap_definitions(File, Module) ->
  Table = table(Module),
  CTable = clauses_table(Module),
  ets:delete(Table, last),
  unwrap_definition(ets:tab2list(Table), CTable, File, [], [], [], [], [], [], []).

unwrap_definition([Fun|T], CTable, File, All, Exports, Private, Def, Defmacro, Functions, Tail) ->
  Tuple   = element(1, Fun),
  Clauses = [Clause || { _, Clause } <- ets:lookup(CTable, Tuple)],

  { NewFun, NewExports, NewPrivate, NewDef, NewDefmacro } =
    case Clauses of
      [] -> { false, Exports, Private, Def, Defmacro };
      _  -> unwrap_definition(element(2, Fun), Tuple, Fun, Exports, Private, Def, Defmacro)
    end,

  { NewFunctions, NewTail } = case NewFun of
    false ->
      NewAll = All,
      { Functions, Tail };
    _ ->
      NewAll = [Tuple|All],
      function_for_stored_definition(NewFun, Clauses, File, Functions, Tail)
  end,

  unwrap_definition(T, CTable, File, NewAll, NewExports, NewPrivate,
    NewDef, NewDefmacro, NewFunctions, NewTail);

unwrap_definition([], _CTable, _File, All, Exports, Private, Def, Defmacro, Functions, Tail) ->
  { All, Exports, Private, ordsets:from_list(Def),
    ordsets:from_list(Defmacro), lists:reverse(Tail ++ Functions) }.

unwrap_definition(def, Tuple, Fun, Exports, Private, Def, Defmacro) ->
  { Fun, [Tuple|Exports], Private, [Tuple|Def], Defmacro };

unwrap_definition(defmacro, { Name, Arity } = Tuple, Fun, Exports, Private, Def, Defmacro) ->
  Macro = { ?elixir_macro(Name), Arity + 1 },
  { setelement(1, Fun, Macro), [Macro|Exports], Private, Def, [Tuple|Defmacro] };

unwrap_definition(defp, Tuple, Fun, Exports, Private, Def, Defmacro) ->
  %% { Name, Arity }, Kind, Line, Check, Defaults
  Info = { Tuple, defp, element(3, Fun), element(5, Fun), element(7, Fun) },
  { Fun, Exports, [Info|Private], Def, Defmacro };

unwrap_definition(defmacrop, Tuple, Fun, Exports, Private, Def, Defmacro) ->
  %% { Name, Arity }, Kind, Line, Check, Defaults
  Info  = { Tuple, defmacrop, element(3, Fun), element(5, Fun), element(7, Fun) },
  { false, Exports, [Info|Private], Def, Defmacro }.

%% Helpers

function_for_stored_definition({{Name,Arity}, _, Line, _, _, {File, _}, _}, Clauses, File, Functions, Tail) ->
  { [{ function, Line, Name, Arity, Clauses }|Functions], Tail };

function_for_stored_definition({{Name,Arity}, _, Line, _, _, Location, _}, Clauses, _File, Functions, Tail) ->
  { Functions, [
    { function, Line, Name, Arity, Clauses },
    { attribute, Line, file, Location } | Tail
  ] }.

default_function_for(Kind, Name, { clause, Line, Args, _Guards, _Exprs } = Clause)
    when Kind == defmacro; Kind == defmacrop ->
  { function, Line, Name, length(Args) - 1, [Clause] };

default_function_for(_, Name, { clause, Line, Args, _Guards, _Exprs } = Clause) ->
  { function, Line, Name, length(Args), [Clause] }.

%% Store each definition in the table.
%% This function also checks and emit warnings in case
%% the kind, of the visibility of the function changes.

store_each(Check, Kind, File, Location, Table, CTable, Defaults, {function, Line, Name, Arity, Clauses}) ->
  Tuple = { Name, Arity },
  case ets:lookup(Table, Tuple) of
    [{ Tuple, StoredKind, StoredLine, StoredFile, StoredCheck, StoredLocation, StoredDefaults }] ->
      FinalLine = StoredLine,
      FinalLocation = StoredLocation,
      FinalDefaults = max(Defaults, StoredDefaults),
      check_valid_kind(Line, File, Name, Arity, Kind, StoredKind),
      (Check and StoredCheck) andalso
        check_valid_clause(Line, File, Name, Arity, Kind, Table, StoredLine, StoredFile),
      check_valid_defaults(Line, File, Name, Arity, Kind, Defaults, StoredDefaults);
    [] ->
      FinalLine = Line,
      FinalLocation = Location,
      FinalDefaults = Defaults
  end,
  Check andalso ets:insert(Table, { last, { Name, Arity } }),
  ets:insert(CTable, [{ Tuple, Clause } || Clause <- Clauses ]),
  ets:insert(Table, { Tuple, Kind, FinalLine, File, Check, FinalLocation, FinalDefaults }).

%% Validations

check_valid_kind(_Line, _File, _Name, _Arity, Kind, Kind) -> [];
check_valid_kind(Line, File, Name, Arity, Kind, StoredKind) ->
  elixir_errors:form_error(Line, File, ?MODULE,
    { changed_kind, { Name, Arity, StoredKind, Kind } }).

check_valid_clause(Line, File, Name, Arity, Kind, Table, StoredLine, StoredFile) ->
  case ets:lookup_element(Table, last, 2) of
    {Name,Arity} -> [];
    [] -> [];
    _ ->
      Relative = elixir_utils:relative_to_cwd(StoredFile),
      elixir_errors:handle_file_warning(File, { Line, ?MODULE,
        { ungrouped_clause, { Kind, Name, Arity, StoredLine, Relative } } })
  end.

check_valid_defaults(_Line, _File, _Name, _Arity, _Kind, 0, _) -> [];
check_valid_defaults(Line, File, Name, Arity, Kind, _, 0) ->
  elixir_errors:handle_file_warning(File, { Line, ?MODULE, { out_of_order_defaults, { Kind, Name, Arity } } });
check_valid_defaults(Line, File, Name, Arity, Kind, _, _) ->
  elixir_errors:form_error(Line, File, ?MODULE, { clauses_with_defaults, { Kind, Name, Arity } }).

check_previous_defaults(Table, Line, Name, Arity, Kind, Defaults, E) ->
  Matches = ets:match(Table, { { Name, '$2' }, '$1', '_', '_', '_', '_', '$3' }),
  [ begin
      elixir_errors:form_error(Line, E#elixir_env.file, ?MODULE,
        { defs_with_defaults, Name, { Kind, Arity }, { K, A } })
    end || [K, A, D] <- Matches, A /= Arity, D /= 0, defaults_conflict(A, D, Arity, Defaults)].

defaults_conflict(A, D, Arity, Defaults) ->
  ((Arity >= (A - D)) andalso (Arity < A)) orelse
    ((A >= (Arity - Defaults)) andalso (A < Arity)).

assert_no_aliases_name(Line, '__aliases__', [Atom], #elixir_env{file=File}) when is_atom(Atom) ->
  Message = "function names should start with lowercase characters or underscore, invalid name ~ts",
  elixir_errors:compile_error(Line, File, Message, [Atom]);

assert_no_aliases_name(_Meta, _Aliases, _Args, _S) ->
  ok.

%% Format errors

format_error({no_module,{Kind,Name,Arity}}) ->
  io_lib:format("cannot define function outside module, invalid scope for ~ts ~ts/~B", [Kind, Name, Arity]);

format_error({defs_with_defaults, Name, { Kind, Arity }, { K, A } }) when Arity > A ->
  io_lib:format("~ts ~ts/~B defaults conflicts with ~ts ~ts/~B",
    [Kind, Name, Arity, K, Name, A]);

format_error({defs_with_defaults, Name, { Kind, Arity }, { K, A } }) when Arity < A ->
  io_lib:format("~ts ~ts/~B conflicts with defaults from ~ts ~ts/~B",
    [Kind, Name, Arity, K, Name, A]);

format_error({clauses_with_defaults,{Kind,Name,Arity}}) ->
  io_lib:format("~ts ~ts/~B has default values and multiple clauses, "
    "use a separate clause for declaring defaults", [Kind, Name, Arity]);

format_error({out_of_order_defaults,{Kind,Name,Arity}}) ->
  io_lib:format("clause with defaults should be the first clause in ~ts ~ts/~B", [Kind, Name, Arity]);

format_error({ungrouped_clause,{Kind,Name,Arity,OrigLine,OrigFile}}) ->
  io_lib:format("clauses for the same ~ts should be grouped together, ~ts ~ts/~B was previously defined (~ts:~B)",
    [Kind, Kind, Name, Arity, OrigFile, OrigLine]);

format_error({changed_kind,{Name,Arity,Previous,Current}}) ->
  io_lib:format("~ts ~ts/~B already defined as ~ts", [Current, Name, Arity, Previous]).
