% Holds the logic responsible for functions definition (def(p) and defmacro(p)).
-module(elixir_def).
-export([table/1,
  clauses_table/1,
  build_table/1,
  delete_table/1,
  reset_last/1,
  lookup_definition/2,
  delete_definition/2,
  wrap_definition/5,
  wrap_definition/7,
  store_definition/6,
  store_definition/8,
  store_each/8,
  unwrap_stored_definitions/2,
  format_error/1]).
-include("elixir.hrl").
-compile({parse_transform, elixir_transform}).

%% Table management functions. Called internally.

table(Module) -> ?atom_concat([f, Module]).
clauses_table(Module) -> ?atom_concat([c, Module]).

build_table(Module) ->
  FunctionsTable = table(Module),
  ClausesTable = clauses_table(Module),
  ets:new(FunctionsTable, [set, named_table, public]),
  ets:new(ClausesTable, [bag, named_table, public]),
  reset_last(Module),
  { FunctionsTable, ClausesTable }.

delete_table(Module) ->
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

%% Wraps the function into a call to store_definition once the function
%% definition is read. The function is compiled into a meta tree to ensure
%% we will receive the full function.
%%
%% We need to wrap functions instead of eagerly defining them to ensure
%% functions inside branches won't propagate, for example:
%%
%%   if false do
%%     def bar, do: 1
%%   else
%%     def bar, do: 2
%%   end
%%
%% If we just analyzed the compiled structure (i.e. the function availables
%% before evaluating the function body), we would see both definitions.

wrap_definition(Kind, Meta, Call, Expr, S) ->
  do_wrap_definition(Kind, Meta, [
    Call, Expr, elixir_scope:serialize(S)
  ]).

wrap_definition(Kind, Meta, Name, Args, Guards, Expr, S) ->
  do_wrap_definition(Kind, Meta, [
    Name, Args, Guards, Expr, elixir_scope:serialize(S)
  ]).

do_wrap_definition(Kind, Meta, Extra) ->
  Line   = ?line(Meta),
  Invoke =
    [{atom, Line, Kind},
     {integer, Line, Line},
     {var, Line, '_@MODULE'}] ++ Extra,

  ?wrap_call(Line, ?MODULE, store_definition, Invoke).

% Invoked by the wrap definition with the function abstract tree.
% Each function is then added to the function table.
store_definition(Kind, Line, Module, Call, Body, RawS) ->
  S = elixir_scope:deserialize(RawS),
  { NameAndArgs, Guards } = elixir_clauses:extract_guards(Call),

  { Name, Args } = case elixir_clauses:extract_args(NameAndArgs) of
    error ->
      Format = [Kind, 'Elixir.Macro':to_binary(NameAndArgs)],
      elixir_errors:syntax_error(Line, S#elixir_scope.file, "invalid syntax in ~ts ~ts", Format);
    Tuple ->
      Tuple
  end,

  assert_no_aliases_name(Line, Name, Args, S),
  store_definition(Kind, Line, Module, Name, Args, Guards, Body, RawS).

store_definition(Kind, Line, nil, _Name, _Args, _Guards, _Body, RawS) ->
  S = elixir_scope:deserialize(RawS),
  elixir_errors:syntax_error(Line, S#elixir_scope.file, "cannot define function outside module, invalid scope for ~ts", [Kind]);

store_definition(Kind, Line, Module, Name, Args, Guards, Body, RawS) ->
  do_store_definition(Kind, Line, Module, Name, Args, Guards, Body, elixir_scope:deserialize(RawS)).

do_store_definition(Kind, Line, Module, Name, Args, Guards, Body, DS) ->
  Arity = length(Args),
  S     = DS#elixir_scope{function={Name,Arity}, module=Module},
  Expr  = def_body(Line, Body),

  CO = elixir_compiler:get_opts(),
  Location = retrieve_file(Line, Module, S, CO),
  run_on_definition_callbacks(Kind, Line, Module, Name, Args, Guards, Body, S, CO),

  { Function, Defaults, TS } = translate_definition(Kind, Line, Name, Args, Guards, Expr, S),

  File  = TS#elixir_scope.file,
  Table = table(Module),
  CTable = clauses_table(Module),

  %% Store function
  if
    (Body == nil) -> [];
    true ->
      compile_super(Module, TS),
      CheckClauses = S#elixir_scope.check_clauses,
      store_each(CheckClauses, Kind, File, Location,
        Table, CTable, length(Defaults), Function)
  end,

  [store_each(false, Kind, File, Location, Table, CTable, 0,
    default_function_for(Kind, Name, Default)) || Default <- Defaults],

  { Name, Arity }.

def_body(_Line, nil)            -> nil;
def_body(_Line, [{ do, Expr }]) -> Expr;
def_body(Line, Else)            -> { 'try', [{line,Line}], [Else] }.

%% @on_definition

run_on_definition_callbacks(Kind, Line, Module, Name, Args, Guards, Expr, S, CO) ->
  case elixir_compiler:get_opt(internal, CO) of
    true ->
      ok;
    _ ->
      Env = elixir_scope:to_ex_env({ Line, S }),
      elixir_module:eval_callbacks(Line, Module, on_definition,
        [Env, Kind, Name, Args, Guards, Expr], S)
  end.

%% Retrieve @file or fallback to default

retrieve_file(Line, Module, S, CO) ->
  case elixir_compiler:get_opt(internal, CO) of
    true -> { binary_to_list(S#elixir_scope.file), Line };
    _ ->
      case 'Elixir.Module':get_attribute(Module, file) of
        nil  -> { binary_to_list(S#elixir_scope.file), Line };
        Else ->
          'Elixir.Module':delete_attribute(Module, file),
          Else
      end
  end.

%% Compile super

compile_super(Module, #elixir_scope{function=Function, super=true}) ->
  elixir_def_overridable:store(Module, Function, true);

compile_super(_Module, _S) -> ok.

%% Translate the given call and expression given
%% and then store it in memory.

translate_definition(Kind, Line, Name, RawArgs, RawGuards, RawExpr, S) when is_integer(Line) ->
  Args    = elixir_quote:linify(Line, RawArgs),
  Guards  = elixir_quote:linify(Line, RawGuards),
  Expr    = elixir_quote:linify(Line, RawExpr),
  Arity   = length(Args),
  IsMacro = is_macro(Kind),

  %% Macros receive a special argument on invocation. Notice it does
  %% not affect the arity of the stored function, but the clause
  %% already contains it.
  ExtendedArgs = case IsMacro of
    true  -> [{ '_@CALLER', [{line,Line}], nil }|Args];
    false -> Args
  end,

  { Unpacked, Defaults } = elixir_def_defaults:unpack(Kind, Name, ExtendedArgs, S),

  { TClause, TS } = elixir_clauses:assigns_block(Line,
    fun elixir_translator:translate/2, Unpacked, [Expr], Guards, S),

  %% Add names to args
  NClause = case TS#elixir_scope.name_args of
    true  ->
      NArgs = elixir_def_overridable:assign_args(Line, element(3, TClause), TS),
      setelement(3, TClause, NArgs);
    false -> TClause
  end,

  %% Set __CALLER__ if used
  FClause = case IsMacro andalso TS#elixir_scope.caller of
    true  ->
      FBody = { 'match', Line,
        { 'var', Line, '__CALLER__' },
        ?wrap_call(Line, elixir_scope, to_ex_env, [{ var, Line, '_@CALLER' }])
      },
      setelement(5, NClause, [FBody|element(5, NClause)]);
    false -> NClause
  end,

  Function = { function, Line, Name, Arity, [FClause] },
  { Function, Defaults, TS }.

is_macro(defmacro)  -> true;
is_macro(defmacrop) -> true;
is_macro(_)         -> false.

% Unwrap the functions stored in the functions table.
% It returns a list of all functions to be exported, plus the macros,
% and the body of all functions.
unwrap_stored_definitions(File, Module) ->
  Table = table(Module),
  CTable = clauses_table(Module),
  ets:delete(Table, last),
  unwrap_stored_definition(ets:tab2list(Table), CTable, File, [], [], [], [], [], {[],[]}).

unwrap_stored_definition([Fun|T], CTable, File, Exports, Private, Def, Defmacro, Defmacrop, Functions) when element(2, Fun) == def ->
  Tuple = element(1, Fun),
  unwrap_stored_definition(
    T, CTable, File, [Tuple|Exports], Private, [Tuple|Def], Defmacro, Defmacrop,
    function_for_stored_definition(Fun, CTable, Tuple, File, Functions)
  );

unwrap_stored_definition([Fun|T], CTable, File, Exports, Private, Def, Defmacro, Defmacrop, Functions) when element(2, Fun) == defmacro ->
  { Name, Arity } = Tuple = element(1, Fun),
  Macro = { ?elixir_macro(Name), Arity + 1 },

  unwrap_stored_definition(
    T, CTable, File, [Macro|Exports], Private, Def, [Tuple|Defmacro], Defmacrop,
    function_for_stored_definition(setelement(1, Fun, Macro), CTable, Tuple, File, Functions)
  );

unwrap_stored_definition([Fun|T], CTable, File, Exports, Private, Def, Defmacro, Defmacrop, Functions) when element(2, Fun) == defp ->
  Tuple = element(1, Fun),
  unwrap_stored_definition(
    T, CTable, File, Exports, [Tuple|Private], Def, Defmacro, Defmacrop,
    function_for_stored_definition(Fun, CTable, Tuple, File, Functions)
  );

unwrap_stored_definition([Fun|T], CTable, File, Exports, Private, Def, Defmacro, Defmacrop, Functions) when element(2, Fun) == defmacrop ->
  Tuple = element(1, Fun),
  unwrap_stored_definition(
    T, CTable, File, Exports, [Tuple|Private], Def, Defmacro,
    [{ Tuple, element(3, Fun), element(5, Fun) }|Defmacrop], Functions
  );

unwrap_stored_definition([], _CTable, _File, Exports, Private, Def, Defmacro, Defmacrop, {Functions,Tail}) ->
  { Exports, Private, ordsets:from_list(Def), ordsets:from_list(Defmacro),
    ordsets:from_list(Defmacrop), lists:reverse(Tail ++ Functions) }.

%% Helpers

function_for_stored_definition({{Name,Arity}, _, Line, _, _, {File, _}, _}, CTable, Tuple, File, {Functions,Tail}) ->
  {
    [{ function, Line, Name, Arity, default_clauses_for(CTable, Tuple) }|Functions],
    Tail
  };

function_for_stored_definition({{Name,Arity}, _, Line, _, _, Location, _}, CTable, Tuple, _File, {Functions,Tail}) ->
  {
    Functions,
    [
      { function, Line, Name, Arity, default_clauses_for(CTable, Tuple) },
      { attribute, Line, file, Location } | Tail
    ]
  }.

default_clauses_for(CTable, Tuple) ->
  [Clause || { _, Clause } <- ets:lookup(CTable, Tuple)].

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
    [{ Tuple, StoredKind, _, _, StoredCheck, StoredLocation, StoredDefaults }] ->
      FinalLocation = StoredLocation,
      FinalDefaults = Defaults + StoredDefaults,
      check_valid_kind(Line, File, Name, Arity, Kind, StoredKind),
      check_valid_defaults(Line, File, Name, Arity, FinalDefaults),
      (Check and StoredCheck) andalso check_valid_clause(Line, File, Name, Arity, Table);
    [] ->
      FinalLocation = Location,
      FinalDefaults = Defaults
  end,
  Check andalso ets:insert(Table, { last, { Name, Arity } }),
  ets:insert(CTable, [{ Tuple, Clause } || Clause <- Clauses ]),
  ets:insert(Table, { Tuple, Kind, Line, File, Check, FinalLocation, FinalDefaults }).

%% Validations

check_valid_kind(_Line, _File, _Name, _Arity, Kind, Kind) -> [];
check_valid_kind(Line, File, Name, Arity, Kind, StoredKind) ->
  elixir_errors:form_error(Line, File, ?MODULE,
    { changed_kind, { Name, Arity, StoredKind, Kind } }).

check_valid_clause(Line, File, Name, Arity, Table) ->
  case ets:lookup_element(Table, last, 2) of
    {Name,Arity} -> [];
    [] -> [];
    _ ->
      elixir_errors:handle_file_warning(File, { Line, ?MODULE,
        { override_function, { Name, Arity } } })
  end.

check_valid_defaults(_Line, _File, _Name, _Arity, 0) -> [];
check_valid_defaults(Line, File, Name, Arity, _) ->
  elixir_errors:handle_file_warning(File, { Line, ?MODULE, { clauses_with_docs, { Name, Arity } } }).

assert_no_aliases_name(Line, '__aliases__', [Atom], #elixir_scope{file=File}) when is_atom(Atom) ->
  Message = "function names should start with lowercase characters or underscore, invalid name ~ts",
  elixir_errors:syntax_error(Line, File, Message, [atom_to_binary(Atom, utf8)]);

assert_no_aliases_name(_Meta, _Aliases, _Args, _S) ->
  ok.

%% Format errors

format_error({clauses_with_docs,{Name,Arity}}) ->
  io_lib:format("function ~ts/~B has default values and multiple clauses, use a separate clause for declaring defaults", [Name, Arity]);

format_error({override_function,{Name,Arity}}) ->
  io_lib:format("trying to override previously defined function ~ts/~B", [Name, Arity]);

format_error({changed_kind,{Name,Arity,Previous,Current}}) ->
  io_lib:format("~ts ~ts/~B already defined as ~ts", [Current, Name, Arity, Previous]).