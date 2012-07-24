% Holds the logic responsible for functions definition (def(p) and defmacro(p)).
-module(elixir_def).
-export([table/1,
  build_table/1,
  delete_table/1,
  reset_last/1,
  wrap_definition/7,
  store_definition/8,
  store_each/8,
  unwrap_stored_definitions/1,
  format_error/1]).
-include("elixir.hrl").
-compile({parse_transform, elixir_transform}).

%% Table management functions. Called internally.

table(Module) -> ?ELIXIR_ATOM_CONCAT([f, Module]).

build_table(Module) ->
  FunctionTable = table(Module),
  ets:new(FunctionTable, [set, named_table, public]),
  ets:insert(FunctionTable, { last, [] }),
  FunctionTable.

delete_table(Module) ->
  ets:delete(table(Module)).

%% Reset the last item. Useful when evaling code.
reset_last(Module) ->
  ets:insert(table(Module), { last, [] }).

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
wrap_definition(Kind, Line, Name, Args, Guards, Expr, S) ->
  MetaS = elixir_scope:serialize(S),

  Invoke = [
    {atom, Line, Kind},
    {integer, Line, Line},
    {var, Line, '_@MODULE'},
    Name,
    Args,
    Guards,
    Expr,
    MetaS
  ],

  ?ELIXIR_WRAP_CALL(Line, ?MODULE, store_definition, Invoke).

% Invoked by the wrap definition with the function abstract tree.
% Each function is then added to the function table.

store_definition(Kind, Line, nil, _Name, _Args, _Guards, _Expr, RawS) ->
  S = elixir_scope:deserialize(RawS),
  elixir_errors:syntax_error(Line, S#elixir_scope.file, "cannot define function outside module, invalid scope for ~s", [Kind]);

store_definition(Kind, Line, Module, Name, Args, Guards, RawExpr, RawS) ->
  Arity = length(Args),

  DS = elixir_scope:deserialize(RawS),
  S = DS#elixir_scope{function={Name,Arity}, module=Module},

  case RawExpr of
    skip_definition -> Expr = nil;
    [{ do, Expr }] -> [];
    _ -> Expr = { 'try', Line, [RawExpr] }
  end,

  { Function, Defaults, TS } = translate_definition(Kind, Line, Name, Args, Guards, Expr, S),

  File = TS#elixir_scope.file,
  FunctionTable = table(Module),

  CO = elixir_compiler:get_opts(),
  compile_docs(Kind, Line, Module, Name, Arity, Args, TS, CO),

  Location = retrieve_file(Module, CO),
  Stack = S#elixir_scope.macro,

  %% Store function
  case RawExpr of
    skip_definition -> [];
    _ ->
      compile_super(Module, TS),
      CheckClauses = S#elixir_scope.check_clauses,
      store_each(CheckClauses, Kind, File, Location,
        Stack, FunctionTable, length(Defaults), Function)
  end,

  %% Store defaults
  [store_each(false, Kind, File, Location, Stack, FunctionTable, 0,
    function_for_default(Kind, Name, Default)) || Default <- Defaults],

  { Name, Arity }.

%% Compile the documentation related to the module.

compile_super(Module, #elixir_scope{function=Function, super=true}) ->
  elixir_def_overridable:store(Module, Function, true);

compile_super(_Module, _S) -> [].

compile_docs(Kind, Line, Module, Name, Arity, Args, S, CO) ->
  case elixir_compiler:get_opt(docs, CO) of
    false -> [];
    true  ->
      case 'Elixir.Module':compile_doc(Module, Line, Kind, { Name, Arity }, Args) of
        { error, Message } -> elixir_errors:handle_file_warning(S#elixir_scope.file,
          { Line, ?MODULE, { Message, { Name, Arity } } });
        _ -> []
      end
  end.

retrieve_file(Module, CO) ->
  case elixir_compiler:get_opt(internal, CO) of
    true -> [];
    _ ->
      case 'Elixir.Module':read_attribute(Module, file) of
        nil  -> [];
        Else ->
          'Elixir.Module':delete_attribute(Module, file),
          Else
      end
  end.

%% Translate the given call and expression given
%% and then store it in memory.

translate_definition(Kind, Line, Name, RawArgs, RawGuards, Expr, S) ->
  { Args, Guards } = lists:mapfoldl(fun
      ({ 'in', _, [Left, _] } = X, Acc) ->
        { Left, add_to_guards(Line, X, Acc) };
      (X, Acc) ->
        { X, Acc }
    end, RawGuards, RawArgs),

  Arity = length(Args),
  IsMacro = (Kind == defmacro) orelse (Kind == defmacrop),

  %% Macros receive a special argument on invocation. Notice it does
  %% not affect the arity of the stored function, but the clause
  %% already contains it.
  ExtendedArgs = case IsMacro of
    true  -> [{ '_@CALLER', Line, nil }|Args];
    false -> Args
  end,

  { Unpacked, Defaults } = elixir_def_defaults:unpack(Kind, Name, ExtendedArgs, S),

  { TClause, TS } = elixir_clauses:assigns_block(Line,
    fun elixir_translator:translate/2, Unpacked, [Expr], Guards, S),

  NClause = case TS#elixir_scope.name_args of
    true  ->
      NArgs = elixir_def_overridable:assign_args(Line, element(3, TClause), TS),
      setelement(3, TClause, NArgs);
    false -> TClause
  end,

  FClause = case IsMacro andalso TS#elixir_scope.caller of
    true  ->
      FBody = { 'match', Line,
        { 'var', Line, '__CALLER__' },
        ?ELIXIR_WRAP_CALL(Line, elixir_scope, to_ex_env, [{ var, Line, '_@CALLER' }])
      },
      setelement(5, NClause, [FBody|element(5, NClause)]);
    false -> NClause
  end,

  Function = { function, Line, Name, Arity, [FClause] },
  { Function, Defaults, TS }.

% Unwrap the functions stored in the functions table.
% It returns a list of all functions to be exported, plus the macros,
% and the body of all functions.
unwrap_stored_definitions(Module) ->
  Table = table(Module),
  ets:delete(Table, last),
  unwrap_stored_definition(ets:tab2list(Table), [], [], [], [], [], {[],[]}).

unwrap_stored_definition([Fun|T], Exports, Private, Def, Defmacro, Defmacrop, Functions) when element(2, Fun) == def ->
  Tuple = element(1, Fun),
  unwrap_stored_definition(
    T, [Tuple|Exports], Private, [Tuple|Def], Defmacro, Defmacrop,
    function_for_stored_definition(Fun, Functions)
  );

unwrap_stored_definition([Fun|T], Exports, Private, Def, Defmacro, Defmacrop, Functions) when element(2, Fun) == defmacro ->
  { Name, Arity } = Tuple = element(1, Fun),
  Macro = { ?ELIXIR_MACRO(Name), Arity + 1 },

  unwrap_stored_definition(
    T, [Macro|Exports], Private, Def, [Tuple|Defmacro], Defmacrop,
    function_for_stored_definition(setelement(1, Fun, Macro), Functions)
  );

unwrap_stored_definition([Fun|T], Exports, Private, Def, Defmacro, Defmacrop, Functions) when element(2, Fun) == defp ->
  unwrap_stored_definition(
    T, Exports, [element(1, Fun)|Private], Def, Defmacro, Defmacrop,
    function_for_stored_definition(Fun, Functions)
  );

unwrap_stored_definition([Fun|T], Exports, Private, Def, Defmacro, Defmacrop, Functions) when element(2, Fun) == defmacrop ->
  unwrap_stored_definition(
    T, Exports, [element(1, Fun)|Private], Def, Defmacro,
    [{ element(1, Fun), element(3, Fun) }|Defmacrop], Functions
  );

unwrap_stored_definition([], Exports, Private, Def, Defmacro, Defmacrop, {Functions,Tail}) ->
  { Exports, Private, ordsets:from_list(Def), ordsets:from_list(Defmacro), ordsets:from_list(Defmacrop), lists:reverse(Tail ++ Functions) }.

%% Helpers

function_for_stored_definition({{Name, Arity}, _, Line, _, [], _, _, Clauses}, {Functions,Tail}) ->
  {
    [{ function, Line, Name, Arity, lists:reverse(Clauses) }|Functions],
    Tail
  };

function_for_stored_definition({{Name, Arity}, _, Line, _, Location, _, _, Clauses}, {Functions,Tail}) ->
  {
    Functions,
    [
      { function, Line, Name, Arity, lists:reverse(Clauses) },
      { attribute, Line, file, Location } | Tail
    ]
  }.

function_for_default(Kind, Name, { clause, Line, Args, _Guards, _Exprs } = Clause)
    when Kind == defmacro; Kind == defmacrop ->
  { function, Line, Name, length(Args) - 1, [Clause] };

function_for_default(_, Name, { clause, Line, Args, _Guards, _Exprs } = Clause) ->
  { function, Line, Name, length(Args), [Clause] }.

%% Store each definition in the table.
%% This function also checks and emit warnings in case
%% the kind, of the visibility of the function changes.

store_each(Check, Kind, File, Location, Stack, Table, Defaults, {function, Line, Name, Arity, Clauses}) ->
  case ets:lookup(Table, {Name, Arity}) of
    [{{Name, Arity}, StoredKind, _, _, StoredLocation, StoredStack, StoredDefaults, StoredClauses}] ->
      FinalLocation = StoredLocation,
      FinalDefaults = Defaults + StoredDefaults,
      FinalClauses  = Clauses ++ StoredClauses,
      check_valid_kind(Line, File, Name, Arity, Kind, StoredKind),
      check_valid_defaults(Line, File, Name, Arity, FinalDefaults),
      Check andalso (Stack == StoredStack) andalso check_valid_clause(Line, File, Name, Arity, Table);
    [] ->
      FinalLocation = Location,
      FinalDefaults = Defaults,
      FinalClauses  = Clauses,
      Check andalso ets:insert(Table, { last, { Name, Arity } })
  end,
  ets:insert(Table, {{Name, Arity}, Kind, Line, File, FinalLocation, Stack, FinalDefaults, FinalClauses}).

%% Validations

check_valid_kind(_Line, _File, _Name, _Arity, Kind, Kind) -> [];
check_valid_kind(Line, File, Name, Arity, Kind, StoredKind) ->
  elixir_errors:form_error(Line, File, ?MODULE,
    { changed_kind, { Name, Arity, StoredKind, Kind } }).

check_valid_clause(Line, File, Name, Arity, Table) ->
  case ets:lookup_element(Table, last, 2) of
    {Name,Arity} -> [];
    [] -> [];
    {ElseName, ElseArity} ->
      elixir_errors:handle_file_warning(File, { Line, ?MODULE,
        { changed_clause, { { Name, Arity }, { ElseName, ElseArity } } } })
  end.

check_valid_defaults(_Line, _File, _Name, _Arity, 0) -> [];
check_valid_defaults(Line, File, Name, Arity, _) ->
  elixir_errors:handle_file_warning(File, { Line, ?MODULE, { clauses_with_docs, { Name, Arity } } }).

%% Helpers

add_to_guards(_Line, Expr, []) ->
  [Expr];
add_to_guards(Line, Expr, Clauses) ->
  [{ 'and', Line, [Expr, Clause] } || Clause <- Clauses].

%% Format errors

format_error({clauses_with_docs,{Name,Arity}}) ->
  io_lib:format("function ~s/~B has default values and multiple clauses, use a separate clause for declaring defaults", [Name, Arity]);

format_error({private_doc,{Name,Arity}}) ->
  io_lib:format("function ~s/~B is private, @doc's are always discarded for private functions", [Name, Arity]);

format_error({existing_doc,{Name,Arity}}) ->
  io_lib:format("@doc's for function ~s/~B have been given more than once, the first version is being kept", [Name, Arity]);

format_error({changed_clause,{{Name,Arity},{ElseName,ElseArity}}}) ->
  io_lib:format("function ~s/~B does not match previous clause ~s/~B", [Name, Arity, ElseName, ElseArity]);

format_error({changed_kind,{Name,Arity,Previous,Current}}) ->
  io_lib:format("~s ~s/~B already defined as ~s", [Current, Name, Arity, Previous]).