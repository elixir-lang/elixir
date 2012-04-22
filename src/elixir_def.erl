% Holds the logic responsible for functions definition (def(p) and defmacro(p)).
-module(elixir_def).
-export([build_table/1,
  delete_table/1,
  reset_last/1,
  local_macro_for/3,
  wrap_definition/7,
  handle_definition/8,
  store_definition/8,
  unwrap_stored_definitions/1,
  format_error/1]).
-include("elixir.hrl").

%% Table management functions. Called internally.

table(Module) -> ?ELIXIR_ATOM_CONCAT([f, Module]).

build_table(Module) ->
  FunctionTable = table(Module),
  ets:new(FunctionTable, [set, named_table, private]),
  ets:insert(FunctionTable, { last, [] }),
  FunctionTable.

delete_table(Module) ->
  ets:delete(table(Module)).

%% Reset the last item. Useful when evaling code.
reset_last(Module) ->
  ets:insert(table(Module), { last, [] }).

%% Retrieves a local macro for the given module.
local_macro_for(_Line, _Tuple, #elixir_scope{module=[]}) -> false;

local_macro_for(Line, Tuple, #elixir_scope{module=Module}) ->
  case ets:lookup(table(Module), Tuple) of
    [{Tuple, _, Kind, _, Clauses}] when Kind == defmacro; Kind == defmacrop ->
      Fun = { 'fun', Line, {clauses, lists:reverse(Clauses)} },
      { value, Result, _Binding } = erl_eval:exprs([Fun], []),
      Result;
    _ -> false
  end.

%% Wraps the function into a call to store_definition once the function
%% definition is read. The function is compiled into a meta tree to ensure
%% we will receive the full function.
%%
%% We need to wrap functions instead of eagerly defining them to ensure
%% functions inside branches won't propagate, for example:
%%
%%   if false do
%%     def bar, do: 1
%%   else:
%%     def bar, do: 2
%%   end
%%
%% If we just analyzed the compiled structure (i.e. the function availables
%% before evaluating the function body), we would see both definitions.
wrap_definition(Kind, Line, Name, Args, Guards, Expr, S) ->
  MetaS = elixir_variables:serialize_scope(S),

  Invoke = [
    {atom, Line, Kind},
    {integer, Line, Line},
    {var, Line, '_EXMODULE'},
    Name,
    Args,
    Guards,
    Expr,
    MetaS
  ],

  ?ELIXIR_WRAP_CALL(Line, ?MODULE, handle_definition, Invoke).

% Invoked by the wrap definition with the function abstract tree.
% Each function is then added to the function table.

handle_definition(Kind, Line, nil, _Name, _Args, _Guards, _Expr, RawS) ->
  S = elixir_variables:deserialize_scope(RawS),
  elixir_errors:syntax_error(Line, S#elixir_scope.filename, "cannot define function outside module, invalid scope for ~s", [Kind]);

handle_definition(Kind, Line, Module, Name, Args, RawGuards, RawExpr, RawS) ->
  Data  = elixir_module:data(Module),
  Arity = length(Args),

  case orddict:find(overridable, Data) of
    { ok, true } ->
      elixir_def_overridable:define(Module, { Name, Arity}, { Kind, Line, Module, Name, Args, RawGuards, RawExpr, RawS }),
      elixir_module:data(Module, orddict:erase(overridable, Data));
    _ ->
      S1 = elixir_variables:deserialize_scope(RawS),
      S2 = S1#elixir_scope{function={Name,Arity}, module=Module},
      store_definition(Kind, Line, Module, Name, Args, RawGuards, RawExpr, S2)
  end.

%% Store the definition after is is handled.

store_definition(Kind, Line, Module, Name, Args, RawGuards, RawExpr, S) ->
  Guards = elixir_clauses:extract_guard_clauses(RawGuards),

  case RawExpr of
    skip_definition -> Expr = nil;
    [{ do, Expr }] -> [];
    _ -> Expr = { 'try', Line, [RawExpr] }
  end,

  { Function, Defaults, TS } = translate_definition(Line, Name, Args, Guards, Expr, S),

  Filename      = TS#elixir_scope.filename,
  Arity         = element(4, Function),
  FunctionTable = table(Module),

  %% Compile documentation
  compile_docs(Kind, Line, Module, Name, Arity, TS),

  %% Store function
  case RawExpr of
    skip_definition -> [];
    _ ->
      compile_super(Module, TS),
      CheckClauses = S#elixir_scope.check_clauses,
      store_each(CheckClauses, Kind, FunctionTable, length(Defaults), Filename, Function)
  end,

  %% Store defaults
  [store_each(false, Kind, FunctionTable, 0, Filename,
    function_for_clause(Name, Default)) || Default <- Defaults],

  { Name, Arity }.

%% Compile the documentation related to the module.

compile_super(Module, #elixir_scope{function=Function, super=true}) ->
  elixir_def_overridable:store(Module, Function, true);

compile_super(_Module, _S) -> [].

compile_docs(Kind, Line, Module, Name, Arity, S) ->
  case elixir_compiler:get_opt(internal) of
    true -> [];
    _ ->
      case '__MAIN__.Module':compile_doc(Module, Line, Kind, { Name, Arity }) of
        { error, Message } -> elixir_errors:handle_file_warning(S#elixir_scope.filename,
          { Line, ?MODULE, { Message, { Name, Arity } } });
        _ -> []
      end
  end.

%% Translate the given call and expression given
%% and then store it in memory.

translate_definition(Line, Name, Args, Guards, Expr, S) ->
  Arity = length(Args),
  { Unpacked, Defaults } = elixir_def_defaults:unpack(Name, Args, S),

  { TClause, TS } = elixir_clauses:assigns_block(Line,
    fun elixir_translator:translate/2, Unpacked, [Expr], Guards, S),

  FClause = case TS#elixir_scope.name_args of
    true  ->
      FArgs = elixir_def_overridable:assign_args(Line, element(3, TClause), TS),
      setelement(3, TClause, FArgs);
    false -> TClause
  end,

  Function = { function, Line, Name, Arity, [FClause] },
  { Function, Defaults, TS }.

% Unwrap the functions stored in the functions table.
% It returns a list of all functions to be exported, plus the macros,
% and the body of all functions.
unwrap_stored_definitions(Module) ->
  Table = table(Module),
  ets:delete(Table, last),
  unwrap_stored_definition(ets:tab2list(Table), [], [], [], []).

unwrap_stored_definition([Def|T], Public, Private, Macros, Functions) when element(3, Def) == def ->
  unwrap_stored_definition(
    T, [element(1, Def)|Public], Private, Macros,
    [function_for_stored_definition(Def)|Functions]
  );

unwrap_stored_definition([Def|T], Public, Private, Macros, Functions) when element(3, Def) == defmacro ->
  unwrap_stored_definition(
    T, [element(1, Def)|Public], Private, [element(1, Def)|Macros],
    [function_for_stored_definition(Def)|Functions]
  );

unwrap_stored_definition([Def|T], Public, Private, Macros, Functions) when element(3, Def) == defp ->
  unwrap_stored_definition(
    T, Public, [element(1, Def)|Private], Macros,
    [function_for_stored_definition(Def)|Functions]
  );

unwrap_stored_definition([Def|T], Public, Private, Macros, Functions) when element(3, Def) == defmacrop ->
  unwrap_stored_definition(
    T, Public, [element(1, Def)|Private], Macros, Functions
  );

unwrap_stored_definition([], Public, Private, Macros, Functions) ->
  { lists:reverse(Public), lists:reverse(Private), lists:reverse(Macros), lists:reverse(Functions) }.

%% Helpers

function_for_stored_definition({{Name, Arity}, Line, _, _, Clauses}) ->
  {function, Line, Name, Arity, lists:reverse(Clauses) }.

function_for_clause(Name, { clause, Line, Args, _Guards, _Exprs } = Clause) ->
  { function, Line, Name, length(Args), [Clause] }.

%% Store each definition in the table.
%% This function also checks and emit warnings in case
%% the kind, of the visibility of the function changes.

store_each(Check, Kind, Table, Defaults, Filename, {function, Line, Name, Arity, Clauses}) ->
  case ets:lookup(Table, {Name, Arity}) of
    [{{Name, Arity}, _, StoredKind, StoredDefaults, StoredClauses}] ->
      FinalDefaults = Defaults + StoredDefaults,
      FinalClauses  = Clauses ++ StoredClauses,
      check_valid_kind(Line, Filename, Name, Arity, Kind, StoredKind),
      check_valid_defaults(Line, Filename, Name, Arity, Defaults),
      Check andalso check_valid_clause(Line, Filename, Name, Arity, Table);
    [] ->
      FinalDefaults = Defaults,
      FinalClauses  = Clauses,
      Check andalso ets:insert(Table, { last, { Name, Arity } })
  end,
  ets:insert(Table, {{Name, Arity}, Line, Kind, FinalDefaults, FinalClauses}).

%% Validations

check_valid_kind(_Line, _Filename, _Name, _Arity, Kind, Kind) -> [];
check_valid_kind(Line, Filename, Name, Arity, Kind, StoredKind) ->
  elixir_errors:form_error(Line, Filename, ?MODULE, {changed_kind, {Name, Arity, StoredKind, Kind}}).

check_valid_clause(Line, Filename, Name, Arity, Table) ->
  case ets:lookup_element(Table, last, 2) of
    {Name,Arity} -> [];
    [] -> [];
    {ElseName, ElseArity} -> elixir_errors:form_error(Line, Filename, ?MODULE,
      { changed_clause, { {Name, Arity}, {ElseName, ElseArity} } })
  end.

check_valid_defaults(_Line, _Filename, _Name, _Arity, 0) -> [];
check_valid_defaults(Line, Filename, Name, Arity, _) ->
  elixir_errors:handle_file_warning(Filename, { Line, ?MODULE, { clauses_with_docs, { Name, Arity } } }).

%% Format errors

format_error({clauses_with_docs,{Name,Arity}}) ->
  io_lib:format("function ~s/~B has default values and multiple clauses, it is recommended to use a separate clause for declaring defalts", [Name, Arity]);

format_error({private_doc,{Name,Arity}}) ->
  io_lib:format("function ~s/~B is private, @doc's are always discarded for private functions", [Name, Arity]);

format_error({existing_doc,{Name,Arity}}) ->
  io_lib:format("@doc's for function ~s/~B have been given more than once, the first version is being kept", [Name, Arity]);

format_error({changed_clause,{{Name,Arity},{ElseName,ElseArity}}}) ->
  io_lib:format("function ~s/~B does not match previous clause ~s/~B", [Name, Arity, ElseName, ElseArity]);

format_error({changed_kind,{Name,Arity,Previous,Current}}) ->
  io_lib:format("~s ~s/~B already defined as ~s", [Current, Name, Arity, Previous]).