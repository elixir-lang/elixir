% Holds the logic responsible for functions definition (def(p) and defmacro(p)).
-module(elixir_def).
-export([table/1,
  build_table/1,
  delete_table/1,
  wrap_definition/7,
  store_definition/8,
  store_each/6,
  unwrap_stored_definitions/1,
  format_error/1]).
-include("elixir.hrl").

%% Table management functions. Called internally.

table(Module) -> ?ELIXIR_ATOM_CONCAT([f, Module]).

build_table(Module) ->
  FunctionTable = table(Module),
  ets:new(FunctionTable, [ordered_set, named_table, public]),
  FunctionTable.

delete_table(Module) ->
  ets:delete(table(Module)).

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

  ?ELIXIR_WRAP_CALL(Line, ?MODULE, store_definition, Invoke).

% Invoked by the wrap definition with the function abstract tree.
% Each function is then added to the function table.

store_definition(Kind, Line, nil, _Name, _Args, _Guards, _Expr, RawS) ->
  S = elixir_variables:deserialize_scope(RawS),
  elixir_errors:syntax_error(Line, S#elixir_scope.filename, "cannot define function outside module, invalid scope for ~s", [Kind]);

store_definition(Kind, Line, Module, Name, Args, Guards, RawExpr, RawS) ->
  Arity = length(Args),

  DS = elixir_variables:deserialize_scope(RawS),
  S = DS#elixir_scope{function={Name,Arity}, module=Module},

  case RawExpr of
    skip_definition -> Expr = nil;
    [{ do, Expr }] -> [];
    _ -> Expr = { 'try', Line, [RawExpr] }
  end,

  { Function, Defaults, TS } = translate_definition(Kind, Line, Name, Args, Guards, Expr, S),

  Filename      = TS#elixir_scope.filename,
  FunctionTable = table(Module),

  %% Compile documentation
  compile_docs(Kind, Line, Module, Name, Arity, TS),

  %% Store function
  case RawExpr of
    skip_definition -> [];
    _ ->
      compile_super(Module, TS),
      CheckClauses = S#elixir_scope.check_clauses,
      store_each(CheckClauses, Kind, Filename, FunctionTable, length(Defaults), Function)
  end,

  %% Store defaults
  [store_each(false, Kind, Filename, FunctionTable, 0,
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

translate_definition(Kind, Line, Name, Args, Guards, Expr, S) ->
  Arity = length(Args),
  { Unpacked, Defaults } = elixir_def_defaults:unpack(Kind, Name, Args, S),

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
  unwrap_stored_definition(ets:tab2list(Table), [], [], [], [], [], []).

unwrap_stored_definition([Fun|T], Exports, Private, Def, Defmacro, Defmacrop, Functions) when element(4, Fun) == def ->
  Tuple = element(1, Fun),
  unwrap_stored_definition(
    T, [Tuple|Exports], Private, [Tuple|Def], Defmacro, Defmacrop,
    [function_for_stored_definition(Fun)|Functions]
  );

unwrap_stored_definition([Fun|T], Exports, Private, Def, Defmacro, Defmacrop, Functions) when element(4, Fun) == defmacro ->
  Tuple = element(1, Fun),
  Macro = { ?ELIXIR_MACRO(element(1, Tuple)), element(2, Tuple) },

  unwrap_stored_definition(
    T, [Macro|Exports], Private, Def, [Tuple|Defmacro], Defmacrop,
    [function_for_stored_definition(setelement(1, Fun, Macro))|Functions]
  );

unwrap_stored_definition([Fun|T], Exports, Private, Def, Defmacro, Defmacrop, Functions) when element(4, Fun) == defp ->
  unwrap_stored_definition(
    T, Exports, [element(1, Fun)|Private], Def, Defmacro, Defmacrop,
    [function_for_stored_definition(Fun)|Functions]
  );

unwrap_stored_definition([Fun|T], Exports, Private, Def, Defmacro, Defmacrop, Functions) when element(4, Fun) == defmacrop ->
  unwrap_stored_definition(
    T, Exports, [element(1, Fun)|Private], Def, Defmacro,
    [{ element(1, Fun), element(2, Fun) }|Defmacrop], Functions
  );

unwrap_stored_definition([], Exports, Private, Def, Defmacro, Defmacrop, Functions) ->
  { Exports, Private, Def, Defmacro, Defmacrop, lists:reverse(Functions) }.

%% Helpers

function_for_stored_definition({{Name, Arity}, Line, _, _, _, Clauses}) ->
  {function, Line, Name, Arity, lists:reverse(Clauses) }.

function_for_clause(Name, { clause, Line, Args, _Guards, _Exprs } = Clause) ->
  { function, Line, Name, length(Args), [Clause] }.

%% Store each definition in the table.
%% This function also checks and emit warnings in case
%% the kind, of the visibility of the function changes.

store_each(Check, Kind, Filename, Table, Defaults, {function, Line, Name, Arity, Clauses}) ->
  case ets:lookup(Table, {Name, Arity}) of
    [{{Name, Arity}, _, _, StoredKind, StoredDefaults, StoredClauses}] ->
      FinalDefaults = Defaults + StoredDefaults,
      FinalClauses  = Clauses ++ StoredClauses,
      check_valid_kind(Line, Filename, Name, Arity, Kind, StoredKind),
      check_valid_defaults(Line, Filename, Name, Arity, Defaults),
      Check;
    [] ->
      FinalDefaults = Defaults,
      FinalClauses  = Clauses,
      Check
  end,
  ets:insert(Table, {{Name, Arity}, Line, Filename, Kind, FinalDefaults, FinalClauses}).

%% Validations

check_valid_kind(_Line, _Filename, _Name, _Arity, Kind, Kind) -> [];
check_valid_kind(Line, Filename, Name, Arity, Kind, StoredKind) ->
  elixir_errors:form_error(Line, Filename, ?MODULE, {changed_kind, {Name, Arity, StoredKind, Kind}}).

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

format_error({changed_kind,{Name,Arity,Previous,Current}}) ->
  io_lib:format("~s ~s/~B already defined as ~s", [Current, Name, Arity, Previous]).
