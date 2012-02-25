% Holds the logic responsible for functions definition (def, defp and defmacro).
-module(elixir_def).
-export([build_table/1,
  delete_table/1,
  reset_last/1,
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
  ets:insert(FunctionTable, { public, [] }),
  ets:insert(FunctionTable, { private, [] }),
  ets:insert(FunctionTable, { macros, [] }),
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
%%   ns Foo
%%
%%   if false do
%%     def bar: [], do: 1
%%   else:
%%     def bar: [], do: 2
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

handle_definition(Kind, Line, Module, Name, Args, _RawGuards, skip_definition, RawS) ->
  S = elixir_variables:deserialize_scope(RawS),
  Data = elixir_module:data(Module),
  elixir_module:data(Module, orddict:erase(abstract, Data)),
  compile_docs(Kind, Line, Module, Name, length(Args), S),
  { Name, length(Args) };

handle_definition(Kind, Line, Module, Name, Args, RawGuards, RawExpr, RawS) ->
  Data  = elixir_module:data(Module),
  Arity = length(Args),

  case orddict:find(abstract, Data) of
    { ok, true } ->
      elixir_def_abstract:store({ Name, Arity}, [Kind, Line, Module, Name, Args, RawGuards, RawExpr, RawS]),
      elixir_module:data(Module, orddict:erase(abstract, Data));
    _ ->
      S1 = elixir_variables:deserialize_scope(RawS),
      S2 = S1#elixir_scope{function={Name,Arity}, module=Module},
      store_definition(Kind, Line, Module, Name, Args, RawGuards, RawExpr, S2)
  end.

%% Store the definition after is is handled.

store_definition(Kind, Line, Module, Name, Args, RawGuards, RawExpr, S) ->
  Guards = elixir_clauses:extract_guard_clauses(RawGuards),

  case RawExpr of
    [{ do, Expr }] -> [];
    _ -> Expr = { 'try', Line, [RawExpr] }
  end,

  { Function, Defaults } = translate_definition(Line, Module, Name, Args, Guards, Expr, S),

  Filename      = S#elixir_scope.filename,
  Arity         = element(4, Function),
  FunctionTable = table(Module),

  %% Normalize visibility and kind
  { Final, Visibility } = case Kind of
    defmacro -> { defmacro, public };
    defp     -> { def, private };
    def      -> { def, public }
  end,

  CheckClauses = S#elixir_scope.check_clauses,

  %% Compile documentation
  compile_docs(Kind, Line, Module, Name, Arity, S),

  %% Store function
  store_each(CheckClauses, Final, FunctionTable, Visibility, Filename, Function),

  %% Store defaults
  [store_each(false, Final, FunctionTable, Visibility, Filename,
    function_for_clause(Name, Default)) || Default <- Defaults],

  { Name, Arity }.

%% Compile the documentation related to the module.

compile_docs(Kind, Line, Module, Name, Arity, S) ->
  case S#elixir_scope.compile#elixir_compile.internal of
    true -> [];
    _ ->
      case '::Module':compile_doc(Module, Line, Kind, { Name, Arity }) of
        warn -> elixir_errors:handle_file_warning(S#elixir_scope.filename,
          { Line, ?MODULE, { invalid_doc, { Name, Arity } } });
        _ -> []
      end
  end.

%% Translate the given call and expression given
%% and then store it in memory.

translate_definition(Line, Module, Name, Args, Guards, Expr, S) ->
  Arity = length(Args),
  { Unpacked, Defaults } = elixir_def_defaults:unpack(Name, Args, S),

  { TClause, _ } = elixir_clauses:assigns_block(Line,
    fun elixir_translator:translate/2, Unpacked, [Expr], Guards, S),

  Function = { function, Line, Name, Arity, [TClause] },
  { Function, Defaults }.

% Unwrap the functions stored in the functions table.
% It returns a list of all functions to be exported, plus the macros,
% and the body of all functions.
unwrap_stored_definitions(Module) ->
  Table     = table(Module),
  Public    = ets:lookup_element(Table, public, 2),
  Private   = ets:lookup_element(Table, private, 2),
  Macros    = ets:lookup_element(Table, macros, 2),
  ets:delete(Table, public),
  ets:delete(Table, private),
  ets:delete(Table, macros),
  ets:delete(Table, last),
  Functions = ets:foldl(fun(X, Acc) -> unwrap_stored_definition(X, Acc, Private) end, [], Table),
  { Public, Private, Macros, Functions }.

unwrap_stored_definition({{Name, Arity}, Line, Clauses}, Acc, _Private) ->
  [{function, Line, Name, Arity, lists:reverse(Clauses) }|Acc].

%% Helpers

%% Generates a function for the given clause.

function_for_clause(Name, { clause, Line, Args, _Guards, _Exprs } = Clause) ->
  { function, Line, Name, length(Args), [Clause] }.

%% Store each definition in the table.
%% This function also checks and emit warnings in case
%% the kind, of the visibility of the function changes.

store_each(Check, Kind, FunctionTable, Visibility, Filename, {function, Line, Name, Arity, Clauses}) ->
  FinalClauses = case ets:lookup(FunctionTable, {Name, Arity}) of
    [{{Name, Arity}, FinalLine, OtherClauses}] ->
      check_valid_visibility(Line, Filename, Name, Arity, Visibility, FunctionTable),
      check_valid_kind(Line, Filename, Name, Arity, Kind, FunctionTable),

      case Check of
        false -> [];
        true -> check_valid_clause(Line, Filename, Name, Arity, FunctionTable)
      end,

      Clauses ++ OtherClauses;
    [] ->
      add_visibility_entry(Name, Arity, Visibility, FunctionTable),
      add_function_entry(Name, Arity, Kind, FunctionTable),
      FinalLine = Line,
      Clauses
  end,
  ets:insert(FunctionTable, {{Name, Arity}, FinalLine, FinalClauses}).

%% Handle kind (def/defmacro) entries in the table

add_function_entry(Name, Arity, defmacro, Table) ->
  Tuple = { Name, Arity },
  Current= ets:lookup_element(Table, macros, 2),
  ets:insert(Table, {last, Tuple}),
  ets:insert(Table, {macros, [Tuple|Current]});

add_function_entry(Name, Arity, def, Table) ->
  ets:insert(Table, {last, {Name,Arity}}).

check_valid_kind(Line, Filename, Name, Arity, Kind, Table) ->
  List = ets:lookup_element(Table, macros, 2),

  Previous = case lists:member({Name, Arity}, List) of
    true -> defmacro;
    false -> def
  end,

  case Kind == Previous of
    false -> elixir_errors:form_error(Line, Filename, ?MODULE, {changed_kind, {Name, Arity, Previous, Kind}});
    true -> []
  end.

%% Handle visibility (public/private) entries in the table

add_visibility_entry(Name, Arity, Visibility, Table) ->
  Current= ets:lookup_element(Table, Visibility, 2),
  ets:insert(Table, {Visibility, [{Name, Arity}|Current]}).

check_valid_visibility(Line, Filename, Name, Arity, Visibility, Table) ->
  Available = [public, private],
  Previous = find_visibility(Name, Arity, Available, Table),
  case Visibility == Previous of
    false -> elixir_errors:form_error(Line, Filename, ?MODULE, {changed_visibility, {Name, Arity, Previous}});
    true -> []
  end.

find_visibility(_Name, _Arity, [H|[]], _Table) ->
  H;

find_visibility(Name, Arity, [Visibility|T], Table) ->
  List = ets:lookup_element(Table, Visibility, 2),
  case lists:member({Name, Arity}, List) of
    true  -> Visibility;
    false -> find_visibility(Name, Arity, T, Table)
  end.

%% Handle clause order

check_valid_clause(Line, Filename, Name, Arity, Table) ->
  case ets:lookup_element(Table, last, 2) of
    {Name,Arity} -> [];
    [] -> [];
    {ElseName, ElseArity} -> elixir_errors:form_error(Line, Filename, ?MODULE,
      { changed_clause, { {Name, Arity}, {ElseName, ElseArity} } })
  end.

%% Format errors

format_error({invalid_doc,{Name,Arity}}) ->
  io_lib:format("function ~s/~B is private. @doc's are always discarded for private functions", [Name, Arity]);

format_error({changed_clause,{{Name,Arity},{ElseName,ElseArity}}}) ->
  io_lib:format("function ~s/~B does not match previous clause ~s/~B", [Name, Arity, ElseName, ElseArity]);

format_error({changed_visibility,{Name,Arity,Previous}}) ->
  io_lib:format("function ~s/~B already defined with visibility ~s", [Name, Arity, Previous]);

format_error({changed_kind,{Name,Arity,Previous,Current}}) ->
  io_lib:format("~s ~s/~B already defined as ~s", [Current, Name, Arity, Previous]).