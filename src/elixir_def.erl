% Holds the logic responsible for functions definition during compile time.
-module(elixir_def).
-export([set_visibility/2,
  build_table/1,
  delete_table/1,
  wrap_definition/6,
  store_definition/5,
  unwrap_stored_definitions/1,
  format_error/1]).
-include("elixir.hrl").

%% Set the visibility entry for the given module.
%% Called from Elixir.

set_visibility(Module, Visibility) when is_atom(Module) ->
  ets:insert(table(Module), { visibility, Visibility }).

%% Table management functions. Called internally.

table(Module) -> ?ELIXIR_ATOM_CONCAT([f, Module]).

build_table(Module) ->
  FunctionTable = table(Module),
  ets:new(FunctionTable, [set, named_table, private]),
  ets:insert(FunctionTable, { public, [] }),
  ets:insert(FunctionTable, { private, [] }),
  ets:insert(FunctionTable, { macros, [] }),
  ets:insert(FunctionTable, { visibility, public }),
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
wrap_definition(Kind, Line, Filename, Module, Function, Defaults) ->
  Meta = elixir_tree_helpers:abstract_syntax(Function),
  MetaDefaults = elixir_tree_helpers:abstract_syntax(Defaults),
  Content = [{atom, Line, Kind}, {string, Line, Filename}, {atom, Line, Module}, Meta, MetaDefaults],
  ?ELIXIR_WRAP_CALL(Line, ?MODULE, store_definition, Content).

% Invoked by the wrap definition with the function abstract tree.
% Each function is then added to the function table.
store_definition(Kind, Filename, Module, Function, Defaults) ->
  FunctionTable = table(Module),
  FunctionName  = element(3, Function),

  Visibility = ets:lookup_element(FunctionTable, visibility, 2),
  [store_each(Kind, FunctionTable, Visibility, Filename, function_for_clause(FunctionName, Default)) || Default <- Defaults],
  store_each(Kind, FunctionTable, Visibility, Filename, Function),
  { FunctionName, element(4, Function) }.

% Unwrap the functions stored in the functions table.
% It returns a list of all functions to be exported, plus the macros,
% and the body of all functions.
unwrap_stored_definitions(Module) ->
  Table     = table(Module),
  Public    = ets:lookup_element(Table, public, 2),
  Private   = ets:lookup_element(Table, private, 2),
  Macros    = ets:lookup_element(Table, macros, 2),
  ets:delete(Table, visibility),
  ets:delete(Table, public),
  ets:delete(Table, private),
  ets:delete(Table, macros),
  Functions = ets:foldl(fun(X, Acc) -> unwrap_stored_definition(X, Acc, Private) end, [], Table),
  { Public, Macros, Functions }.

unwrap_stored_definition({{Name, Arity}, Line, Clauses}, Acc, _Private) ->
  [{function, Line, Name, Arity, lists:reverse(Clauses) }|Acc].

%% Helpers

%% Generates a function for the given clause.

function_for_clause(Name, { clause, Line, Args, _Guards, _Exprs } = Clause) ->
  { function, Line, Name, length(Args), [Clause] }.

%% Store each definition in the table.
%% This function also checks and emit warnings in case
%% the kind, of the visibility of the function changes.

store_each(Kind, FunctionTable, Visibility, Filename, {function, Line, Name, Arity, Clauses}) ->
  FinalClauses = case ets:lookup(FunctionTable, {Name, Arity}) of
    [{{Name, Arity}, FinalLine, OtherClauses}] ->
      % TODO: Raise a warning if storing the same name/arity but
      % the last clause was not the same name/arity
      check_valid_visibility(Line, Filename, Name, Arity, Visibility, FunctionTable),
      check_valid_kind(Line, Filename, Name, Arity, Kind, FunctionTable),
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
  Current= ets:lookup_element(Table, macros, 2),
  ets:insert(Table, {macros, [{Name, Arity}|Current]});

add_function_entry(_Name, _Arity, def, _Table) -> [].

check_valid_kind(Line, Filename, Name, Arity, Kind, Table) ->
  List = ets:lookup_element(Table, macros, 2),

  Previous = case lists:member({Name, Arity}, List) of
    true -> defmacro;
    false -> def
  end,

  case Kind == Previous of
    false -> elixir_errors:form_error(Line, Filename, ?MODULE, {changed_kind, {Name, Arity, Previous}});
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
    false -> elixir_errors:form(Line, Filename, ?MODULE, {changed_visibility, {Name, Arity, Previous}});
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

%% Format errors

format_error({changed_visibility,{Name,Arity,Previous}}) ->
  io_lib:format("function ~s/~B already defined with visibility ~s", [Name, Arity, Previous]);

format_error({changed_kind,{Name,Arity,Previous}}) ->
  io_lib:format("function ~s/~B already defined as ~s", [Name, Arity, Previous]).