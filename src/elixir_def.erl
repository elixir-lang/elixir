% Holds the logic responsible for methods definition during compile time.
% For methods introspection, check elixir_methods.
-module(elixir_def).
-export([new_method_table/1, set_visibility/2,
  wrap_method_definition/6, store_wrapped_method/5, unwrap_stored_methods/1, delete_table/1]).
-include("elixir.hrl").

% Visibility
set_visibility(Module, Visibility) when is_atom(Module) ->
  ets:insert(table_name(Module), { visibility, Visibility }).

delete_table(Module) ->
  ets:delete(table_name(Module)).

% Creates a new method table for the given name.
new_method_table(Module) ->
  MethodTable = table_name(Module),
  ets:new(MethodTable, [set, named_table, private]),
  ets:insert(MethodTable, { public, [] }),
  ets:insert(MethodTable, { private, [] }),
  ets:insert(MethodTable, { macros, [] }),
  ets:insert(MethodTable, { visibility, public }),
  MethodTable.

% Wraps the method into a call that will call store_wrapped_method
% once the method definition is read. The method is compiled into a
% meta tree to ensure we will receive the full method.
%
% We need to wrap methods instead of eagerly defining them to ensure
% functions inside if branches won't propagate, for example:
%
%   ns Foo
%
%   if false do
%     def bar: [], do: 1
%   else:
%     def bar: [], do: 2
%   end
%
% If we just analyzed the compiled structure (i.e. the method availables
% before evaluating the method body), we would see both definitions.
wrap_method_definition(Kind, Line, Filename, Module, Method, Defaults) ->
  Meta = elixir_tree_helpers:abstract_syntax(Method),
  MetaDefaults = elixir_tree_helpers:abstract_syntax(Defaults),
  Content = [{atom, Line, Kind}, {string, Line, Filename}, {atom, Line, Module}, Meta, MetaDefaults],
  ?ELIXIR_WRAP_CALL(Line, ?MODULE, store_wrapped_method, Content).

% Invoked by the wrapped method with the method abstract tree.
% Each method is then added to the method table.
store_wrapped_method(Kind, Filename, Module, Method, Defaults) ->
  MethodTable = table_name(Module),
  MethodName  = element(3, Method),

  Visibility = ets:lookup_element(MethodTable, visibility, 2),
  [store_each_method(Kind, MethodTable, Visibility, Filename, function_for_clause(MethodName, Default)) || Default <- Defaults],
  store_each_method(Kind, MethodTable, Visibility, Filename, Method),
  { MethodName, element(4, Method) }.

% Helper to unwrap the methods stored in the methods table. It also returns
% a list of methods to be exported with all methods.
unwrap_stored_methods(Module) ->
  Table     = table_name(Module),
  Public    = ets:lookup_element(Table, public, 2),
  Private   = ets:lookup_element(Table, private, 2),
  Macros    = ets:lookup_element(Table, macros, 2),
  ets:delete(Table, visibility),
  ets:delete(Table, public),
  ets:delete(Table, private),
  ets:delete(Table, macros),
  Functions = ets:foldl(fun(X, Acc) -> unwrap_stored_method(X, Acc, Private) end, [], Table),
  { Public, Macros, Functions }.

unwrap_stored_method({{Name, Arity}, Line, Clauses}, Acc, _Private) ->
  [{function, Line, Name, Arity, lists:reverse(Clauses) }|Acc].

%% Helpers

% Generates a function for the given clause.
function_for_clause(Name, { clause, Line, Args, _Guards, _Exprs } = Clause) ->
  { function, Line, Name, length(Args), [Clause] }.

% Store each of the given method in the MethodTable.
store_each_method(Kind, MethodTable, Visibility, Filename, {function, Line, Name, Arity, Clauses}) ->
  FinalClauses = case ets:lookup(MethodTable, {Name, Arity}) of
    [{{Name, Arity}, FinalLine, OtherClauses}] ->
      check_valid_visibility(Line, Filename, Name, Arity, Visibility, MethodTable),
      check_valid_kind(Line, Filename, Name, Arity, Kind, MethodTable),
      Clauses ++ OtherClauses;
    [] ->
      add_visibility_entry(Name, Arity, Visibility, MethodTable),
      add_function_entry(Name, Arity, Kind, MethodTable),
      FinalLine = Line,
      Clauses
  end,
  ets:insert(MethodTable, {{Name, Arity}, FinalLine, FinalClauses}).

% Add function entry
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
    false -> elixir_errors:handle_file_warning(Filename, {Line, ?MODULE, {changed_kind, {Name, Arity, Previous}}});
    true -> []
  end.

% Check the visibility of the method with the given Name and Arity in the attributes table.
add_visibility_entry(Name, Arity, Visibility, Table) ->
  Current= ets:lookup_element(Table, Visibility, 2),
  ets:insert(Table, {Visibility, [{Name, Arity}|Current]}).

check_valid_visibility(Line, Filename, Name, Arity, Visibility, Table) ->
  Available = [public, private],
  Previous = find_visibility(Name, Arity, Available, Table),
  case Visibility == Previous of
    false -> elixir_errors:handle_file_warning(Filename, {Line, ?MODULE, {changed_visibility, {Name, Arity, Previous}}});
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

table_name(Module) -> ?ELIXIR_ATOM_CONCAT([f, Module]).