% Holds the logic responsible for methods definition during parse time.
% For modules introspection, check elixir_methods.
-module(elixir_def_method).
-export([is_empty_table/1, new_method_table/1, wrap_method_definition/4, store_wrapped_method/3, unwrap_stored_methods/1]).
-include("elixir.hrl").

% Returns if a given table is empty or not.
%
% Since we use the same method table to store the current visibility,
% public, protected and callbacks method, the table is empty if its
% size is 4.
is_empty_table(MethodTable) ->
  case ets:info(MethodTable, size) of
    4 -> true;
    _ -> false
  end.

% Creates a new method table for the given name.
new_method_table(Name) ->
  MethodTable = ?ELIXIR_ATOM_CONCAT([mex_, Name]),
  ets:new(MethodTable, [set, named_table, private]),
  ets:insert(MethodTable, { public, [] }),
  ets:insert(MethodTable, { protected, [] }),
  ets:insert(MethodTable, { callbacks, [] }),
  ets:insert(MethodTable, { visibility, public }),
  MethodTable.

% Wraps the method into a call that will call store_wrapped_method
% once the method definition is read. The method is compiled into a
% meta tree to ensure we will receive the full method.
%
% We need to wrap methods instead of eagerly defining them to ensure
% functions inside if branches won't propagate, for example:
%
%   module Foo
%     if false
%       def bar; 1; end
%     else
%       def bar; 2; end
%     end
%   end
%
% If we just analyzed the compiled structure (i.e. the method availables
% before evaluating the method body), we would see both definitions.
wrap_method_definition(Name, Line, Filename, Method) ->
  Meta = erl_syntax:revert(erl_syntax:abstract(Method)),
  Content = [{atom, Line, Name}, {string, Line, Filename}, Meta],
  ?ELIXIR_WRAP_CALL(Line, ?MODULE, store_wrapped_method, Content).

% Gets a module stored in the CompiledTable with Index and
% move it to the AddedTable.
store_wrapped_method(Module, Filename, {function, Line, Name, Arity, Clauses}) ->
  MethodTable = ?ELIXIR_ATOM_CONCAT([mex_, Module]),
  Visibility = ets:lookup_element(MethodTable, visibility, 2),

  FinalClauses = case ets:lookup(MethodTable, {Name, Arity}) of
    [{{Name, Arity}, FinalLine, OtherClauses}] ->
      check_valid_visibility(Line, Filename, Name, Arity, Visibility, MethodTable),
      [hd(Clauses)|OtherClauses];
    [] ->
      add_visibility_entry(Name, Arity, Visibility, MethodTable),
      FinalLine = Line,
      Clauses
  end,
  ets:insert(MethodTable, {{Name, Arity}, FinalLine, FinalClauses}).

% Helper to unwrap the methods stored in the methods table. It also returns
% a list of methods to be exported with all protected methods.
unwrap_stored_methods(Table) ->
  Public    = ets:lookup_element(Table, public, 2),
  Protected = ets:lookup_element(Table, protected, 2),
  Callbacks = ets:lookup_element(Table, callbacks, 2),
  ets:delete(Table, visibility),
  ets:delete(Table, public),
  ets:delete(Table, protected),
  ets:delete(Table, callbacks),
  AllProtected = Protected ++ Callbacks,
  { Callbacks, { Public ++ AllProtected, AllProtected, ets:foldl(fun unwrap_stored_method/2, [], Table) } }.

unwrap_stored_method({{Name, Arity}, Line, Clauses}, Acc) ->
  [{function, Line, Name, Arity, lists:reverse(Clauses)}|Acc].

% Check the visibility of the method with the given Name and Arity in the attributes table.
add_visibility_entry(Name, Arity, private, Table) ->
  [];

add_visibility_entry(Name, Arity, Visibility, Table) ->
  Current= ets:lookup_element(Table, Visibility, 2),
  ets:insert(Table, {Visibility, [{Name, Arity}|Current]}).

check_valid_visibility(Line, Filename, Name, Arity, Visibility, Table) ->
  Available = [public, protected, callbacks, private],
  PrevVisibility = find_visibility(Name, Arity, Available, Table),
  case Visibility == PrevVisibility of
    false -> elixir_errors:handle_file_warning(Filename, {Line, ?MODULE, {changed_visibility, {Name, PrevVisibility}}});
    true -> []
  end.

find_visibility(Name, Arity, [H|[]], Table) ->
  H;

find_visibility(Name, Arity, [Visibility|T], Table) ->
  List = ets:lookup_element(Table, Visibility, 2),
  case lists:member({Name, Arity}, List) of
    true  -> Visibility;
    false -> find_visibility(Name, Arity, T, Table)
  end.

