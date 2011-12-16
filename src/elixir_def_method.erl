% Holds the logic responsible for methods definition during compile time.
% For methods introspection, check elixir_methods.
-module(elixir_def_method).
-export([unpack_default_clause/2, new_method_table/1,
  wrap_method_definition/4, store_wrapped_method/4, unwrap_stored_methods/1]).
-include("elixir.hrl").

% Creates a new method table for the given name.
new_method_table(Namespace) ->
  MethodTable = ?ELIXIR_ATOM_CONCAT([m, Namespace]),
  ets:new(MethodTable, [set, named_table, private]),
  ets:insert(MethodTable, { public, [] }),
  ets:insert(MethodTable, { private, [] }),
  ets:insert(MethodTable, { macros, [] }),
  ets:insert(MethodTable, { visibility, public }),
  MethodTable.

% Unpack default args from the given clause. Invoked by elixir_translate.
unpack_default_clause(Name, Clause) ->
  { NewArgs, NewClauses } = unpack_default_args(Name, element(3, Clause), [], []),
  { setelement(3, Clause, NewArgs), NewClauses }.

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
wrap_method_definition(Line, Filename, Method, Defaults) ->
  Meta = elixir_tree_helpers:abstract_syntax(Method),
  MetaDefaults = elixir_tree_helpers:abstract_syntax(Defaults),
  Content = [{var, Line, self}, {string, Line, Filename}, Meta, MetaDefaults],
  ?ELIXIR_WRAP_CALL(Line, ?MODULE, store_wrapped_method, Content).

% Invoked by the wrapped method with the method abstract tree.
% Each method is then added to the method table.
store_wrapped_method(Self, Filename, OriginalMethod, Defaults) ->
  Name        = Self#elixir_module__.name,
  MethodTable = ?ELIXIR_ATOM_CONCAT([m, Name]),
  ElixirName  = ?ELIXIR_EX_MODULE(Name),

  MethodName = case element(3, OriginalMethod) of
    []   -> ?ELIXIR_ATOM_CONCAT(["__anonymous_method_", ElixirName, "_", methods_count(MethodTable)+1]);
    Else -> Else
  end,
  Method = setelement(3, OriginalMethod, MethodName),

  Visibility = ets:lookup_element(MethodTable, visibility, 2),
  [store_each_method(MethodTable, Visibility, Filename, function_from_default(MethodName, Default)) || Default <- Defaults],
  store_each_method(MethodTable, Visibility, Filename, Method).

  % Returns a method object at the end.
  % try
  %   Arity = element(4, Method),
  %   Constant = elixir_constants:lookup('UnboundMethod::Behavior'),
  %   elixir_module_behavior:slate_bind(Constant, [Name, MethodName, Arity - 1])
  % catch
  %   error:{no_module,'UnboundMethod::Behavior'} -> []
  % end.

% Helper to unwrap the methods stored in the methods table. It also returns
% a list of methods to be exported with all methods.
unwrap_stored_methods(Namespace) ->
  Table     = ?ELIXIR_ATOM_CONCAT([m, Namespace]),
  Public    = ets:lookup_element(Table, public, 2),
  Private   = ets:lookup_element(Table, private, 2),
  Macros    = ets:lookup_element(Table, macros, 2),
  ets:delete(Table, visibility),
  ets:delete(Table, public),
  ets:delete(Table, private),
  ets:delete(Table, macros),
  { Public, Macros, ets:foldl(fun(X, Acc) -> unwrap_stored_method(X, Acc, Private) end, [], Table) }.

unwrap_stored_method({{Name, Arity}, Line, Clauses}, Acc, Private) ->
  [{function, Line, Name, Arity, Clauses}|Acc].

%% Helpers

methods_count(MethodTable) ->
  ets:info(MethodTable, size) - 4.

% Generates a function given a default clause.
function_from_default(Name, { clause, Line, Args, _Guards, _Exprs } = Clause) ->
  { function, Line, Name, length(Args), [Clause] }.

% Store each of the given method in the MethodTable.
store_each_method(MethodTable, Visibility, Filename, {function, Line, Name, Arity, Clauses}) ->
  FinalClauses = case ets:lookup(MethodTable, {Name, Arity}) of
    [{{Name, Arity}, FinalLine, OtherClauses}] ->
      check_valid_visibility(Line, Filename, Name, Arity, Visibility, MethodTable),
      Clauses ++ OtherClauses;
    [] ->
      add_visibility_entry(Name, Arity, Visibility, MethodTable),
      FinalLine = Line,
      Clauses
  end,
  ets:insert(MethodTable, {{Name, Arity}, FinalLine, FinalClauses}).

% Unpack default args from clauses
unpack_default_args(Name, [{default_arg, Line, Expr, Default}|T] = List, Acc, Clauses) ->
  { Args, Invoke } = build_default_arg(Acc, Line, [], []),
  Defaults = lists:map(fun extract_default/1, List),
  Clause = { clause, Line, Args, [], [
    { call, Line, {atom, Line, Name}, Invoke ++ Defaults }
  ]},
  unpack_default_args(Name, T, [Expr|Acc], [Clause|Clauses]);

unpack_default_args(Name, [H|T], Acc, Clauses) ->
  unpack_default_args(Name, T, [H|Acc], Clauses);

unpack_default_args(_Name, [], Acc, Clauses) ->
  { lists:reverse(Acc), lists:reverse(Clauses) }.

% Extract default values
extract_default({default_arg, Line, Expr, Default}) ->
  Default.

% Build an args list
build_default_arg([], _Line, Args, Invoke) -> { Args, Invoke };

build_default_arg([H|T], Line, Args, Invoke) ->
  Var = { var, Line, ?ELIXIR_ATOM_CONCAT(["X", length(T)]) },
  build_default_arg(T, Line, [{match, Line, Var, prune_vars(H)}|Args], [Var|Invoke]).

% Remove any reference to vars from the given form.
prune_vars({var, Line, _}) ->
  { var, Line, '_' };

prune_vars(H) when is_tuple(H) ->
  list_to_tuple(lists:map(fun prune_vars/1, tuple_to_list(H)));

prune_vars(H) when is_list(H) ->
  lists:map(fun prune_vars/1, H);

prune_vars(H) -> H.

% Check the visibility of the method with the given Name and Arity in the attributes table.
add_visibility_entry(Name, Arity, Visibility, Table) ->
  Current= ets:lookup_element(Table, Visibility, 2),
  ets:insert(Table, {Visibility, [{Name, Arity}|Current]}).

check_valid_visibility(Line, Filename, Name, Arity, Visibility, Table) ->
  Available = [public, private],
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

% Build an args list
build_arg(0, _Line, Args) -> Args;

build_arg(Counter, Line, Args) ->
  build_arg(Counter - 1, Line, [{ var, Line, ?ELIXIR_ATOM_CONCAT(["X", Counter]) }|Args]).