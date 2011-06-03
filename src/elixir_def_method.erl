% Holds the logic responsible for methods definition during compile time.
% For methods introspection, check elixir_methods.
-module(elixir_def_method).
-export([unpack_default_clause/2, is_empty_table/1, new_method_table/1, flat_module/5,
  wrap_method_definition/5, store_wrapped_method/5, unwrap_stored_methods/1]).
-include("elixir.hrl").

% Returns if a given table is empty or not.
%
% Since we use the same method table to store the current visibility,
% public and inherited method, the table is empty if its size is 3.
is_empty_table(MethodTable) ->
  case ets:info(MethodTable, size) of
    3 -> true;
    _ -> false
  end.

% Creates a new method table for the given name.
new_method_table(Name) ->
  MethodTable = ?ELIXIR_ATOM_CONCAT([mex_, Name]),
  ets:new(MethodTable, [set, named_table, private]),
  ets:insert(MethodTable, { public, [] }),
  ets:insert(MethodTable, { inherited, [] }),
  ets:insert(MethodTable, { visibility, public }),
  MethodTable.

% Unpack default args from the given clause. Invoked by elixir_transform.
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
wrap_method_definition(Name, Line, Filename, Method, Defaults) ->
  Meta = elixir_tree_helpers:abstract_syntax(Method),
  MetaDefaults = elixir_tree_helpers:abstract_syntax(Defaults),
  Content = [{var, Line, self}, {atom, Line, Name}, {string, Line, Filename}, Meta, MetaDefaults],
  ?ELIXIR_WRAP_CALL(Line, ?MODULE, store_wrapped_method, Content).

% Invoked by the wrapped method with the method abstract tree.
% Each method is then added to the method table.
store_wrapped_method(Self, Module, Filename, OriginalMethod, Defaults) ->
  MethodTable = ?ELIXIR_ATOM_CONCAT([mex_, Module]),
  Name = case element(3, OriginalMethod) of
    []   -> ?ELIXIR_ATOM_CONCAT(["__anonymous_method_", Module, "_", ets:info(MethodTable, size)]);
    Else -> Else
  end,
  Method = setelement(3, OriginalMethod, Name),

  Visibility = ets:lookup_element(MethodTable, visibility, 2),
  [store_each_method(MethodTable, Visibility, Filename, function_from_default(Name, Default)) || Default <- Defaults],
  store_each_method(MethodTable, Visibility, Filename, Method),

  % Returns a method object at the end.
  try
    Arity = element(4, Method),
    Constant = elixir_constants:lookup('UnboundMethod::Instance'),
    elixir_bind:slate_bind(Constant, [?ELIXIR_ERL_MODULE(Self#elixir_object__.name), Name, Arity - 1])
  catch
    error:{noconstant,'UnboundMethod::Instance'} -> []
  end.

% Helper to unwrap the methods stored in the methods table. It also returns
% a list of methods to be exported with all methods.
unwrap_stored_methods(Table) ->
  Public    = ets:lookup_element(Table, public, 2),
  Inherited = ets:lookup_element(Table, inherited, 2),
  ets:delete(Table, visibility),
  ets:delete(Table, public),
  ets:delete(Table, inherited),
  { Public, Inherited, ets:foldl(fun unwrap_stored_method/2, [], Table) }.

unwrap_stored_method({{Name, Arity}, Line, Clauses}, Acc) ->
  [{function, Line, Name, Arity, lists:reverse(Clauses)}|Acc].

% Receives a method table and adds the given What from Object in it.
% TODO Maybe this could be done by simply using the import directive?
flat_module(Object, Line, What, #elixir_object__{name=ModuleName}, MethodTable) ->
  SelfModules = elixir_object_methods:What(Object),
  RawModules = lists:delete(ModuleName, SelfModules),

  % Do not flat Module::Methods that are temporarily added.
  Modules = case Object#elixir_object__.parent of
    'Module' -> RawModules;
    Else     -> lists:delete('Module::Methods', RawModules)
  end,

  Visibility = lists:foldl(fun(Module, Acc1) ->
    DispatchTo = ?ELIXIR_ERL_MODULE(Module),
    lists:foldl(fun({Method, ElixirArity}, Acc2) ->
      Arity = ElixirArity + 1,
      case ets:lookup(MethodTable, {Method, Arity}) of
        [] ->
          BuiltArgs = build_arg(Arity, Line, []),

          ets:insert(MethodTable, {
            { Method, Arity }, Line, [
              { clause, Line, BuiltArgs, [], [?ELIXIR_WRAP_CALL(Line, DispatchTo, Method, BuiltArgs)] }
            ]
          }),

          [{Method,Arity}|Acc2];
        _ -> Acc2
      end
    end, Acc1, elixir_methods:owner_methods(Module))
  end, [], Modules),

  ets:insert(MethodTable, { inherited, Visibility }).

%% Helpers

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
add_visibility_entry(Name, Arity, private, Table) ->
  [];

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