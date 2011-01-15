-module(elixir_module).
-export([build/1, scope_for/2, transform/3, compile/4, wrap_method_definition/3, store_wrapped_method/3]).
-include("elixir.hrl").

% Create the scope for the given module.
%
% The scope is given by a set of two integers representing
% the ETS tables used to keep the method definitions.
scope_for(PreviousScope, Name) ->
  NewName = scoped_name(PreviousScope, Name),
  Options = [ordered_set, private],
  CompiledTable = ets:new(?ELIXIR_ATOM_CONCAT([aex, NewName]), Options),
  AddedTable = ets:new(?ELIXIR_ATOM_CONCAT([cex, NewName]), Options),
  {NewName, CompiledTable, AddedTable}.

% Generates module transform. It wraps the module definition into
% a function that is invoked with self as argument. We need to wrap
% them into anonymous functions so nested module definitions have
% the variable self shadowed.
transform(Line, Scope, Body) ->
  { Name, CompiledTable, AddedTable } = Scope,

  % This is the hook called at the end of the function
  % that will actually compiled the module.
  Hook = ?ELIXIR_WRAP_CALL(Line, elixir_module, compile,
    [{ var, Line, self }, {integer, Line, Line}, { integer, Line, CompiledTable }, { integer, Line, AddedTable}]
  ),

  % The argument for the module invocation will be the
  % result of elixir_module:build
  Args = ?ELIXIR_WRAP_CALL(Line, elixir_module, build, [{ atom, Line, Name }]),

  % Wraps the module definition into an anonymous function and
  % return an abstract form that will invoke it.
  Clause = { clause, Line, [{var, Line, self}], [], lists:append(Body, [Hook]) },
  Fun = { 'fun', Line, { clauses, [Clause] } },
  { call, Line, Fun, [Args] }.

% Build an object for this module that will be passed as self
% to the module function invocation.
build(Name) ->
  Object = #elixir_object{name=Name, parent='Module'},
  elixir_constants:store(Name, Object), % TODO We don't need to store this
  Object.

% Returns the new module name based on the previous scope.
scoped_name([], Name) -> Name;
scoped_name(Scope, Name) -> ?ELIXIR_ATOM_CONCAT([element(1, Scope), '::', Name]).

% Compile the given module by executing its body and compiling
% the result into binary and loading it into Erlang VM.
compile(Object, Line, CompiledTable, AddedTable) ->
  Name = Object#elixir_object.name,
  compile_module(build_module_form(Line, Name, AddedTable)),
  ets:delete(CompiledTable),
  ets:delete(AddedTable).

% Store a method in the compiled table. Returns an Erlang Abstract Form
% that, when executed, calls elixir_module:add_to_module which will
% retrieve the function from CompiledTable and move it to the AddedTable.
%
% All functions in the AddedTable are then copied to the module. We need
% to do it in two steps to ensure functions inside if branches won't
% propagate, for example:
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
wrap_method_definition(Scope, Line, Method) ->
  { _, CompiledTable, AddedTable } = Scope,
  Index = append_to_table(CompiledTable, Method),
  Content = [{integer, Line, Index}, {integer, Line, CompiledTable}, {integer, Line, AddedTable}],
  ?ELIXIR_WRAP_CALL(Line, elixir_module, store_wrapped_method, Content).

% Gets a module stored in the CompiledTable with Index and
% move it to the AddedTable.
store_wrapped_method(Index, CompiledTable, AddedTable) ->
  [{Index, Function}] = ets:lookup(CompiledTable, Index),
  append_to_table(AddedTable, Function).

% Gets all the functions in the AddedTable and generate Erlang
% Abstract Form that defines these modules.
build_module_form(Line, Name, Table) ->
  Pairs = ets:tab2list(Table),
  Functions = [element(2, Pair) || Pair <- Pairs],
  [{attribute, Line, module, Name}, {attribute, Line, compile, [export_all]} | Functions].

% Compile and load module.
% TODO Check warnings?
compile_module(Forms) ->
  case compile:forms(Forms) of
    {ok,ModuleName,Binary}           -> code:load_binary(ModuleName, "nofile", Binary);
    {ok,ModuleName,Binary,_Warnings} -> code:load_binary(ModuleName, "nofile", Binary)
  end.

% Append object to the table using last Index + 1.
append_to_table(Table, Set) ->
  Last = ets:last(Table),
  Index = next_table_index(Table),
  true = ets:insert(Table, {Index, Set}),
  Index.

% Get next table index.
next_table_index('$end_of_table') -> 1;
next_table_index(I) -> I + 1.