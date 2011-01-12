-module(elixir_module).
-export([scope_for/1, compile/4, store_method/2, move_method/3]).

% Create the scope for the given module.
%
% The scope is given by a set of two integers representing
% the ETS tables used to keep the method definitions.
scope_for(Name) ->
  Options = [ordered_set, private],
  CompiledTable = ets:new(prepend_to_atom(c, Name), Options),
  AddedTable = ets:new(prepend_to_atom(a, Name), Options),
  {CompiledTable, AddedTable}.

% Compile the given module by executing its body and compiling
% the result into binary and loading it into Erlang VM.
compile(Line, Name, Body, Scope) ->
  {CompiledTable, AddedTable} = Scope,
  {value, Value, _} = erl_eval:exprs(Body, []),
  load_module(build_module(Line, Name, AddedTable)),
  ets:delete(CompiledTable),
  ets:delete(AddedTable).

% Converts a tuple of atoms into a concatenated atom with these tuples.
prepend_to_atom(Prefix, Atom) ->
  list_to_atom(lists:concat([Prefix, Atom])).

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
% before evaluating the method body), we see both definitions.
store_method(Scope, Method) ->
  { CompiledTable, AddedTable } = Scope,
  Index = append_to_table(CompiledTable, Method),
  Content = [{integer, 0, Index}, {integer, 0, CompiledTable}, {integer, 0, AddedTable}],
  wrap_into_call(elixir_module, move_method, Content).

% Gets a module stored in the CompiledTable with Index and
% move it to the AddedTable.
move_method(Index, CompiledTable, AddedTable) ->
  [{Index, Function}] = ets:lookup(CompiledTable, Index),
  append_to_table(AddedTable, Function).

% Gets all the functions in the AddedTable and generate Erlang
% Abstract Form that defines these modules.
build_module(Line, Name, Table) ->
  Pairs = ets:tab2list(Table),
  Functions = [element(2, Pair) || Pair <- Pairs],
  Export = [{element(3, Function), element(4, Function)} || Function <- Functions],
  [{attribute, Line, module, Name}, {attribute, Line, export, Export} | Functions].

% Compile and load module.
% TODO Check warnings?
load_module(Forms) ->
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

% Receive Module, Method, Args and generate Erlang Abstract Syntax
% to invoke Module:Method(Args).
wrap_into_call(Module, Method, Args) ->
  { call, 0,
    { remote, 0, { atom, 0, Module }, { atom, 0, Method} },
    Args
  }.