-module(elixir_module).
-export([scope_for/2, transform/3, compile/3, wrap_method_definition/3, store_wrapped_method/2]).
-include("elixir.hrl").

% Returns the new module name based on the previous scope.
scope_for([], Name) -> Name;
scope_for(Scope, Name) -> ?ELIXIR_ATOM_CONCAT([Scope, '::', Name]).

% Generates module transform. It wraps the module definition into
% a function that will be invoked by compile passing self as argument.
% We need to wrap them into anonymous functions so nested module
% definitions have the variable self shadowed.
transform(Line, Name, Body) ->
  Clause = { clause, Line, [{var, Line, self}], [], Body },
  Fun = { 'fun', Line, { clauses, [Clause] } },
  Args = [{integer, Line, Line}, {atom, Line, Name}, Fun],
  ?ELIXIR_WRAP_CALL(Line, elixir_module, compile, Args).

% Receive the module function to be invoked, invoke it passing
% self and then compile the added methods into an Erlang module
% and loaded it in the VM.
compile(Line, Name, Fun) ->
  Table = ?ELIXIR_ATOM_CONCAT([tex_, Name]),
  ets:new(Table, [bag, named_table, private]),

  try
    Object = #elixir_object{name=Name, parent='Module'},
    Result = Fun(Object),
    load_module(build_module_form(Line, Name, Table)),
    Result
  after
    ets:delete(Table)
  end.

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
wrap_method_definition(Name, Line, Method) ->
  Meta = erl_syntax:revert(erl_syntax:abstract(Method)),
  Content = [{atom, Line, Name}, Meta],
  ?ELIXIR_WRAP_CALL(Line, elixir_module, store_wrapped_method, Content).

% Gets a module stored in the CompiledTable with Index and
% move it to the AddedTable.
store_wrapped_method(Name, Method) ->
  TempTable = ?ELIXIR_ATOM_CONCAT([tex_, Name]),
  ets:insert(TempTable, Method).

% Gets all the functions in the AddedTable and generate Erlang
% Abstract Form that defines these modules.
build_module_form(Line, Name, Table) ->
  Functions = ets:tab2list(Table),
  [{attribute, Line, module, Name}, {attribute, Line, compile, [export_all]} | Functions].

% Compile and load module.
% TODO Check warnings?
load_module(Forms) ->
  case compile:forms(Forms) of
    {ok,ModuleName,Binary}           -> code:load_binary(ModuleName, "nofile", Binary);
    {ok,ModuleName,Binary,_Warnings} -> code:load_binary(ModuleName, "nofile", Binary)
  end.