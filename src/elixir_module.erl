-module(elixir_module).
-export([build/1, scope_for/2, transform/3, compile/2, wrap_method_definition/3, store_wrapped_method/2]).
-include("elixir.hrl").

% Returns the new module name based on the previous scope.
scope_for([], Name) -> Name;
scope_for(Scope, Name) -> ?ELIXIR_ATOM_CONCAT([Scope, '::', Name]).

% Generates module transform. It wraps the module definition into
% a function that is invoked with self as argument. We need to wrap
% them into anonymous functions so nested module definitions have
% the variable self shadowed.
transform(Line, Name, Body) ->
  % This is the hook called at the end of the function
  % that will actually compiled the module.
  Hook = ?ELIXIR_WRAP_CALL(Line, elixir_module, compile,
    [{ var, Line, self }, {integer, Line, Line}]
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
  Options = [bag, named_table, private],
  ets:new(?ELIXIR_ATOM_CONCAT([tex_, Name]), Options),
  #elixir_object{name=Name, parent='Module'}.

% Compile the given module by executing its body and compiling
% the result into binary and loading it into Erlang VM.
compile(Object, Line) ->
  Name = Object#elixir_object.name,
  TempTable = ?ELIXIR_ATOM_CONCAT([tex_, Name]),
  load_module(build_module_form(Line, Name, TempTable)),
  ets:delete(TempTable).

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