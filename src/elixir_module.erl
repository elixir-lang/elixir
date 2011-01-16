-module(elixir_module).
-export([build_object/1, scope_for/2, transform/4, compile/4, wrap_method_definition/3, store_wrapped_method/2]).
-include("elixir.hrl").

% Build an object from the module name.
build_object(Name) ->
  Module = Name:module_info(attributes),
  Parent = proplists:get_value(parent, Module),
  Mixins = proplists:get_value(mixins, Module),
  Protos = proplists:get_value(protos, Module),
  #elixir_object{name=Name, parent=Parent, mixins=Mixins, protos=Protos}.

% Returns the new module name based on the previous scope.
scope_for([], Name) -> Name;
scope_for(Scope, Name) -> ?ELIXIR_ATOM_CONCAT([Scope, '::', Name]).

% Generates module transform. It wraps the module definition into
% a function that will be invoked by compile passing self as argument.
% We need to wrap them into anonymous functions so nested module
% definitions have the variable self shadowed.
transform(Kind, Line, Name, Body) ->
  Clause = { clause, Line, [{var, Line, self}], [], Body },
  Fun = { 'fun', Line, { clauses, [Clause] } },
  Args = [{atom, Line, Kind}, {integer, Line, Line}, {atom, Line, Name}, Fun],
  ?ELIXIR_WRAP_CALL(Line, elixir_module, compile, Args).

% Receive the module function to be invoked, invoke it passing
% self and then compile the added methods into an Erlang module
% and loaded it in the VM.
compile(Kind, Line, Name, Fun) ->
  MethodTable = ?ELIXIR_ATOM_CONCAT([mex_, Name]),
  AttributeTable = ?ELIXIR_ATOM_CONCAT([aex_, Name]),
  ets:new(MethodTable, [bag, named_table, private]),
  ets:new(AttributeTable, [set, named_table, private]),

  Parent = parent(Kind),
  Mixins = default_mixins(Name, Kind),
  Protos = default_protos(Name, Kind),
  ets:insert(AttributeTable, { mixins, Mixins }),
  ets:insert(AttributeTable, { parent, Parent }),
  ets:insert(AttributeTable, { protos, Protos }),

  try
    Object = #elixir_object{name=Name, parent=Parent, mixins=tweak_mixins(Name, Mixins), data=AttributeTable},
    Result = Fun(Object),
    load_module(build_module_form(Line, Object, MethodTable)),
    Result
  after
    ets:delete(MethodTable),
    ets:delete(AttributeTable)
  end.

% Returns the parent object based on the declaration.
parent(object) -> 'Object';
parent(module) -> 'Module'.

% Default mixins based on the declaration type.
default_mixins('Object', _)  -> [];
default_mixins('Module', _)  -> ['Module', 'Object::Methods'];
default_mixins(Name, module) -> [Name, 'Module', 'Object::Methods'];
default_mixins(Name, object) -> ['Object::Methods'].

% Default prototypes.
default_protos('Object', _)  -> [];
default_protos(Name, module) -> [];
default_protos(Name, object) -> ['Object::Methods'].

% Special case Object to include Bootstrap methods.
tweak_mixins('Object', _)  -> ['elixir_object_methods'];
tweak_mixins(Else, Mixins) -> Mixins.

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
  TempTable = ?ELIXIR_ATOM_CONCAT([mex_, Name]),
  ets:insert(TempTable, Method).

% Gets all the functions in the AddedTable and generate Erlang
% Abstract Form that defines these modules.
build_module_form(Line, Object, Table) ->
  Name = Object#elixir_object.name,
  Attrs = ets:tab2list(Object#elixir_object.data),
  Functions = ets:tab2list(Table),
  Transform = fun(X, Acc) -> [{attribute, Line, element(1, X), element(2, X)}|Acc] end,
  Base = lists:foldr(Transform, Functions, Attrs),
  [{attribute, Line, module, Name}, {attribute, Line, compile, [export_all]} | Base].

% Compile and load module.
% TODO Check warnings?
load_module(Forms) ->
  case compile:forms(Forms) of
    {ok,ModuleName,Binary}           -> code:load_binary(ModuleName, "nofile", Binary);
    {ok,ModuleName,Binary,_Warnings} -> code:load_binary(ModuleName, "nofile", Binary)
  end.