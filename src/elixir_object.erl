-module(elixir_object).
-export([build/1, scope_for/2, transform/4, compile/5, wrap_method_definition/3, store_wrapped_method/2]).
-include("elixir.hrl").

%% EXTERNAL API

% Build an object from the module name. Used by elixir_constants to
% build objects from their compiled modules.
build(Name) ->
  Module = Name:module_info(attributes),
  Mixins = proplists:get_value(mixins, Module),
  Protos = proplists:get_value(protos, Module),
  Parent = case proplists:get_value(parent, Module) of
    []   -> [];
    Else -> hd(Else)
  end,
  #elixir_object{name=Name, parent=Parent, mixins=Mixins, protos=Protos}.

%% TEMPLATE BUILDING FOR MODULE COMPILATION

% Build a template of an object or module used on compilation.
% TODO Copy data from parent and ensure parent cannot be a mixin.
build_template(Name, BaseParent) ->
  AttributeTable = ?ELIXIR_ATOM_CONCAT([aex_, Name]),
  ets:new(AttributeTable, [set, named_table, private]),

  Parent = default_parent(Name, BaseParent),
  Mixins = default_mixins(Name, Parent),
  Protos = default_protos(Name, Parent),
  ets:insert(AttributeTable, { parent, Parent }),
  ets:insert(AttributeTable, { mixins, Mixins }),
  ets:insert(AttributeTable, { protos, Protos }),

  Object = #elixir_object{name=Name, parent=Parent, mixins=tweak_mixins(Name, Mixins), protos=Protos, data={def, AttributeTable}},
  { Object, AttributeTable }.

% Returns the parent object based on the declaration.
default_parent('Object', _)  -> [];
default_parent(Name, Parent) -> Parent.

% Default mixins based on the declaration type.
default_mixins(Name, [])       -> [];        % object Object
default_mixins(Name, 'Object') -> [];        % object Post
default_mixins(Name, 'Module') -> [Name];    % module Numeric
default_mixins(Name, Parent)   -> [Parent].  % object SimplePost < Post

% Default prototypes. Modules have themselves as the default prototype.
default_protos(Name, 'Module') -> [Name];
default_protos(Name, Parent)   -> [].

% Special case Object to include Bootstrap methods.
tweak_mixins('Object', _) -> ['elixir_object_methods'];
tweak_mixins(_, Mixins)   -> Mixins.

%% USED ON TRANSFORMATION AND MODULE COMPILATION

% Returns the new module name based on the previous scope.
scope_for([], Name) -> Name;
scope_for(Scope, Name) -> ?ELIXIR_ATOM_CONCAT([Scope, '::', Name]).

% Generates module transform. It wraps the module definition into
% a function that will be invoked by compile/5 passing self as argument.
% We need to wrap them into anonymous functions so nested module
% definitions have the variable self shadowed.
transform(Line, Name, Parent, Body) ->
  Clause = { clause, Line, [{var, Line, self}], [], Body },
  Fun = { 'fun', Line, { clauses, [Clause] } },
  Args = [{integer, Line, Line}, {var, Line, self}, {atom, Line, Name}, {atom, Line, Parent}, Fun],
  ?ELIXIR_WRAP_CALL(Line, ?MODULE, compile, Args).

% Initial step of template compilation. Generate a method
% table and pass it forward to the next compile method.
compile(Line, Current, Name, Parent, Fun) ->
  MethodTable = ?ELIXIR_ATOM_CONCAT([mex_, Name]),
  ets:new(MethodTable, [bag, named_table, private]),

  try
    compile(Line, Current, Name, Parent, Fun, MethodTable)
  after
    ets:delete(MethodTable)
  end.

% Receive the module function to be invoked, invoke it passing
% self and then compile the added methods into an Erlang module
% and loaded it in the VM.
compile(Line, Current, Name, Parent, Fun, MethodTable) ->
  { Object, AttributeTable } = build_template(Name, Parent),

  try
    Result = case Fun of
      [] -> [];
      _  -> Fun(Object)
    end,
    compile_kind(Parent, Line, Current, Object, MethodTable),
    Result
  after
    ets:delete(AttributeTable)
  end.

% Handle compilation logic specific to objects or modules.
% TODO Allow object reopening.
% TODO Do not allow module reopening.
compile_kind('Module', Line, Current, Object, MethodTable) ->
  Name = Object#elixir_object.name,
  Functions = ets:tab2list(MethodTable),
  load_form(build_erlang_form(Line, Object, Functions)),
  add_implicit_mixins(Current, Name);

compile_kind(Parent, Line, Current, Object, MethodTable) ->
  case ets:first(MethodTable) of
    '$end_of_table' -> [];
    _ ->
      Name = ?ELIXIR_ATOM_CONCAT([Object#elixir_object.name, '::', 'Proto']),
      compile(Line, Object, Name, 'Module', [], MethodTable)
  end,
  load_form(build_erlang_form(Line, Object)).

% Check if the module currently defined is inside an object
% definition an automatically include it.
add_implicit_mixins(#elixir_object{name=Name} = Self, ModuleName) ->
  Proto = lists:concat([Name, '::', 'Proto']),
  Mixin = lists:concat([Name, '::', 'Mixin']),
  case atom_to_list(ModuleName) of
    Proto -> elixir_object_methods:proto(Self, build(ModuleName));
    Mixin -> elixir_object_methods:mixin(Self, build(ModuleName));
    Else  -> []
  end;

add_implicit_mixins(_, _) -> [].

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
  ?ELIXIR_WRAP_CALL(Line, ?MODULE, store_wrapped_method, Content).

% Gets a module stored in the CompiledTable with Index and
% move it to the AddedTable.
store_wrapped_method(Name, Method) ->
  TempTable = ?ELIXIR_ATOM_CONCAT([mex_, Name]),
  ets:insert(TempTable, Method).

% Retrieve all attributes in the attribute table and generate
% an Erlang Abstract Form that defines an Erlang module.
build_erlang_form(Line, Object) ->
  build_erlang_form(Line, Object, []).

build_erlang_form(Line, Object, Functions) ->
  Name = Object#elixir_object.name,
  {def, AttrTable} = Object#elixir_object.data,
  Attrs = ets:tab2list(AttrTable),
  Transform = fun(X, Acc) -> [{attribute, Line, element(1, X), element(2, X)}|Acc] end,
  Base = lists:foldr(Transform, Functions, Attrs),
  [{attribute, Line, module, Name}, {attribute, Line, compile, [export_all]} | Base].

% Compile and load given forms as an Erlang module.
load_form(Forms) ->
  case compile:forms(Forms, [nowarn_unused_vars]) of
    {ok,ModuleName,Binary}           -> code:load_binary(ModuleName, "nofile", Binary);
    {ok,ModuleName,Binary,_Warnings} -> code:load_binary(ModuleName, "nofile", Binary)
  end.