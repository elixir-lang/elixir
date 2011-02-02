-module(elixir_object).
-export([build/1, scope_for/2, transform/5, compile/6, wrap_method_definition/4, store_wrapped_method/3]).
-include("elixir.hrl").

%% EXTERNAL API

% Build an object from the given name. Used by elixir_constants to
% build objects from their compiled modules. Assumes the given name
% exists.
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
% TODO Copy data from parent
build_template(Name, BaseParent) ->
  Parent = default_parent(Name, BaseParent),
  Mixins = default_mixins(Name, Parent),
  Protos = default_protos(Name, Parent),
  Data   = default_data(Parent),

  AttributeTable = ?ELIXIR_ATOM_CONCAT([aex_, Name]),
  ets:new(AttributeTable, [set, named_table, private]),

  ets:insert(AttributeTable, { parent, Parent }),
  ets:insert(AttributeTable, { mixins, Mixins }),
  ets:insert(AttributeTable, { protos, Protos }),
  ets:insert(AttributeTable, { data, dict:new() }),
  ets:insert(AttributeTable, { visibility, public }),

  Object = #elixir_object{name=Name, parent=Parent, mixins=Mixins, protos=Protos, data=AttributeTable},
  { Object, AttributeTable }.

% Returns the parent object based on the declaration.
default_parent('Object', _)    -> [];
default_parent(Name, 'Object') -> 'Object'; % No need check if Object really exists.
default_parent(Name, 'Module') -> 'Module'; % No need check if Module really exists.
default_parent(Name, Parent) ->
  % abstract_parent will check if the constant was defined and then retrieve
  % its parent so we can assert we are not inheriting from a Module
  case elixir_object_methods:abstract_parent(Parent) of
    'Module' -> elixir_errors:raise(badarg, "cannot inherit from module ~s", [Parent]);
    _ -> Parent
  end.

% Default mixins based on the declaration type.
default_mixins(Name, [])       -> ['Object::Methods']; % object Object
default_mixins(Name, 'Object') -> [];                  % object Post
default_mixins(Name, 'Module') -> [Name];              % module Numeric
default_mixins(Name, Parent)   -> [{object, Parent}].  % object SimplePost < Post

% Default prototypes. Modules have themselves as the default prototype.
default_protos(Name, [])       -> ['Object::Methods'];
default_protos(Name, 'Module') -> [Name];
default_protos(Name, Parent)   -> [].

% Returns the default data from parents.
default_data([])       -> dict:new();
default_data('Object') -> dict:new();
default_data('Module') -> dict:new();
default_data(Parent)   -> elixir_object_methods:abstract_data(Parent).

%% USED ON TRANSFORMATION AND MODULE COMPILATION

% Returns the new module name based on the previous scope.
scope_for([], Name) -> Name;
scope_for(Scope, Name) -> ?ELIXIR_ATOM_CONCAT([Scope, '::', Name]).

% Generates module transform. It wraps the module definition into
% a function that will be invoked by compile/5 passing self as argument.
% We need to wrap them into anonymous functions so nested module
% definitions have the variable self shadowed.
transform(Line, Filename, Name, Parent, Body) ->
  Clause = { clause, Line, [{var, Line, self}], [], Body },
  Fun = { 'fun', Line, { clauses, [Clause] } },
  Args = [{integer, Line, Line}, {string, Line, Filename}, {var, Line, self},
    {atom, Line, Name}, {atom, Line, Parent}, Fun],
  ?ELIXIR_WRAP_CALL(Line, ?MODULE, compile, Args).

% Initial step of template compilation. Generate a method
% table and pass it forward to the next compile method.
compile(Line, Filename, Current, Name, Parent, Fun) ->
  MethodTable = ?ELIXIR_ATOM_CONCAT([mex_, Name]),
  ets:new(MethodTable, [set, named_table, private]),

  try
    compile(Line, Filename, Current, Name, Parent, Fun, MethodTable)
  after
    ets:delete(MethodTable)
  end.

% Receive the module function to be invoked, invoke it passing
% self and then compile the added methods into an Erlang module
% and loaded it in the VM.
compile(Line, Filename, Current, Name, Parent, Fun, MethodTable) ->
  { Object, AttributeTable } = build_template(Name, Parent),

  try
    Result = case Fun of
      [] -> [];
      _  -> Fun(Object)
    end,
    compile_kind(Parent, Line, Filename, Current, Object, MethodTable),
    Result
  after
    ets:delete(AttributeTable)
  end.

% Handle compilation logic specific to objects or modules.
% TODO Allow object reopening but without method definition.
% TODO Do not allow module reopening.
compile_kind('Module', Line, Filename, Current, Object, MethodTable) ->
  Name = Object#elixir_object.name,
  Functions = unwrap_stored_methods(MethodTable),
  load_form(build_erlang_form(Line, Object, Functions), Filename),
  add_implicit_mixins(Current, Name);

% Compile an object. If methods were defined in the object scope,
% we create a Proto module and automatically include it.
compile_kind(Parent, Line, Filename, Current, Object, MethodTable) ->
  case ets:first(MethodTable) of
    '$end_of_table' -> [];
    _ ->
      Name = ?ELIXIR_ATOM_CONCAT([Object#elixir_object.name, '::', 'Proto']),
      compile(Line, Filename, Object, Name, 'Module', [], MethodTable)
  end,
  load_form(build_erlang_form(Line, Object), Filename).

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
wrap_method_definition(Name, Line, Filename, Method) ->
  Meta = erl_syntax:revert(erl_syntax:abstract(Method)),
  Content = [{atom, Line, Name}, {string, Line, Filename}, Meta],
  ?ELIXIR_WRAP_CALL(Line, ?MODULE, store_wrapped_method, Content).

% Gets a module stored in the CompiledTable with Index and
% move it to the AddedTable.
store_wrapped_method(Module, Filename, {function, Line, Name, Arity, Clauses}) ->
  AttributeTable = ?ELIXIR_ATOM_CONCAT([aex_, Module]),
  MethodTable = ?ELIXIR_ATOM_CONCAT([mex_, Module]),
  [{_, Visibility}] = ets:lookup(AttributeTable, visibility),

  FinalClauses = case ets:lookup(MethodTable, {Name, Arity}) of
    [{{Name, Arity}, PrevVisibility, FinalLine, OtherClauses}] ->
      case Visibility == PrevVisibility of
        false ->
          Message = io_lib:format("method ~s already defined with visibility ~s", [Name, PrevVisibility]),
          io:format(elixir_errors:file_format(Line, Filename, Message) ++ [$\n]);
        true -> []
      end,
      [hd(Clauses)|OtherClauses];
    [] -> FinalLine = Line, PrevVisibility = Visibility, Clauses
  end,
  ets:insert(MethodTable, {{Name, Arity}, PrevVisibility, FinalLine, FinalClauses}).

unwrap_stored_methods(MethodTable) ->
  ets:foldl(fun unwrap_stored_methods/2, [], MethodTable).

unwrap_stored_methods({{Name, Arity}, Visibility, Line, Clauses}, Acc) ->
  [{function, Line, Name, Arity, lists:reverse(Clauses)}|Acc].

% Retrieve all attributes in the attribute table and generate
% an Erlang Abstract Form that defines an Erlang module.
build_erlang_form(Line, Object) ->
  build_erlang_form(Line, Object, []).

build_erlang_form(Line, Object, Functions) ->
  Name = Object#elixir_object.name,
  AttrTable = Object#elixir_object.data,
  ets:delete(AttrTable, visibility), % Do not convert visibility to a final attribute
  Attrs = ets:tab2list(AttrTable),
  Transform = fun(X, Acc) -> [{attribute, Line, element(1, X), element(2, X)}|Acc] end,
  Base = lists:foldr(Transform, Functions, Attrs),
  [{attribute, Line, module, Name}, {attribute, Line, compile, [export_all]} | Base].

% Compile and load given forms as an Erlang module.
load_form(Forms, Filename) ->
  case compile:forms(Forms, [return]) of
    {ok, ModuleName, Binary, Warnings} ->
      format_errors(false, Filename, Warnings),
      code:load_binary(ModuleName, Filename, Binary);
    {error, Errors, Warnings} ->
      format_errors(false, Filename, Warnings),
      format_errors(true, Filename, Errors)
  end.

format_errors(Raise, Filename, []) ->
  case Raise of
    true -> elixir_errors:raise(bad, "compilation failed but no reason was given");
    false -> []
  end;

format_errors(Bool, Filename, Errors) ->
  lists:foreach(fun ({_, Each}) -> format_error(Bool, Filename, Each) end, Errors).

% Overwritten warnings and errors.

format_error(false, Filename, [{Line,_,{unused_var,self}}|T]) ->
  format_error(false, Filename, T);

format_error(true, Filename, [{Line,Module,{undefined_function,{Name, Arity}}}|T]) ->
  Message = io_lib:format("undefined local method ~s/~w", [Name, Arity-1]),
  elixir_errors:file_error(undefined_local_method, Line, Filename, Message);

% Default warnings and errors.

format_error(false, Filename, [{Line,Module,Desc}|T]) ->
  Message = Module:format_error(Desc),
  io:format(elixir_errors:file_format(Line, Filename, Message) ++ [$\n]),
  format_error(false, Filename, T);

format_error(true, Filename, [{Line,Module,Desc}|T]) ->
  Message = Module:format_error(Desc),
  elixir_errors:file_error(element(1, Desc), Line, Filename, Message);

format_error(_, _, []) ->
  [].