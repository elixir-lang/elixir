-module(elixir_object).
-export([build/1, scope_for/2, transform/6, compile/7]).
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
build_template(Kind, Name, Template) ->
  Parent = default_parent(Kind, Name),
  Mixins = default_mixins(Kind, Name, Template),
  Protos = default_protos(Kind, Name, Template),
  Data   = default_data(Template),

  AttributeTable = ?ELIXIR_ATOM_CONCAT([aex_, Name]),
  ets:new(AttributeTable, [set, named_table, private]),

  ets:insert(AttributeTable, { mixins, Mixins }),
  ets:insert(AttributeTable, { protos, Protos }),
  ets:insert(AttributeTable, { data,   Data }),

  Object = #elixir_object{name=Name, parent=Parent, mixins=[], protos=[], data=AttributeTable},
  { Object, AttributeTable }.

% Returns the parent object based on the declaration.
default_parent(_, 'Object')   -> [];
default_parent(object, _Name) -> 'Object';
default_parent(module, _Name) -> 'Module'.

% Default mixins based on the declaration type.
default_mixins(_, 'Object', _Template)  -> ['Object::Methods']; % object Object
default_mixins(_, 'Module', _Template)  -> ['Module::Methods']; % object Module
default_mixins(object, _Name, [])       -> ['Module::Methods']; % object Post
default_mixins(module, Name, _Template) -> [Name];              % module Numeric
default_mixins(object, _Name, Template) ->                      % object SimplePost from Post
  Template#elixir_object.mixins ++ ['Module::Methods'].

% Default prototypes. Modules have themselves as the default prototype.
default_protos(_, 'Object', _Template)  -> ['Object::Methods'];           % object Object
default_protos(_, 'Module', _Template)  -> ['Module::Methods'];           % object Module
default_protos(object, _Name, [])       -> [];                            % object Post
default_protos(module, Name, _Template) -> [Name];                        % module Numeric
default_protos(object, _Name, Template) -> Template#elixir_object.protos. % object SimplePost from Post

% Returns the default data from parents.
default_data([])       -> dict:new();
default_data(Template) -> Template#elixir_object.data.

%% USED ON TRANSFORMATION AND MODULE COMPILATION

% Returns the new module name based on the previous scope.
scope_for([], Name) -> Name;
scope_for(Scope, Name) -> ?ELIXIR_ATOM_CONCAT([Scope, '::', Name]).

% Generates module transform. It wraps the module definition into
% a function that will be invoked by compile/5 passing self as argument.
% We need to wrap them into anonymous functions so nested module
% definitions have the variable self shadowed.
transform(Kind, Line, Filename, Name, Parent, Body) ->
  Clause = { clause, Line, [{var, Line, self}], [], Body },
  Fun = { 'fun', Line, { clauses, [Clause] } },
  Args = [{atom, Line, Kind}, {integer, Line, Line}, {string, Line, Filename},
    {var, Line, self}, {atom, Line, Name}, {atom, Line, Parent}, Fun],
  ?ELIXIR_WRAP_CALL(Line, ?MODULE, compile, Args).

% Initial step of template compilation. Generate a method
% table and pass it forward to the next compile method.
compile(Kind, Line, Filename, Current, Name, Template, Fun) ->
  MethodTable = ?ELIXIR_ATOM_CONCAT([mex_, Name]),
  ets:new(MethodTable, [set, named_table, private]),

  ets:insert(MethodTable, { public, [] }),
  ets:insert(MethodTable, { protected, [] }),
  ets:insert(MethodTable, { visibility, public }),

  try
    compile(Kind, Line, Filename, Current, Name, Template, Fun, MethodTable)
  after
    ets:delete(MethodTable)
  end.

% Receive the module function to be invoked, invoke it passing
% self and then compile the added methods into an Erlang module
% and loaded it in the VM.
compile(Kind, Line, Filename, Current, Name, Template, Fun, MethodTable) ->
  { Object, AttributeTable } = build_template(Kind, Name, Template),

  try
    Result = Fun(Object),
    compile_kind(Kind, Line, Filename, Current, Object, MethodTable),
    Result
  after
    ets:delete(AttributeTable)
  end.

% Handle compilation logic specific to objects or modules.
% TODO Allow object reopening but without method definition.
% TODO Do not allow module reopening.
compile_kind(module, Line, Filename, Current, Object, MethodTable) ->
  Name = Object#elixir_object.name,
  Functions = elixir_methods:unwrap_stored_methods(MethodTable),
  load_form(build_erlang_form(Line, Object, Functions), Filename),
  add_implicit_mixins(Current, Name);

% Compile an object. If methods were defined in the object scope,
% we create a Proto module and automatically include it.
compile_kind(_Kind, Line, Filename, Current, Object, MethodTable) ->
  case ets:info(MethodTable, size) of
    3 -> [];
    _ ->
      Name = ?ELIXIR_ATOM_CONCAT([Object#elixir_object.name, '::', 'Proto']),
      compile(module, Line, Filename, Object, Name, [], fun(X) -> [] end, MethodTable)
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


% Retrieve all attributes in the attribute table and generate
% an Erlang Abstract Form that defines an Erlang module.
build_erlang_form(Line, Object) ->
  build_erlang_form(Line, Object, {[],[],[]}).

build_erlang_form(Line, Object, {Export, Protected, Functions}) ->
  Name = Object#elixir_object.name,
  Parent = Object#elixir_object.parent,
  AttrTable = Object#elixir_object.data,
  Transform = fun(X, Acc) -> [transform_attribute(Line, X)|Acc] end,
  Base = ets:foldr(Transform, Functions, AttrTable),
  [{attribute, Line, module, Name}, {attribute, Line, parent, Parent},
   {attribute, Line, export, Export}, {attribute, Line, protected, Protected} | Base].

transform_attribute(Line, {mixins, List}) ->
  {attribute, Line, mixins, lists:delete('Module::Methods', List)};

transform_attribute(Line, X) ->
  {attribute, Line, element(1, X), element(2, X)}.

% Compile and load given forms as an Erlang module.
load_form(Forms, Filename) ->
  case compile:forms(Forms, [return]) of
    {ok, ModuleName, Binary, Warnings} ->
      format_warnings(Filename, Warnings),
      code:load_binary(ModuleName, Filename, Binary);
    {error, Errors, Warnings} ->
      format_warnings(Filename, Warnings),
      format_errors(Filename, Errors)
  end.

format_errors(Filename, []) ->
  elixir_errors:raise(bad, "compilation failed but no error was raised");

format_errors(Filename, Errors) ->
  lists:foreach(fun ({_, Each}) ->
    lists:foreach(fun (Error) -> format_error(Filename, Error) end, Each)
  end, Errors).

format_warnings(Filename, Warnings) ->
  lists:foreach(fun ({_, Each}) ->
    lists:foreach(fun (Warning) -> format_warning(Filename, Warning) end, Each)
  end, Warnings).


% Handle warnings

format_warning(Filename, {Line,_,{unused_var,self}}) ->
  [];

format_warning(Filename, {Line, _, {unused_function, {Name, Arity}}}) ->
  Message = io_lib:format("unused local method ~s/~w\n", [Name, Arity-1]),
  io:format(elixir_errors:file_format(Line, Filename, Message));

format_warning(Filename, {Line,Module,Desc}) ->
  Message = Module:format_error(Desc),
  io:format(elixir_errors:file_format(Line, Filename, Message) ++ [$\n]).

% Handle errors

format_error(Filename, {Line,Module,{undefined_function,{Name, Arity}}}) ->
  Message = io_lib:format("undefined local method ~s/~w", [Name, Arity-1]),
  elixir_errors:file_error(undefined_local_method, Line, Filename, Message);

format_error(Filename, {Line,Module,Desc}) ->
  Message = Module:format_error(Desc),
  elixir_errors:file_error(reason_from_desc(Desc), Line, Filename, Message).

reason_from_desc(Desc) when is_tuple(Desc) ->
  element(1, Desc);

reason_from_desc(Desc) ->
  Desc.