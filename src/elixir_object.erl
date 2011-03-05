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
  #elixir_object__{name=Name, parent=Parent, mixins=Mixins, protos=Protos}.

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

  case Kind of
    object -> ets:insert(AttributeTable, { module, [] });
    _ -> []
  end,

  ets:insert(AttributeTable, { mixins, Mixins }),
  ets:insert(AttributeTable, { protos, Protos }),
  ets:insert(AttributeTable, { data,   Data }),

  Object = #elixir_object__{name=Name, parent=Parent, mixins=[], protos=[], data=AttributeTable},
  { Object, AttributeTable }.

% Returns the parent object based on the declaration.
default_parent(_, 'Object')   -> [];
default_parent(object, _Name) -> 'Object';
default_parent(module, _Name) -> 'Module'.

% Default mixins based on the declaration type.
default_mixins(_, 'Object', _Template)  -> ['Object::Methods']; % object Object
default_mixins(_, 'Module', _Template)  -> ['Module::Methods']; % object Module
default_mixins(object, _Name, [])       -> ['Module::Methods']; % object Post
default_mixins(module, Name, _Template) -> [];                  % module Numeric
default_mixins(object, _Name, Template) ->                      % object SimplePost from Post
  Template#elixir_object__.mixins ++ ['Module::Methods'].

% Default prototypes. Modules have themselves as the default prototype.
default_protos(_, 'Object', _Template)  -> ['Object::Methods'];           % object Object
default_protos(_, 'Module', _Template)  -> ['Module::Methods'];           % object Module
default_protos(object, _Name, [])       -> [];                            % object Post
default_protos(module, Name, _Template) -> [Name];                        % module Numeric
default_protos(object, _Name, Template) -> Template#elixir_object__.protos. % object SimplePost from Post

% Returns the default data from parents.
default_data([])       -> orddict:new();
default_data(Template) -> Template#elixir_object__.data.

%% USED ON TRANSFORMATION AND MODULE COMPILATION

% Returns the new module name based on the previous scope.
scope_for([], Name) -> Name;
scope_for(Scope, Name) -> ?ELIXIR_ATOM_CONCAT([Scope, "::", Name]).

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
  MethodTable = elixir_def_method:new_method_table(Name),

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

merge_module_mixins(Object) ->
  OldMixins = elixir_object_methods:object_mixins(Object),
  OldProtos = elixir_object_methods:object_protos(Object),
  Adder = fun(X, Acc) ->
    case lists:member(X, Acc) of
      true  -> Acc;
      false -> [X|Acc]
    end
  end,
  NewMixins = lists:foldl(Adder, lists:reverse(OldMixins), OldProtos),
  ets:insert(Object#elixir_object__.data, { mixins, lists:reverse(NewMixins) }).

% Handle compilation logic specific to objects or modules.
% TODO Allow object reopening but without method definition.
% TODO Do not allow module reopening.
compile_kind(module, Line, Filename, Current, Object, MethodTable) ->
  Name = Object#elixir_object__.name,
  { Callbacks, Functions } = elixir_def_method:unwrap_stored_methods(MethodTable),
  Behavior = elixir_callbacks:behavior(Object),
  compile_callbacks(Behavior, Line, Filename, Object, Callbacks),
  merge_module_mixins(Object),
  load_form(build_erlang_form(Line, Object, Functions), Filename),
  add_implicit_mixins(Current, Name);

% Compile an object. If methods were defined in the object scope,
% we create a Proto module and automatically include it.
compile_kind(object, Line, Filename, Current, Object, MethodTable) ->
  AttributeTable = Object#elixir_object__.data,
  case elixir_def_method:is_empty_table(MethodTable) of
    true  -> [];
    false ->
      Name = ?ELIXIR_ATOM_CONCAT([Object#elixir_object__.name, "::Proto"]),
      Attributes = ets:lookup_element(AttributeTable, module, 2),
      Define = elixir_module_methods:copy_attributes_fun(Attributes),
      compile(module, Line, Filename, Object, Name, [], Define, MethodTable)
  end,
  ets:delete(AttributeTable, module),
  load_form(build_erlang_form(Line, Object), Filename).

% Handle callbacks compilation.
compile_callbacks([], _Line, _Filename, _Object, _Callbacks) -> [];
compile_callbacks(Behavior, Line, Filename, Object, Callbacks) ->
  elixir_module_methods:define_attribute(Object, behavior, elixir_callbacks),
  Form = elixir_callbacks:build_module_form(Line, Object, Behavior, Callbacks),
  load_form(Form, Filename).

% Check if the module currently defined is inside an object
% definition an automatically include it.
add_implicit_mixins(#elixir_object__{name=Name} = Self, ModuleName) ->
  Proto = lists:concat([Name, "::Proto"]),
  Mixin = lists:concat([Name, "::Mixin"]),
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
  Name = Object#elixir_object__.name,
  Parent = Object#elixir_object__.parent,
  AttrTable = Object#elixir_object__.data,
  Transform = fun(X, Acc) -> [transform_attribute(Line, X)|Acc] end,
  Base = ets:foldr(Transform, Functions, AttrTable),
  [{attribute, Line, module, Name}, {attribute, Line, parent, Parent}, {attribute, Line, compile, no_auto_import()},
   {attribute, Line, export, Export}, {attribute, Line, protected, Protected} | Base].

no_auto_import() ->
  {no_auto_import, [
    {size, 1}, {length, 1}, {error, 2}, {self, 1}, {put, 2}, {get, 1}
  ]}.

transform_attribute(Line, {mixins, List}) ->
  {attribute, Line, mixins, lists:delete('Module::Methods', List)};

transform_attribute(Line, X) ->
  {attribute, Line, element(1, X), element(2, X)}.

% Compile and load given forms as an Erlang module.
load_form(Forms, Filename) ->
  case compile:forms(Forms, [return]) of
    {ok, ModuleName, Binary, Warnings} ->
      case get(elixir_compile_core) of
        true ->
          CompiledName = filename:rootname(Filename, ".ex") ++ ".exb",
          ok = file:write_file(CompiledName, term_to_binary({ModuleName, Filename, Binary}));
        _ -> []
      end,
      format_warnings(Filename, Warnings),
      code:load_binary(ModuleName, Filename, Binary);
    {error, Errors, Warnings} ->
      format_warnings(Filename, Warnings),
      format_errors(Filename, Errors)
  end.

format_errors(Filename, []) ->
  exit({nocompile, "compilation failed but no error was raised"});

format_errors(Filename, Errors) ->
  lists:foreach(fun ({_, Each}) ->
    lists:foreach(fun (Error) -> elixir_errors:handle_file_error(Filename, Error) end, Each)
  end, Errors).

format_warnings(Filename, Warnings) ->
  lists:foreach(fun ({_, Each}) ->
    lists:foreach(fun (Warning) -> elixir_errors:handle_file_warning(Filename, Warning) end, Each)
  end, Warnings).
