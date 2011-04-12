-module(elixir_object).
-export([build/1, scope_for/2, transform/6, compile/8, default_mixins/3, default_protos/3]).
-include("elixir.hrl").

%% EXTERNAL API

% Build an object from the given name. Used by elixir_constants to
% build objects from their compiled modules. Assumes the given name
% exists.
build(Name) ->
  Attributes = Name:module_info(attributes),
  Snapshot = proplists:get_value(snapshot, Attributes),
  case Snapshot of
    [Value] -> Value;
    _ -> Snapshot
  end.

%% TEMPLATE BUILDING FOR MODULE COMPILATION

% Build a template of an object or module used on compilation.
build_template(Kind, Name, Template) ->
  Parent = default_parent(Kind, Name),
  Mixins = default_mixins(Kind, Name, Template),
  Protos = default_protos(Kind, Name, Template),
  Data   = default_data(Template),

  AttributeTable = ?ELIXIR_ATOM_CONCAT([aex_, Name]),
  ets:new(AttributeTable, [set, named_table, private]),

  case Kind of
    object ->
      ets:insert(AttributeTable, { module, [] }),
      ets:insert(AttributeTable, { proto, [] }),
      ets:insert(AttributeTable, { mixin, [] });
    _ -> []
  end,

  ets:insert(AttributeTable, { mixins, Mixins }),
  ets:insert(AttributeTable, { protos, Protos }),
  ets:insert(AttributeTable, { data,   Data }),

  Object = #elixir_object__{name=Name, parent=Parent, mixins=[], protos=[], data=AttributeTable},
  { Object, AttributeTable, { Mixins, Protos } }.

% Returns the parent object based on the declaration.
default_parent(_, 'Object')   -> [];
default_parent(object, _Name) -> 'Object';
default_parent(module, _Name) -> 'Module'.

% Default mixins based on the declaration type.
default_mixins(_, 'Object', _Template)  -> ['Object::Methods']; % object Object
default_mixins(_, 'Module', _Template)  -> ['Module::Methods']; % object Module
default_mixins(object, _Name, [])       -> ['Module::Methods']; % object Post
default_mixins(module, _Name, [])       -> [];                  % module Numeric
default_mixins(object, _Name, Template) ->                      % object SimplePost from Post
  Template#elixir_object__.mixins ++ ['Module::Methods'].

% Default prototypes. Modules have themselves as the default prototype.
default_protos(_, 'Object', _Template)  -> ['Object::Methods']; % object Object
default_protos(_, 'Module', _Template)  -> ['Module::Methods']; % object Module
default_protos(_Kind, _Name, [])        -> [];                  % module Numeric
default_protos(object, _Name, Template) ->                      % object SimplePost from Post
  Template#elixir_object__.protos.

% Returns the default data from parents.
default_data([])       -> orddict:new();
default_data(Template) -> Template#elixir_object__.data.

%% USED ON TRANSFORMATION AND MODULE COMPILATION

% Returns the new module name based on the previous scope.
scope_for([], Name) -> Name;
scope_for(Scope, Name) -> ?ELIXIR_ATOM_CONCAT([Scope, "::", Name]).

% Generates module transform. It wraps the module definition into
% a function that will be invoked by compile/7 passing self as argument.
% We need to wrap them into anonymous functions so nested module
% definitions have the variable self shadowed.
transform(Kind, Line, Name, Parent, Body, S) ->
  Filename = S#elixir_scope.filename,
  CompilePath = S#elixir_scope.compile_path,
  Clause = { clause, Line, [{var, Line, self}], [], Body },
  Fun = { 'fun', Line, { clauses, [Clause] } },
  Args = [{atom, Line, Kind}, {integer, Line, Line}, {string, Line, Filename},
    {string, Line, CompilePath}, {var, Line, self}, {atom, Line, Name}, {atom, Line, Parent}, Fun],
  ?ELIXIR_WRAP_CALL(Line, ?MODULE, compile, Args).

% Initial step of template compilation. Generate a method
% table and pass it forward to the next compile method.
compile(Kind, Line, Filename, CompilePath, Current, Name, Template, Fun) ->
  MethodTable = elixir_def_method:new_method_table(Name),

  try
    compile(Kind, Line, Filename, CompilePath, Current, Name, Template, Fun, MethodTable)
  catch
    Type:Reason -> clean_up_tables(Type, Reason)
  end.

% Receive the module function to be invoked, invoke it passing
% self and then compile the added methods into an Erlang module
% and loaded it in the VM.
compile(Kind, Line, Filename, CompilePath, Current, Name, Template, Fun, MethodTable) ->
  { Object, AttributeTable, Extra } = build_template(Kind, Name, Template),

  try
    Result = Fun(Object),

    try
      elixir_constants:lookup(Name),
      error({objectdefined, Name})
    catch
      error:{noconstant, _} -> []
    end,

    compile_kind(Kind, Line, Filename, CompilePath, Current, Object, Extra, MethodTable),
    Result
  catch
    Type:Reason -> clean_up_tables(Type, Reason)
  end.

% Handle compilation logic specific to objects or modules.
compile_kind(module, Line, Filename, CompilePath, Current, Module, _, MethodTable) ->
  Name = Module#elixir_object__.name,

  % Update mixins to have the module itself
  AttributeTable = Module#elixir_object__.data,
  Mixins = ets:lookup_element(AttributeTable, mixins, 2),
  ets:insert(AttributeTable, { mixins, [Name|Mixins] }),

  case add_implicit_modules(Current, Module, { Line, Filename, Module, MethodTable }) of
    true -> [];
    false ->
      case Name of
        'Object::Methods' -> [];
        'Module::Methods' -> [];
        _ -> elixir_def_method:flat_module(Module, Line, mixins, Module, MethodTable)
      end,
      compile_module(Line, Filename, CompilePath, Module, MethodTable)
  end;

% Compile an object. If methods were defined in the object scope,
% we create a Proto module and automatically include it.
compile_kind(object, Line, Filename, CompilePath, Current, Object, { Mixins, Protos }, MethodTable) ->
  AttributeTable = Object#elixir_object__.data,

  % Check if methods were defined, if so, create a Proto module.
  case elixir_def_method:is_empty_table(MethodTable) of
    true  ->
      ets:delete(AttributeTable, module),
      ets:delete(MethodTable);
    false ->
      Name = ?ELIXIR_ATOM_CONCAT([Object#elixir_object__.name, "::Proto"]),
      Attributes = destructive_read(AttributeTable, module),
      Define = elixir_module_methods:copy_attributes_fun(Attributes),
      compile(module, Line, Filename, CompilePath, Object, Name, [], Define, MethodTable)
  end,

  % Generate implicit modules if there isn't a ::Proto or ::Mixin
  % and protos and mixins were added.
  generate_implicit_module_if(Line, Filename, CompilePath, Protos, Object, proto, "::Proto"),
  generate_implicit_module_if(Line, Filename, CompilePath, Mixins, Object, mixin, "::Mixin"),

  % Read implicitly added modules, compile the form and load implicit modules.
  Proto = read_implicit_module(Object, AttributeTable, proto, CompilePath),
  Mixin = read_implicit_module(Object, AttributeTable, mixin, CompilePath),

  load_form(build_erlang_form(Line, Filename, Object, {Mixin, Proto}), Filename, CompilePath),
  ets:delete(AttributeTable).

% Handle logic compilation. Called by both compile_kind(module) and compile_kind(object).
% The latter uses it for implicit modules.
compile_module(Line, Filename, CompilePath, Module, MethodTable) ->
  Functions = elixir_def_method:unwrap_stored_methods(MethodTable),
  load_form(build_erlang_form(Line, Filename, Module, {[],[]}, Functions), Filename, CompilePath),
  ets:delete(Module#elixir_object__.data),
  ets:delete(MethodTable).

% Check if the module currently defined is inside an object
% definition an automatically include it.
add_implicit_modules(#elixir_object__{parent='Module'} = Self, _Module, _Copy) -> false;

add_implicit_modules(#elixir_object__{name=Name, data=AttributeTable} = Self, Module, Copy) ->
  Proto = lists:concat([Name, "::Proto"]),
  Mixin = lists:concat([Name, "::Mixin"]),
  case atom_to_list(Module#elixir_object__.name) of
    Proto -> ets:insert(AttributeTable, { proto, Copy }), true;
    Mixin -> ets:insert(AttributeTable, { mixin, Copy }), true;
    _ -> false
  end;

add_implicit_modules(_, _, _) -> false.

% Read implicit modules for object

read_implicit_module(Object, AttributeTable, Attribute, CompilePath) ->
  Value = destructive_read(AttributeTable, Attribute),
  case Value of
    [] -> [];
    { Line, Filename, Module, MethodTable } ->
      elixir_object_methods:Attribute(Object, Module, false),
      elixir_def_method:flat_module(Object, Line, ?ELIXIR_ATOM_CONCAT([Attribute,"s"]), Module, MethodTable),
      compile_module(Line, Filename, CompilePath, Module, MethodTable),
      Module
  end.

generate_implicit_module_if(Line, Filename, CompilePath, Match,
  #elixir_object__{name=Name, data=AttributeTable} = Object, Attribute, Suffix) ->

  Method = ?ELIXIR_ATOM_CONCAT([Attribute, "s"]),
  Bool1 = ets:lookup_element(AttributeTable, Method, 2) == Match,
  Bool2 = ets:lookup_element(AttributeTable, Attribute, 2) /= [],

  if
    Bool1 or Bool2 -> [];
    true ->
      Implicit = ?ELIXIR_ATOM_CONCAT([Name, Suffix]),
      compile(module, Line, Filename, CompilePath, Object, Implicit, [], fun(X) -> [] end)
  end.

% Retrieve all attributes in the attribute table and generate
% an Erlang Abstract Form that defines an Erlang module.
build_erlang_form(Line, Filename, Object, Chains) ->
  build_erlang_form(Line, Filename, Object, Chains, {[],[],[]}).

build_erlang_form(Line, Filename, Object, {Mixin, Proto}, {Export, Inherited, Functions}) ->
  Name = Object#elixir_object__.name,
  Parent = Object#elixir_object__.parent,
  AttributeTable = Object#elixir_object__.data,
  Data  = destructive_read(AttributeTable, data),
  Snapshot = build_snapshot(Name, Parent, Mixin, Proto, Data),
  Transform = fun(X, Acc) -> [transform_attribute(Line, X)|Acc] end,
  ModuleName = ?ELIXIR_ERL_MODULE(Name),
  Base = ets:foldr(Transform, Functions, AttributeTable) ++ [exported_function(Line, ModuleName)],
  [{attribute, Line, module, ModuleName}, {attribute, Line, parent, Parent},
   {attribute, Line, compile, no_auto_import()}, {attribute, Line, file, Filename},
   {attribute, Line, inherited, Inherited}, {attribute, Line, snapshot, Snapshot},
   {attribute, Line, export, [{'__function_exported__',2}|Export]} | Base].

exported_function(Line, ModuleName) ->
  { function, Line, '__function_exported__', 2,
    [{ clause, Line, [{var,Line,function},{var,Line,arity}], [], [
      ?ELIXIR_WRAP_CALL(
        Line, erlang, function_exported,
        [{atom,Line,ModuleName},{var,Line,function},{var,Line,arity}]
      )
    ]}]
  }.

destructive_read(Table, Attribute) ->
  Value = ets:lookup_element(Table, Attribute, 2),
  ets:delete(Table, Attribute),
  Value.

build_snapshot(Name, Parent, Mixin, Proto, Data) ->
  FinalMixin = snapshot_module(Name, Parent, Mixin),
  FinalProto = snapshot_module(Name, Parent, Proto),
  #elixir_object__{name=Name, parent=Parent, mixins=FinalMixin, protos=FinalProto, data=Data}.

snapshot_module('Object', _, [])      -> 'exObject::Methods';
snapshot_module('Module', _, [])      -> 'exModule::Methods';
snapshot_module(Name,  'Module', _)   -> ?ELIXIR_ERL_MODULE(Name);
snapshot_module(_,  _, [])            -> 'exObject::Methods';
snapshot_module(_, _, Module) -> ?ELIXIR_ERL_MODULE(Module#elixir_object__.name).

no_auto_import() ->
  {no_auto_import, [
    {size, 1}, {length, 1}, {error, 2}, {self, 1}, {put, 2}, {get, 1}
  ]}.

transform_attribute(Line, {mixins, List}) ->
  {attribute, Line, mixins, lists:delete('Module::Methods', List)};

transform_attribute(Line, X) ->
  {attribute, Line, element(1, X), element(2, X)}.

% Compile and load given forms as an Erlang module.
load_form(Forms, Filename, CompilePath) ->
  case compile:forms(Forms, [return]) of
    {ok, ModuleName, Binary, Warnings} ->
      case CompilePath of
        [] -> [];
        _  ->
          Path = filename:join(CompilePath, atom_to_list(ModuleName) ++ ".beam"),
          ok = file:write_file(Path, Binary)
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

clean_up_tables(Type, Reason) ->
  ElixirTables = [atom_to_list(X) || X <- ets:all(), is_atom(X)],
  lists:foreach(fun clean_up_table/1, ElixirTables),
  erlang:Type(Reason).

clean_up_table("aex_" ++ _ = X) ->
  ets:delete(list_to_atom(X));

clean_up_table("mex_" ++ _ = X) ->
  ets:delete(list_to_atom(X));

clean_up_table(_) -> [].