-module(elixir_module).
-export([translate/4, compile/4, forwardings/1,
   format_error/1, binding_and_scope_for_eval/4]).
-include("elixir.hrl").

binding_and_scope_for_eval(Line, Filename, Module, Binding) ->
  binding_and_scope_for_eval(Line, Filename, Module, Binding, #elixir_scope{filename=Filename}).

binding_and_scope_for_eval(_Line, _Filename, Module, Binding, S) ->
  {
    binding_for_eval(Module, Binding),
    scope_for_eval(Module, S)
  }.

binding_for_eval(Module, Binding) -> [{'_EXMODULE',Module}|Binding].
scope_for_eval(Module, S) -> S#elixir_scope{module=Module}.

forwardings(Module) ->
  ets:lookup_element(data_table(Module), forwardings, 2).

%% TABLE METHODS

data_table(Module) ->
  ?ELIXIR_ATOM_CONCAT([d, Module]).

attribute_table(Module) ->
  ?ELIXIR_ATOM_CONCAT([a, Module]).

docs_table(Module) ->
  ?ELIXIR_ATOM_CONCAT([o, Module]).

%% TRANSFORMATION METHODS

%% Transformation of args and scope into a compiled erlang call.
%% The abstract form for extra arguments may be given and they
%% will be passed to the invoked function.

translate(Line, Ref, Block, S) ->
  MetaBlock = elixir_tree_helpers:abstract_syntax(Block),
  MetaS     = elixir_variables:serialize_scope(S),

  Args = [{integer, Line, Line}, Ref, MetaBlock, MetaS],
  ?ELIXIR_WRAP_CALL(Line, ?MODULE, compile, Args).

%% The compilation hook.

compile(Line, Module, Block, RawS) when is_atom(Module) ->
  S = elixir_variables:deserialize_scope(RawS),
  C = S#elixir_scope.compile,
  Filename = S#elixir_scope.filename,

  check_module_availability(Line, Filename, Module, S#elixir_scope.compile),
  build(Module),

  try
    Result           = eval_form(Line, Filename, Module, Block, S),
    { Funs, Forms0 } = functions_form(Line, Filename, Module, C),
    Forms1           = attributes_form(Line, Filename, Module, Forms0),

    elixir_import:ensure_no_local_conflict(Line, Filename, Module, Funs),
    elixir_import:ensure_no_import_conflict(Line, Filename, Module, Funs),

    Final = [
      {attribute, Line, module, Module},
      {attribute, Line, file, {Filename,Line}} | Forms1
    ],

    load_form(Final, S),
    Result
  after
    ets:delete(data_table(Module)),
    ets:delete(docs_table(Module)),
    ets:delete(attribute_table(Module)),
    elixir_def:delete_table(Module),
    elixir_import:delete_table(Module)
  end;

compile(Line, Other, _Block, RawS) ->
  S = elixir_variables:deserialize_scope(RawS),
  elixir_errors:form_error(Line, S#elixir_scope.filename, ?MODULE, { invalid_module, Other }).

%% Hook that builds both attribute and functions and set up common hooks.

build(Module) ->
  %% Data table with defaults
  DataTable = data_table(Module),
  ets:new(DataTable, [set, named_table, private]),
  ets:insert(DataTable, { data, [] }),
  ets:insert(DataTable, { compile_callbacks, [] }),
  ets:insert(DataTable, { forwardings, [] }),
  ets:insert(DataTable, { registered_attributes, [behavior, behaviour, compile, vsn, on_load] }),

  AttrTable = attribute_table(Module),
  ets:new(AttrTable, [bag, named_table, private]),

  DocsTable = docs_table(Module),
  ets:new(DocsTable, [ordered_set, named_table, private]),

  %% Function and imports table
  elixir_def:build_table(Module),
  elixir_import:build_table(Module).

%% Receives the module representation and evaluates it.

eval_form(Line, Filename, Module, Block, RawS) ->
  Temp = ?ELIXIR_ATOM_CONCAT(['COMPILE-',Module]),
  { Binding, S } = binding_and_scope_for_eval(Line, Filename, Module, [], RawS),
  { Value, NewS } = elixir_compiler:eval_forms([Block], Line, Temp, S),
  { Callbacks, FinalS } = callbacks_for(Line, compile_callbacks, Module, [Module], NewS),
  elixir:eval_forms(Callbacks, binding_for_eval(Module, Binding), FinalS#elixir_scope{check_clauses=false}),
  Forwardings = ets:lookup_element(data_table(Module), forwardings, 2),
  case Forwardings of
    [] -> [];
    _  -> '::Module':compile_forwardings(Module, Forwardings)
  end,
  Value.

%% Return the form with exports and function declarations.

functions_form(Line, Filename, Module, C) ->
  { Export, Private, Macros, Functions } = elixir_def:unwrap_stored_definitions(Module),

  { FinalExport, FinalFunctions } =
    add_info_function(Line, Filename, Module, Export, Functions, Macros, C),

  { FinalExport ++ Private, [
    {attribute, Line, export, lists:sort(FinalExport)} | FinalFunctions
  ] }.

%% Add attributes handling to the form

attributes_form(Line, _Filename, Module, Current) ->
  Transform = fun(X, Acc) -> [translate_attribute(Line, X)|Acc] end,
  ets:foldr(Transform, Current, attribute_table(Module)).

%% Loads the form into the code server.

load_form(Forms, S) ->
  elixir_compiler:module(Forms, S, fun(ModuleName, Binary) ->
    case get(elixir_compiled) of
      Current when is_list(Current) ->
        put(elixir_compiled, [{ModuleName,Binary}|Current]);
      _ ->
        []
    end
  end).

check_module_availability(Line, Filename, Module, #elixir_compile{ignore_module_conflict=false}) ->
  case code:ensure_loaded(Module) of
    { module, _ } -> elixir_errors:form_error(Line, Filename, ?MODULE, { module_defined, Module });
    { error, _ }  -> []
  end;

check_module_availability(_Line, _Filename, _Module, _C) ->
  [].

% EXTRA FUNCTIONS

add_info_function(Line, Filename, Module, Export, Functions, Macros, C) ->
  Pair = { '__info__', 1 },
  case lists:member(Pair, Export) of
    true  -> elixir_errors:form_error(Line, Filename, ?MODULE, {internal_function_overridden, Pair});
    false ->
      Contents = { function, Line, '__info__', 1, [
        macros_clause(Line, Macros),
        data_clause(Line, Module),
        docs_clause(Line, Module, C),
        moduledoc_clause(Line, Module, C),
        else_clause(Line)
      ] },
      { [Pair|Export], [Contents|Functions] }
  end.

macros_clause(Line, Macros) ->
  Sorted = lists:sort(Macros),
  { clause, Line, [{ atom, Line, macros }], [], [elixir_tree_helpers:abstract_syntax(Sorted)] }.

docs_clause(Line, Module, #elixir_compile { docs = true }) ->
  Docs = ets:tab2list(docs_table(Module)),
  { clause, Line, [{ atom, Line, docs }], [], [elixir_tree_helpers:abstract_syntax(Docs)] };

docs_clause(Line, _Module, _) ->
  { clause, Line, [{ atom, Line, docs }], [], [{ atom, Line, nil }] }.

moduledoc_clause(Line, Module, #elixir_compile { docs = true }) ->
  Docs = '::Module':read_data(Module, moduledoc),
  { clause, Line, [{ atom, Line, moduledoc }], [], [elixir_tree_helpers:abstract_syntax({ Line, Docs })] };

moduledoc_clause(Line, _Module, _) ->
  { clause, Line, [{ atom, Line, moduledoc }], [], [{ atom, Line, nil }] }.

data_clause(Line, Module) ->
  AttrTable  = attribute_table(Module),
  DataTable  = data_table(Module),
  Data       = ets:lookup_element(DataTable, data, 2),
  Registered = ets:lookup_element(DataTable, registered_attributes, 2),
  Pruned     = translate_data(AttrTable, Registered, Data),
  { clause, Line, [{ atom, Line, data }], [], [elixir_tree_helpers:abstract_syntax(Pruned)] }.

else_clause(Line) ->
  Info = { call, Line, { atom, Line, module_info }, [{ var, Line, atom }] },
  { clause, Line, [{ var, Line, atom }], [], [Info] }.

% HELPERS

callbacks_for(Line, Kind, Module, Args, S) ->
  Table = data_table(Module),
  Callbacks = ets:lookup_element(Table, Kind, 2),

  { Exprs, Refers } = lists:mapfoldl(
    fun (X, Acc) -> each_callback_for(Line, Args, X, Acc) end,
    S#elixir_scope.refer, Callbacks),

  { Exprs, S#elixir_scope{refer=Refers} }.

each_callback_for(Line, Args, {M,F}, Acc) ->
  Expr = { { '.', Line, [M,F] }, Line, Args },
  Refer = case orddict:find(M, Acc) of
    { ok, _ } -> Acc;
    _ -> orddict:store(M, M, Acc)
  end,
  { Expr, Refer }.

% ATTRIBUTES

translate_data(Table, Registered, [{_,nil}|T]) ->
  translate_data(Table, Registered, T);

translate_data(Table, Registered, [{Skip,_}|T]) when Skip == doc; Skip == moduledoc ->
  translate_data(Table, Registered, T);

translate_data(Table, Registered, [{on_load,V}|T]) when is_atom(V) ->
  translate_data(Table, Registered, [{on_load,{V,0}}|T]);

translate_data(Table, Registered, [{K,V}|T]) ->
  case reserved_data(Registered, K) of
    true  ->
      ets:insert(Table, { K, V }),
      translate_data(Table, Registered, T);
    false -> [{K,V}|translate_data(Table, Registered, T)]
  end;

translate_data(_, _, []) -> [].

translate_attribute(Line, X) ->
  { attribute, Line, element(1, X), element(2, X) }.

reserved_data(_, callback)     -> true;
reserved_data(_, type)         -> true;
reserved_data(_, export_type)  -> true;
reserved_data(_, spec)         -> true;
reserved_data(Registered, Key) -> lists:member(Key, Registered).

% ERROR HANDLING

format_error({ internal_function_overridden, { Name, Arity } }) ->
  io_lib:format("function ~s/~B is internal and should not be overriden", [Name, Arity]);

format_error({ invalid_module, Module}) ->
  io_lib:format("invalid module name: ~p", [Module]);

format_error({ no_super, { Name, Arity }, Module, Defined }) ->
  Bins = [ joined(X,Y) || { X, Y } <- Defined],
  Joined = '::Enum':join(Bins, <<", ">>),
  io_lib:format("no super defined for ~s/~B in module ~p. Forwardings defined are: ~s", [Name, Arity, Module, Joined]);

format_error({ module_defined, Module }) ->
  io_lib:format("module ~s already defined", [Module]).

joined(X, Y) ->
  A = atom_to_binary(X, utf8),
  B = list_to_binary(integer_to_list(Y)),
  << A/binary, $/, B/binary >>.