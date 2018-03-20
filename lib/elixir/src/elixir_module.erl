-module(elixir_module).
-export([data_table/1, data_tables/1, is_open/1, delete_definition_attributes/6,
         compile/4, expand_callback/6, format_error/1, compiler_modules/0,
         write_cache/3, read_cache/2]).
-include("elixir.hrl").

-define(lexical_attr, {elixir, lexical_tracker}).
-define(persisted_attr, {elixir, persisted_attributes}).

%% Stores modules currently being defined by the compiler

compiler_modules() ->
  case erlang:get(elixir_compiler_modules) of
    undefined -> [];
    M when is_list(M) -> M
  end.

put_compiler_modules([]) ->
  erlang:erase(elixir_compiler_modules);
put_compiler_modules(M) when is_list(M) ->
  erlang:put(elixir_compiler_modules, M).

%% Table functions

data_table(Module) ->
  ets:lookup_element(elixir_modules, Module, 2).

data_tables(Module) ->
  ets:lookup_element(elixir_modules, Module, 3).

is_open(Module) ->
  ets:lookup(elixir_modules, Module) /= [].

delete_definition_attributes(#{module := Module}, _, _, _, _, _) ->
  DataSet = data_table(Module),
  ets:delete(DataSet, doc),
  ets:delete(DataSet, since),
  ets:delete(DataSet, deprecated),
  ets:delete(DataSet, impl).

write_cache(Module, Key, Value) ->
  ets:insert(data_table(Module), {{cache, Key}, Value}).

read_cache(Module, Key) ->
  ets:lookup_element(data_table(Module), {cache, Key}, 2).

%% Compilation hook

compile(Module, Block, Vars, #{line := Line} = Env) when is_atom(Module) ->
  %% In case we are generating a module from inside a function,
  %% we get rid of the lexical tracker information as, at this
  %% point, the lexical tracker process is long gone.
  LexEnv = case ?key(Env, function) of
    nil -> Env#{module := Module, unused_vars := #{}};
    _   -> Env#{lexical_tracker := nil, function := nil, module := Module, unused_vars := #{}}
  end,

  case ?key(LexEnv, lexical_tracker) of
    nil ->
      elixir_lexical:run(?key(LexEnv, file), nil, fun(Pid) ->
        compile(Line, Module, Block, Vars, LexEnv#{lexical_tracker := Pid})
      end);
    _ ->
      compile(Line, Module, Block, Vars, LexEnv)
  end;
compile(Module, _Block, _Vars, #{line := Line, file := File}) ->
  elixir_errors:form_error([{line, Line}], File, ?MODULE, {invalid_module, Module}).

compile(Line, Module, Block, Vars, E) ->
  File = ?key(E, file),
  check_module_availability(Line, File, Module),

  CompilerModules = compiler_modules(),
  {DataSet, DataBag, Ref} = build(Line, File, Module, ?key(E, lexical_tracker)),

  try
    put_compiler_modules([Module | CompilerModules]),
    {Result, NE, OverridablePairs} = eval_form(Line, Module, DataSet, Block, Vars, E),

    PersistedAttributes = ets:lookup_element(DataSet, ?persisted_attr, 2),
    Attributes = attributes(DataSet, PersistedAttributes),
    {AllDefinitions, Unreachable} = elixir_def:fetch_definitions(File, Module),

    (not elixir_config:get(bootstrap)) andalso
     'Elixir.Module':check_behaviours_and_impls(E, DataSet, DataBag, AllDefinitions, OverridablePairs),

    CompileOpts = lists:flatten(ets:lookup_element(DataSet, compile, 2)),

    ModuleMap = #{
      module => Module,
      line => Line,
      file => File,
      attributes => Attributes,
      definitions => AllDefinitions,
      unreachable => Unreachable,
      compile_opts => CompileOpts,
      deprecated => get_deprecated(DataBag)
    },

    Binary = elixir_erl:compile(ModuleMap),
    warn_unused_attributes(File, DataSet, PersistedAttributes),
    autoload_module(Module, Binary, CompileOpts, NE),
    eval_callbacks(Line, DataSet, after_compile, [NE, Binary], NE),
    make_module_available(Module, Binary),
    {module, Module, Binary, Result}
  catch
    error:undef ->
      case erlang:get_stacktrace() of
        [{Module, Fun, Args, _Info} | _] = Stack when is_list(Args) ->
          compile_undef(Module, Fun, length(Args), Stack);
        [{Module, Fun, Arity, _Info} | _] = Stack ->
          compile_undef(Module, Fun, Arity, Stack);
        Stack ->
          erlang:raise(error, undef, Stack)
      end
  after
    put_compiler_modules(CompilerModules),
    ets:delete(DataSet),
    ets:delete(DataBag),
    elixir_code_server:call({undefmodule, Ref})
  end.

%% An undef error for a function in the module being compiled might result in an
%% exception message suggesting the current module is not loaded. This is
%% misleading so use a custom reason.
compile_undef(Module, Fun, Arity, Stack) ->
  ExMod = 'Elixir.UndefinedFunctionError',
  case code:ensure_loaded(ExMod) of
    {module, _} ->
      Opts = [{module, Module}, {function, Fun}, {arity, Arity},
              {reason, 'function not available'}],
      Exception = 'Elixir.UndefinedFunctionError':exception(Opts),
      erlang:raise(error, Exception, Stack);
    {_, _} ->
      erlang:raise(error, undef, Stack)
  end.

%% Handle reserved modules and duplicates.

check_module_availability(Line, File, Module) ->
  Reserved = ['Elixir.Any', 'Elixir.BitString', 'Elixir.Function', 'Elixir.PID',
              'Elixir.Reference', 'Elixir.Elixir', 'Elixir'],

  case lists:member(Module, Reserved) of
    true  -> elixir_errors:form_error([{line, Line}], File, ?MODULE, {module_reserved, Module});
    false -> ok
  end,

  case elixir_compiler:get_opt(ignore_module_conflict) of
    false ->
      case code:ensure_loaded(Module) of
        {module, _} ->
          elixir_errors:form_warn([{line, Line}], File, ?MODULE, {module_defined, Module});
        {error, _}  ->
          ok
      end;
    true ->
      ok
  end.

%% Hook that builds both attribute and functions and set up common hooks.

build(Line, File, Module, Lexical) ->
  %% In the set table we store:
  %%
  %% * {Attribute, Value, AccumulateOrReadOrUnreadline}
  %% * {{elixir, ...}, ...}
  %% * {{doc, Tuple}, ...}
  %% * {{typedoc, Tuple}, ...}
  %% * {{callbackdoc, Tuple}, ...}
  %% * {{def, Tuple}, ...} (from elixir_def)
  %% * {{import, Tuple}, ...} (from_elixir_locals)
  %%
  DataSet = ets:new(Module, [set, public]),

  %% In the bag table we store:
  %%
  %% * {{attribute, Attribute}, Value}
  %% * {impls, ...}
  %% * {deprecated, ...}
  %% * {defs, ...} (from elixir_def)
  %% * {{default, Name}, ...} (from elixir_def)
  %% * {{clauses, Tuple}, ...} (from elixir_def)
  %% * {reattach, ...} (from elixir_local)
  %% * {{local, Tuple}, ...} (from elixir_local)
  %%
  DataBag = ets:new(Module, [duplicate_bag, public]),

  OnDefinition =
    case elixir_config:get(bootstrap) of
      false -> [{'Elixir.Module', compile_definition_attributes}];
      _ -> [{elixir_module, delete_definition_attributes}]
    end,

  ets:insert(DataSet, [
    % {Key, Value, Accumulate?, UnreadLine}
    {after_compile, [], true, nil},
    {before_compile, [], true, nil},
    {behaviour, [], true, nil},
    {compile, [], true, nil},
    {derive, [], true, nil},
    {dialyzer, [], true, nil},
    {external_resource, [], true, nil},
    {moduledoc, nil, false, nil},
    {on_definition, OnDefinition, true, nil},
    {on_load, [], true, nil},

    % Types
    {callback, [], true, nil},
    {opaque, [], true, nil},
    {optional_callbacks, [], true, nil},
    {macrocallback, [], true, nil},
    {spec, [], true, nil},
    {type, [], true, nil},
    {typep, [], true, nil}
  ]),

  Persisted = [behaviour, on_load, compile, external_resource, dialyzer, vsn],
  ets:insert(DataSet, {?persisted_attr, Persisted}),
  ets:insert(DataSet, {?lexical_attr, Lexical}),

  %% Setup definition related modules
  Tables = {DataSet, DataBag},
  elixir_def:setup(Tables),
  elixir_locals:setup(Tables),
  elixir_overridable:setup(Tables),
  Tuple = {Module, DataSet, Tables, Line, File},

  Ref =
    case elixir_code_server:call({defmodule, Module, self(), Tuple}) of
      {ok, ModuleRef} ->
        ModuleRef;
      {error, {Module, _, _, OldLine, OldFile}} ->
        ets:delete(DataSet),
        ets:delete(DataBag),
        Error = {module_in_definition, Module, OldFile, OldLine},
        elixir_errors:form_error([{line, Line}], File, ?MODULE, Error)
    end,

  {DataSet, DataBag, Ref}.

%% Handles module and callback evaluations.

eval_form(Line, Module, DataSet, Block, Vars, E) ->
  {Value, EE} = elixir_compiler:eval_forms(Block, Vars, E),
  Pairs1 = elixir_overridable:store_pending(Module),
  EV = elixir_env:linify({Line, elixir_env:reset_vars(EE)}),
  EC = eval_callbacks(Line, DataSet, before_compile, [EV], EV),
  Pairs2 = elixir_overridable:store_pending(Module),
  OverridablePairs = Pairs1 ++ Pairs2,
  {Value, EC, OverridablePairs}.

eval_callbacks(Line, DataSet, Name, Args, E) ->
  Callbacks = ets:lookup_element(DataSet, Name, 2),
  lists:foldr(fun({M, F}, Acc) ->
    expand_callback(Line, M, F, Args, elixir_env:reset_vars(Acc),
                    fun(AM, AF, AA) -> apply(AM, AF, AA) end)
  end, E, Callbacks).

expand_callback(Line, M, F, Args, E, Fun) ->
  Meta = [{line, Line}, {required, true}],

  {EE, ET} = elixir_dispatch:dispatch_require(Meta, M, F, Args, E, fun(AM, AF, AA) ->
    Fun(AM, AF, AA),
    {ok, E}
  end),

  if
    is_atom(EE) ->
      ET;
    true ->
      try
        {_Value, _Binding, EF, _S} = elixir:eval_forms(EE, [], ET),
        EF
      catch
        Kind:Reason ->
          Stacktrace = erlang:get_stacktrace(),
          Info = {M, F, length(Args), location(Line, E)},
          erlang:raise(Kind, Reason, prune_stacktrace(Info, Stacktrace))
      end
  end.

%% Add attributes handling to the form

attributes(DataSet, PersistedAttributes) ->
  [{Key, Value} || Key <- PersistedAttributes, Value <- lookup_attribute(DataSet, Key)].

lookup_attribute(DataSet, Key) when is_atom(Key) ->
  case ets:lookup(DataSet, Key) of
    [{Key, Values, true, _}] -> Values;
    [{Key, Value, false, _}] -> [Value];
    [] -> []
  end.

warn_unused_attributes(File, DataSet, PersistedAttrs) ->
  ReservedAttrs = [after_compile, before_compile, moduledoc,
                   on_definition, optional_callbacks | PersistedAttrs],
  Keys = ets:select(DataSet, [{{'$1', '_', '_', '$2'}, [{is_atom, '$1'}, {is_integer, '$2'}], [['$1', '$2']]}]),
  [elixir_errors:form_warn([{line, Line}], File, ?MODULE, {unused_attribute, Key}) ||
   [Key, Line] <- Keys, not lists:member(Key, ReservedAttrs)].

get_deprecated(Bag) ->
  lists:usort(bag_lookup_element(Bag, deprecated, 2)).

bag_lookup_element(Table, Name, Pos) ->
  try
    ets:lookup_element(Table, Name, Pos)
  catch
    error:badarg -> []
  end.

%% Takes care of autoloading the module if configured.

autoload_module(Module, Binary, Opts, E) ->
  case proplists:get_value(autoload, Opts, true) of
    true  -> code:load_binary(Module, beam_location(E), Binary);
    false -> ok
  end.

beam_location(#{lexical_tracker := Pid, module := Module}) ->
  case elixir_lexical:dest(Pid) of
    nil -> "";
    Dest ->
      filename:join(elixir_utils:characters_to_list(Dest),
                    atom_to_list(Module) ++ ".beam")
  end.

%% Integration with elixir_compiler that makes the module available

make_module_available(Module, Binary) ->
  case get(elixir_module_binaries) of
    Current when is_list(Current) ->
      put(elixir_module_binaries, [{Module, Binary} | Current]);
    _ ->
      ok
  end,

  case get(elixir_compiler_pid) of
    undefined ->
      ok;
    PID ->
      Ref = make_ref(),
      PID ! {module_available, self(), Ref, get(elixir_compiler_file), Module, Binary},
      receive {Ref, ack} -> ok end
  end.

%% Error handling and helpers.

%% We've reached the elixir_module or eval internals, skip it with the rest
prune_stacktrace(Info, [{elixir, eval_forms, _, _} | _]) ->
  [Info];
prune_stacktrace(Info, [{elixir_module, _, _, _} | _]) ->
  [Info];
prune_stacktrace(Info, [H | T]) ->
  [H | prune_stacktrace(Info, T)];
prune_stacktrace(Info, []) ->
  [Info].

location(Line, E) ->
  [{file, elixir_utils:characters_to_list(?key(E, file))}, {line, Line}].

format_error({unused_attribute, typedoc}) ->
  "module attribute @typedoc was set but no type follows it";
format_error({unused_attribute, doc}) ->
  "module attribute @doc was set but no definition follows it";
format_error({unused_attribute, impl}) ->
  "module attribute @impl was set but no definition follows it";
format_error({unused_attribute, Attr}) ->
  io_lib:format("module attribute @~ts was set but never used", [Attr]);
format_error({invalid_module, Module}) ->
  io_lib:format("invalid module name: ~ts", ['Elixir.Kernel':inspect(Module)]);
format_error({module_defined, Module}) ->
  Extra =
    case code:which(Module) of
      "" ->
        " (current version defined in memory)";
      Path when is_list(Path) ->
        io_lib:format(" (current version loaded from ~ts)", [elixir_utils:relative_to_cwd(Path)]);
      _ ->
        ""
    end,
  io_lib:format("redefining module ~ts~ts", [elixir_aliases:inspect(Module), Extra]);
format_error({module_reserved, Module}) ->
  io_lib:format("module ~ts is reserved and cannot be defined", [elixir_aliases:inspect(Module)]);
format_error({module_in_definition, Module, File, Line}) ->
  io_lib:format("cannot define module ~ts because it is currently being defined in ~ts:~B",
    [elixir_aliases:inspect(Module), elixir_utils:relative_to_cwd(File), Line]).
