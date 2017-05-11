-module(elixir_module).
-export([data_table/1, defs_table/1, is_open/1, delete_doc/6,
         compile/4, expand_callback/6, format_error/1,
         compiler_modules/0, delete_impl/6]).
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

defs_table(Module) ->
  ets:lookup_element(elixir_modules, Module, 3).

is_open(Module) ->
  ets:lookup(elixir_modules, Module) /= [].

delete_doc(#{module := Module}, _, _, _, _, _) ->
  ets:delete(data_table(Module), doc),
  ok.

delete_impl(#{module := Module}, _, _, _, _, _) ->
  ets:delete(data_table(Module), impl),
  ok.

%% Compilation hook

compile(Module, Block, Vars, #{line := Line} = Env) when is_atom(Module) ->
  %% In case we are generating a module from inside a function,
  %% we get rid of the lexical tracker information as, at this
  %% point, the lexical tracker process is long gone.
  LexEnv = case ?key(Env, function) of
    nil -> Env#{module := Module};
    _   -> Env#{lexical_tracker := nil, function := nil, module := Module}
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
  {Data, Defs, Ref} = build(Line, File, Module, ?key(E, lexical_tracker)),

  try
    put_compiler_modules([Module | CompilerModules]),
    {Result, NE, OverridablePairs} = eval_form(Line, Module, Data, Block, Vars, E),

    PersistedAttributes = ets:lookup_element(Data, ?persisted_attr, 2),
    Attributes = attributes(Line, File, Data, PersistedAttributes),
    OnLoad = ets:lookup_element(Data, 'on_load', 2),
    [elixir_locals:record_local(Tuple, Module) || Tuple <- OnLoad],

    {AllDefinitions, Unreachable} = elixir_def:fetch_definitions(File, Module),

    (not elixir_compiler:get_opt(internal)) andalso
     'Elixir.Module':check_behaviours_and_impls(E, Data, AllDefinitions, OverridablePairs),

    CompileOpts = lists:flatten(ets:lookup_element(Data, compile, 2)),

    ModuleMap = #{
      module => Module,
      line => Line,
      file => File,
      attributes => Attributes,
      definitions => AllDefinitions,
      unreachable => Unreachable,
      compile_opts => CompileOpts
    },

    Binary = elixir_erl:compile(ModuleMap),
    warn_unused_attributes(File, Data, PersistedAttributes),
    autoload_module(Module, Binary, CompileOpts, NE),
    eval_callbacks(Line, Data, after_compile, [NE, Binary], NE),
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
    elixir_locals:cleanup(Module),
    ets:delete(Data),
    ets:delete(Defs),
    elixir_code_server:call({undefmodule, Ref})
  end.

%% An undef error for a function in the module being compiled might result in an
%% exception message suggesting the current module is not loaded. This is
%% misleading so use a custom reason.
compile_undef(Module, Fun, Arity, Stack) ->
  ExMod = 'Elixir.UndefinedFunctionError',
  case code:is_loaded(ExMod) of
    false ->
      erlang:raise(error, undef, Stack);
    _ ->
      Opts = [{module, Module}, {function, Fun}, {arity, Arity},
              {reason, 'function not available'}],
      Exception = 'Elixir.UndefinedFunctionError':exception(Opts),
      erlang:raise(error, Exception, Stack)
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
  case elixir_code_server:call({lookup, Module}) of
    [{Module, _, _, OldLine, OldFile}] ->
      Error = {module_in_definition, Module, OldFile, OldLine},
      elixir_errors:form_error([{line, Line}], File, ?MODULE, Error);
    _ ->
      []
  end,

  Data = ets:new(Module, [set, public]),
  Defs = ets:new(Module, [duplicate_bag, public]),
  Ref  = elixir_code_server:call({defmodule, self(),
                                 {Module, Data, Defs, Line, File}}),

  DocsOnDefinition =
      case elixir_compiler:get_opt(docs) of
        true -> [{'Elixir.Module', compile_doc}];
        _    -> [{elixir_module, delete_doc}]
      end,

  ImplOnDefinition =
      case elixir_compiler:get_opt(internal) of
        true -> [{elixir_module, delete_impl}];
        _    -> [{'Elixir.Module', compile_impl}]
      end,

  %% Docs must come first as they read the impl callback.
  OnDefinition = DocsOnDefinition ++ ImplOnDefinition,

  ets:insert(Data, [
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
    {typep, [], true, nil},

    % Internal
    {{elixir, impls}, []}
  ]),

  Persisted = [behaviour, on_load, compile, external_resource, dialyzer, vsn],
  ets:insert(Data, {?persisted_attr, Persisted}),
  ets:insert(Data, {?lexical_attr, Lexical}),

  %% Setup definition related modules
  elixir_def:setup(Module),
  elixir_locals:setup(Module),
  elixir_overridable:setup(Module),

  {Data, Defs, Ref}.

%% Handles module and callback evaluations.

eval_form(Line, Module, Data, Block, Vars, E) ->
  {Value, EE} = elixir_compiler:eval_forms(Block, Vars, E),
  Pairs1 = elixir_overridable:store_pending(Module),
  EV = elixir_env:linify({Line, reset_env(EE)}),
  EC = eval_callbacks(Line, Data, before_compile, [EV], EV),
  Pairs2 = elixir_overridable:store_pending(Module),
  OverridablePairs = Pairs1 ++ Pairs2,
  {Value, EC, OverridablePairs}.

eval_callbacks(Line, Data, Name, Args, E) ->
  Callbacks = ets:lookup_element(Data, Name, 2),
  lists:foldr(fun({M, F}, Acc) ->
    expand_callback(Line, M, F, Args, reset_env(Acc),
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
          Info = {M, F, length(Args), location(Line, E)},
          erlang:raise(Kind, Reason, prune_stacktrace(Info, erlang:get_stacktrace()))
      end
  end.

reset_env(Env) ->
  Env#{vars := [], export_vars := nil}.

%% Add attributes handling to the form

attributes(Line, File, Data, PersistedAttributes) ->
  [{Key, Value} || Key <- PersistedAttributes,
                   Value <- lookup_attribute(Line, File, Data, Key)].

lookup_attribute(Line, File, Data, Key) when is_atom(Key) ->
  case ets:lookup(Data, Key) of
    [{resource, Values, true, _}] ->
      lists:usort([validate_external_resource(Line, File, Value) || Value <- Values]);
    [{Key, Values, true, _}] ->
      Values;
    [{Key, Value, false, _}] ->
      [Value];
    [] ->
      []
  end.

validate_external_resource(_Line, _File, Value) when is_binary(Value) ->
  Value;
validate_external_resource(Line, File, Value) ->
  elixir_errors:form_error([{line, Line}], File, ?MODULE, {invalid_external_resource, Value}).

%% Takes care of autoloading the module if configured.

autoload_module(Module, Binary, Opts, E) ->
  case proplists:get_value(autoload, Opts, true) of
    true  -> code:load_binary(Module, beam_location(E), Binary);
    false -> ok
  end.

beam_location(#{lexical_tracker := Pid, module := Module}) ->
  case elixir_lexical:dest(Pid) of
    nil  -> in_memory;
    Dest ->
      filename:join(elixir_utils:characters_to_list(Dest),
                    atom_to_list(Module) ++ ".beam")
  end.

%% Handle unused attributes warnings and special cases.

warn_unused_attributes(File, Data, PersistedAttrs) ->
  ReservedAttrs = [after_compile, before_compile, moduledoc, on_definition | PersistedAttrs],
  Keys = ets:select(Data, [{{'$1', '_', '_', '$2'}, [{is_atom, '$1'}, {is_integer, '$2'}], [['$1', '$2']]}]),
  [elixir_errors:form_warn([{line, Line}], File, ?MODULE, {unused_attribute, Key}) ||
   [Key, Line] <- Keys, not lists:member(Key, ReservedAttrs)].

%% Integration with elixir_compiler that makes the module available

make_module_available(Module, Binary) ->
  case get(elixir_module_binaries) of
    Current when is_list(Current) ->
      put(elixir_module_binaries, [{Module, Binary} | Current]),

      case get(elixir_compiler_pid) of
        undefined ->
          ok;
        PID ->
          Ref = make_ref(),
          PID ! {module_available, self(), Ref, get(elixir_compiler_file), Module, Binary},
          receive {Ref, ack} -> ok end
      end;
    _ ->
      ok
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

format_error({invalid_external_resource, Value}) ->
  io_lib:format("expected a string value for @external_resource, got: ~p",
    ['Elixir.Kernel':inspect(Value)]);
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
      Path when is_list(Path) ->
        io_lib:format(" (current version loaded from ~ts)", [elixir_utils:relative_to_cwd(Path)]);
      in_memory ->
        " (current version defined in memory)";
      _ ->
        ""
    end,
  io_lib:format("redefining module ~ts~ts", [elixir_aliases:inspect(Module), Extra]);
format_error({module_reserved, Module}) ->
  io_lib:format("module ~ts is reserved and cannot be defined", [elixir_aliases:inspect(Module)]);
format_error({module_in_definition, Module, File, Line}) ->
  io_lib:format("cannot define module ~ts because it is currently being defined in ~ts:~B",
    [elixir_aliases:inspect(Module), elixir_utils:relative_to_cwd(File), Line]).
