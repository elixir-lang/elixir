defmodule Protocol do
  @moduledoc """
  Functions for working with protocols.
  """

  @doc """
  Defines a new protocol function.

  Protocols do not allow functions to be defined directly, instead, the
  regular `Kernel.def/*` macros are replaced by this macro which
  defines the protocol functions with the appropriate callbacks.
  """
  defmacro def(signature)

  defmacro def({_, _, args}) when args == [] or is_atom(args) do
    raise ArgumentError, "protocol functions expect at least one argument"
  end

  defmacro def({name, _, args}) when is_atom(name) and is_list(args) do
    arity = length(args)

    type_args = :lists.map(fn _ -> quote(do: term) end, :lists.seq(2, arity))
    type_args = [quote(do: t) | type_args]

    varify = fn pos -> Macro.var(String.to_atom("arg" <> Integer.to_string(pos)), __MODULE__) end

    call_args = :lists.map(varify, :lists.seq(2, arity))
    call_args = [quote(do: term) | call_args]

    quote do
      name = unquote(name)
      arity = unquote(arity)

      @functions [{name, arity} | @functions]

      # Generate a fake definition with the user
      # signature that will be used by docs
      Kernel.def(unquote(name)(unquote_splicing(args)))

      # Generate the actual implementation
      Kernel.def unquote(name)(unquote_splicing(call_args)) do
        impl_for!(term).unquote(name)(unquote_splicing(call_args))
      end

      # Convert the spec to callback if possible,
      # otherwise generate a dummy callback
      Module.spec_to_callback(__MODULE__, {name, arity}) ||
        @callback unquote(name)(unquote_splicing(type_args)) :: term
    end
  end

  defmacro def(_) do
    raise ArgumentError, "invalid arguments for def inside defprotocol"
  end

  @doc """
  Checks if the given module is loaded and is protocol.

  Returns `:ok` if so, otherwise raises `ArgumentError`.
  """
  @spec assert_protocol!(module) :: :ok
  def assert_protocol!(module) do
    assert_protocol!(module, "")
  end

  defp assert_protocol!(module, extra) do
    case Code.ensure_compiled(module) do
      {:module, ^module} -> :ok
      _ -> raise ArgumentError, "#{inspect(module)} is not available" <> extra
    end

    try do
      module.__protocol__(:module)
    rescue
      UndefinedFunctionError ->
        raise ArgumentError, "#{inspect(module)} is not a protocol" <> extra
    end

    :ok
  end

  @doc """
  Checks if the given module is loaded and is an implementation
  of the given protocol.

  Returns `:ok` if so, otherwise raises `ArgumentError`.
  """
  @spec assert_impl!(module, module) :: :ok
  def assert_impl!(protocol, base) do
    assert_impl!(protocol, base, "")
  end

  defp assert_impl!(protocol, base, extra) do
    impl = Module.concat(protocol, base)

    case Code.ensure_compiled(impl) do
      {:module, ^impl} -> :ok
      _ -> raise ArgumentError, "#{inspect(impl)} is not available" <> extra
    end

    try do
      impl.__impl__(:protocol)
    rescue
      UndefinedFunctionError ->
        raise ArgumentError, "#{inspect(impl)} is not an implementation of a protocol" <> extra
    else
      ^protocol ->
        :ok

      other ->
        raise ArgumentError,
              "expected #{inspect(impl)} to be an implementation of #{inspect(protocol)}" <>
                ", got: #{inspect(other)}" <> extra
    end
  end

  @doc """
  Derives the `protocol` for `module` with the given options.

  If your implementation passes options or if you are generating
  custom code based on the struct, you will also need to implement
  a macro defined as `__deriving__(module, struct, options)`
  to get the options that were passed.

  ## Examples

      defprotocol Derivable do
        def ok(a)
      end

      defimpl Derivable, for: Any do
        defmacro __deriving__(module, struct, options) do
          quote do
            defimpl Derivable, for: unquote(module) do
              def ok(arg) do
                {:ok, arg, unquote(Macro.escape(struct)), unquote(options)}
              end
            end
          end
        end

        def ok(arg) do
          {:ok, arg}
        end
      end

      defmodule ImplStruct do
        @derive [Derivable]
        defstruct a: 0, b: 0

        defimpl Sample do
          def ok(struct) do
            Unknown.undefined(struct)
          end
        end
      end

  Explicit derivations can now be called via `__deriving__`:

      # Explicitly derived via `__deriving__`
      Derivable.ok(%ImplStruct{a: 1, b: 1})

      # Explicitly derived by API via `__deriving__`
      require Protocol
      Protocol.derive(Derivable, ImplStruct, :oops)
      Derivable.ok(%ImplStruct{a: 1, b: 1})

  """
  defmacro derive(protocol, module, options \\ []) do
    quote do
      Protocol.__derive__([{unquote(protocol), unquote(options)}], unquote(module), __ENV__)
    end
  end

  ## Consolidation

  @doc """
  Extracts all protocols from the given paths.

  The paths can be either a charlist or a string. Internally
  they are worked on as charlists, so passing them as lists
  avoid extra conversion.

  Does not load any of the protocols.

  ## Examples

      # Get Elixir's ebin directory path and retrieve all protocols
      iex> path = :code.lib_dir(:elixir, :ebin)
      iex> mods = Protocol.extract_protocols([path])
      iex> Enumerable in mods
      true

  """
  @spec extract_protocols([charlist | String.t()]) :: [atom]
  def extract_protocols(paths) do
    extract_matching_by_attribute(paths, 'Elixir.', fn module, attributes ->
      case attributes[:protocol] do
        [fallback_to_any: _] -> module
        _ -> nil
      end
    end)
  end

  @doc """
  Extracts all types implemented for the given protocol from
  the given paths.

  The paths can be either a charlist or a string. Internally
  they are worked on as charlists, so passing them as lists
  avoid extra conversion.

  Does not load any of the implementations.

  ## Examples

      # Get Elixir's ebin directory path and retrieve all protocols
      iex> path = :code.lib_dir(:elixir, :ebin)
      iex> mods = Protocol.extract_impls(Enumerable, [path])
      iex> List in mods
      true

  """
  @spec extract_impls(module, [charlist | String.t()]) :: [atom]
  def extract_impls(protocol, paths) when is_atom(protocol) do
    prefix = Atom.to_charlist(protocol) ++ '.'

    extract_matching_by_attribute(paths, prefix, fn _mod, attributes ->
      case attributes[:protocol_impl] do
        [protocol: ^protocol, for: for] -> for
        _ -> nil
      end
    end)
  end

  defp extract_matching_by_attribute(paths, prefix, callback) do
    for path <- paths,
        file <- list_dir(path),
        mod = extract_from_file(path, file, prefix, callback),
        do: mod
  end

  defp list_dir(path) when is_list(path) do
    case :file.list_dir(path) do
      {:ok, files} -> files
      _ -> []
    end
  end

  defp list_dir(path), do: list_dir(to_charlist(path))

  defp extract_from_file(path, file, prefix, callback) do
    if :lists.prefix(prefix, file) and :filename.extension(file) == '.beam' do
      extract_from_beam(:filename.join(path, file), callback)
    end
  end

  defp extract_from_beam(file, callback) do
    case :beam_lib.chunks(file, [:attributes]) do
      {:ok, {module, [attributes: attributes]}} ->
        callback.(module, attributes)

      _ ->
        nil
    end
  end

  @doc """
  Returns `true` if the protocol was consolidated.
  """
  @spec consolidated?(module) :: boolean
  def consolidated?(protocol) do
    protocol.__protocol__(:consolidated?)
  end

  @doc """
  Receives a protocol and a list of implementations and
  consolidates the given protocol.

  Consolidation happens by changing the protocol `impl_for`
  in the abstract format to have fast lookup rules. Usually
  the list of implementations to use during consolidation
  are retrieved with the help of `extract_impls/2`.

  It returns the updated version of the protocol bytecode.
  A given bytecode or protocol implementation can be checked
  to be consolidated or not by analyzing the protocol
  attribute:

      Protocol.consolidated?(Enumerable)

  If the first element of the tuple is `true`, it means
  the protocol was consolidated.

  This function does not load the protocol at any point
  nor loads the new bytecode for the compiled module.
  However each implementation must be available and
  it will be loaded.
  """
  @spec consolidate(module, [module]) ::
          {:ok, binary}
          | {:error, :not_a_protocol}
          | {:error, :no_beam_info}
  def consolidate(protocol, types) when is_atom(protocol) do
    with {:ok, ast_info, specs, compile_info} <- beam_protocol(protocol),
         {:ok, definitions} <- change_debug_info(protocol, ast_info, types),
         do: compile(definitions, specs, compile_info)
  end

  defp beam_protocol(protocol) do
    chunk_ids = [:debug_info, 'Docs', 'ExDp']
    opts = [:allow_missing_chunks]

    case :beam_lib.chunks(beam_file(protocol), chunk_ids, opts) do
      {:ok, {^protocol, [{:debug_info, debug_info} | chunks]}} ->
        {:debug_info_v1, _backend, {:elixir_v1, info, specs}} = debug_info
        %{attributes: attributes, definitions: definitions} = info
        chunks = :lists.map(fn {name, value} -> {List.to_string(name), value} end, chunks)

        case attributes[:protocol] do
          [fallback_to_any: any] ->
            {:ok, {any, definitions}, specs, {info, chunks}}

          _ ->
            {:error, :not_a_protocol}
        end

      _ ->
        {:error, :no_beam_info}
    end
  end

  defp beam_file(module) when is_atom(module) do
    case :code.which(module) do
      [_ | _] = file -> file
      _ -> module
    end
  end

  # Change the debug information to the optimized
  # impl_for/1 dispatch version.
  defp change_debug_info(protocol, {any, definitions}, types) do
    types = if any, do: types, else: List.delete(types, Any)
    all = [Any] ++ for {_guard, mod} <- __built_in__(), do: mod
    structs = types -- all

    case List.keytake(definitions, {:__protocol__, 1}, 0) do
      {protocol_def, definitions} ->
        {impl_for, definitions} = List.keytake(definitions, {:impl_for, 1}, 0)
        {struct_impl_for, definitions} = List.keytake(definitions, {:struct_impl_for, 1}, 0)

        protocol_def = change_protocol(protocol_def, types)
        impl_for = change_impl_for(impl_for, protocol, types)
        struct_impl_for = change_struct_impl_for(struct_impl_for, protocol, types, structs)

        {:ok, [protocol_def, impl_for, struct_impl_for] ++ definitions}

      nil ->
        {:error, :not_a_protocol}
    end
  end

  defp change_protocol({_name, _kind, meta, clauses}, types) do
    clauses =
      Enum.map(clauses, fn
        {meta, [:consolidated?], [], _} -> {meta, [:consolidated?], [], true}
        {meta, [:impls], [], _} -> {meta, [:impls], [], {:consolidated, types}}
        clause -> clause
      end)

    {{:__protocol__, 1}, :def, meta, clauses}
  end

  defp change_impl_for({_name, _kind, meta, _clauses}, protocol, types) do
    fallback = if Any in types, do: load_impl(protocol, Any)
    line = meta[:line]

    clauses =
      for {guard, mod} <- __built_in__(),
          mod in types,
          do: built_in_clause_for(mod, guard, protocol, meta, line)

    struct_clause = struct_clause_for(meta, line)
    fallback_clause = fallback_clause_for(fallback, protocol, meta)
    clauses = [struct_clause] ++ clauses ++ [fallback_clause]

    {{:impl_for, 1}, :def, meta, clauses}
  end

  defp change_struct_impl_for({_name, _kind, meta, _clauses}, protocol, types, structs) do
    fallback = if Any in types, do: load_impl(protocol, Any)
    clauses = for struct <- structs, do: each_struct_clause_for(struct, protocol, meta)
    clauses = clauses ++ [fallback_clause_for(fallback, protocol, meta)]

    {{:struct_impl_for, 1}, :defp, meta, clauses}
  end

  defp built_in_clause_for(mod, guard, protocol, meta, line) do
    x = quote(line: line, do: x)
    guard = quote(line: line, do: :erlang.unquote(guard)(unquote(x)))
    body = load_impl(protocol, mod)
    {meta, [x], [guard], body}
  end

  defp struct_clause_for(meta, line) do
    x = quote(line: line, do: x)
    head = quote(line: line, do: %{__struct__: unquote(x)})
    guard = quote(line: line, do: :erlang.is_atom(unquote(x)))
    body = quote(line: line, do: struct_impl_for(unquote(x)))
    {meta, [head], [guard], body}
  end

  defp each_struct_clause_for(struct, protocol, meta) do
    {meta, [struct], [], load_impl(protocol, struct)}
  end

  defp fallback_clause_for(value, _protocol, meta) do
    {meta, [quote(do: _)], [], value}
  end

  defp load_impl(protocol, for) do
    Module.concat(protocol, for).__impl__(:target)
  end

  # Finally compile the module and emit its bytecode.
  defp compile(definitions, specs, {info, chunks}) do
    info = %{info | definitions: definitions}
    {:ok, :elixir_erl.consolidate(info, specs, chunks)}
  end

  ## Definition callbacks

  @doc false
  def __protocol__(name, do: block) do
    quote do
      defmodule unquote(name) do
        # We don't allow function definition inside protocols
        import Kernel,
          except: [
            defmacrop: 1,
            defmacrop: 2,
            defmacro: 1,
            defmacro: 2,
            defp: 1,
            defp: 2,
            def: 1,
            def: 2
          ]

        # Import the new dsl that holds the new def
        import Protocol, only: [def: 1]

        # Compile with debug info for consolidation
        @compile :debug_info

        # Set up a clear slate to store defined functions
        @functions []
        @fallback_to_any false

        # Invoke the user given block
        _ = unquote(block)

        # Finalize expansion
        unquote(after_defprotocol())
      end
    end
  end

  defp after_defprotocol do
    quote bind_quoted: [built_in: __built_in__()] do
      any_impl_for =
        if @fallback_to_any do
          quote do: unquote(__MODULE__.Any).__impl__(:target)
        else
          nil
        end

      # Disable dialyzer checks - before and after consolidation
      # the types could be more strict
      @dialyzer {:nowarn_function, __protocol__: 1, impl_for: 1, impl_for!: 1}

      @doc false
      @spec impl_for(term) :: atom | nil
      Kernel.def(impl_for(data))

      # Define the implementation for structs.
      #
      # It simply delegates to struct_impl_for which is then
      # optimized during protocol consolidation.
      Kernel.def impl_for(%struct{}) do
        struct_impl_for(struct)
      end

      # Define the implementation for built-ins
      :lists.foreach(
        fn {guard, mod} ->
          target = Module.concat(__MODULE__, mod)

          Kernel.def impl_for(data) when :erlang.unquote(guard)(data) do
            case Code.ensure_compiled?(unquote(target)) and
                   function_exported?(unquote(target), :__impl__, 1) do
              true -> unquote(target).__impl__(:target)
              false -> unquote(any_impl_for)
            end
          end
        end,
        built_in
      )

      # Define a catch-all impl_for/1 clause to pacify Dialyzer (since
      # destructuring opaque types is illegal, Dialyzer will think none of the
      # previous clauses matches opaque types, and without this clause, will
      # conclude that impl_for can't handle an opaque argument). This is a hack
      # since it relies on Dialyzer not being smart enough to conclude that all
      # opaque types will get the any_impl_for/0 implementation.
      Kernel.def impl_for(_) do
        unquote(any_impl_for)
      end

      @doc false
      @spec impl_for!(term) :: atom
      if any_impl_for do
        Kernel.def impl_for!(data) do
          impl_for(data)
        end
      else
        Kernel.def impl_for!(data) do
          impl_for(data) || raise(Protocol.UndefinedError, protocol: __MODULE__, value: data)
        end
      end

      # Internal handler for Structs
      Kernel.defp struct_impl_for(struct) do
        target = Module.concat(__MODULE__, struct)

        case Code.ensure_compiled?(target) and function_exported?(target, :__impl__, 1) do
          true -> target.__impl__(:target)
          false -> unquote(any_impl_for)
        end
      end

      # Inline struct implementation for performance
      @compile {:inline, struct_impl_for: 1}

      unless Module.defines_type?(__MODULE__, {:t, 0}) do
        @type t :: term
      end

      # Store information as an attribute so it
      # can be read without loading the module.
      Module.register_attribute(__MODULE__, :protocol, persist: true)
      @protocol [fallback_to_any: !!@fallback_to_any]

      @doc false
      @spec __protocol__(:module) :: __MODULE__
      @spec __protocol__(:functions) :: unquote(Protocol.__functions_spec__(@functions))
      @spec __protocol__(:consolidated?) :: boolean
      @spec __protocol__(:impls) :: :not_consolidated | {:consolidated, [module]}
      Kernel.def(__protocol__(:module), do: __MODULE__)
      Kernel.def(__protocol__(:functions), do: unquote(:lists.sort(@functions)))
      Kernel.def(__protocol__(:consolidated?), do: false)
      Kernel.def(__protocol__(:impls), do: :not_consolidated)
    end
  end

  @doc false
  def __functions_spec__([]), do: []

  def __functions_spec__([head | tail]),
    do: [:lists.foldl(&{:|, [], [&1, &2]}, head, tail), quote(do: ...)]

  @doc false
  def __impl__(protocol, opts) do
    do_defimpl(protocol, :lists.keysort(1, opts))
  end

  defp do_defimpl(protocol, do: block, for: for) when is_list(for) do
    for f <- for, do: do_defimpl(protocol, do: block, for: f)
  end

  defp do_defimpl(protocol, do: block, for: for) do
    # Unquote the implementation just later
    # when all variables will already be injected
    # into the module body.
    impl =
      quote unquote: false do
        @doc false
        @spec __impl__(:for) :: unquote(for)
        @spec __impl__(:target) :: __MODULE__
        @spec __impl__(:protocol) :: unquote(protocol)
        def __impl__(:for), do: unquote(for)
        def __impl__(:target), do: __MODULE__
        def __impl__(:protocol), do: unquote(protocol)
      end

    quote do
      protocol = unquote(protocol)
      for = unquote(for)
      name = Module.concat(protocol, for)

      Protocol.assert_protocol!(protocol)
      Protocol.__ensure_defimpl__(protocol, for, __ENV__)

      defmodule name do
        @behaviour protocol
        @protocol protocol
        @for for

        unquote(block)

        Module.register_attribute(__MODULE__, :protocol_impl, persist: true)
        @protocol_impl [protocol: @protocol, for: @for]

        unquote(impl)
      end
    end
  end

  @doc false
  def __derive__(derives, for, %Macro.Env{} = env) when is_atom(for) do
    struct =
      if for == env.module do
        Module.get_attribute(for, :struct) || raise "struct is not defined for #{inspect(for)}"
      else
        for.__struct__
      end

    foreach = fn
      proto when is_atom(proto) ->
        derive(proto, for, struct, [], env)

      {proto, opts} when is_atom(proto) ->
        derive(proto, for, struct, opts, env)
    end

    :lists.foreach(foreach, :lists.flatten(derives))

    :ok
  end

  defp derive(protocol, for, struct, opts, env) do
    extra = ", cannot derive #{inspect(protocol)} for #{inspect(for)}"
    assert_protocol!(protocol, extra)
    __ensure_defimpl__(protocol, for, env)
    assert_impl!(protocol, Any, extra)

    # Clean up variables from eval context
    env = :elixir_env.reset_vars(env)
    args = [for, struct, opts]
    impl = Module.concat(protocol, Any)

    :elixir_module.expand_callback(env.line, impl, :__deriving__, args, env, fn mod, fun, args ->
      if function_exported?(mod, fun, length(args)) do
        apply(mod, fun, args)
      else
        quoted =
          quote do
            Module.register_attribute(__MODULE__, :protocol_impl, persist: true)
            @protocol_impl [protocol: unquote(protocol), for: unquote(for)]

            @doc false
            @spec __impl__(:target) :: unquote(impl)
            @spec __impl__(:protocol) :: unquote(protocol)
            @spec __impl__(:for) :: unquote(for)
            def __impl__(:target), do: unquote(impl)
            def __impl__(:protocol), do: unquote(protocol)
            def __impl__(:for), do: unquote(for)
          end

        Module.create(Module.concat(protocol, for), quoted, Macro.Env.location(env))
      end
    end)
  end

  @doc false
  def __ensure_defimpl__(protocol, for, env) do
    if Protocol.consolidated?(protocol) do
      message =
        "the #{inspect(protocol)} protocol has already been consolidated, an " <>
          "implementation for #{inspect(for)} has no effect. If you want to " <>
          "implement protocols after compilation or during tests, check the " <>
          "\"Consolidation\" section in the documentation for Kernel.defprotocol/2"

      :elixir_errors.warn(env.line, env.file, message)
    end

    :ok
  end

  ## Helpers

  @doc false
  def __built_in__ do
    [
      is_tuple: Tuple,
      is_atom: Atom,
      is_list: List,
      is_map: Map,
      is_bitstring: BitString,
      is_integer: Integer,
      is_float: Float,
      is_function: Function,
      is_pid: PID,
      is_port: Port,
      is_reference: Reference
    ]
  end
end
