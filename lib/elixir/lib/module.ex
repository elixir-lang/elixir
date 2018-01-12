defmodule Module do
  @moduledoc ~S'''
  Provides functions to deal with modules during compilation time.

  It allows a developer to dynamically add, delete and register
  attributes, attach documentation and so forth.

  After a module is compiled, using many of the functions in
  this module will raise errors, since it is out of their scope
  to inspect runtime data. Most of the runtime data can be inspected
  via the `__info__/1` function attached to each compiled module.

  ## Module attributes

  Each module can be decorated with one or more attributes. The following ones
  are currently defined by Elixir:

  ### `@after_compile`

  A hook that will be invoked right after the current module is compiled.
  Accepts a module or a `{module, function_name}`. See the "Compile callbacks"
  section below.

  ### `@before_compile`

  A hook that will be invoked before the module is compiled.
  Accepts a module or a `{module, function_or_macro_name}` tuple.
  See the "Compile callbacks" section below.

  ### `@behaviour` (notice the British spelling)

  Behaviours can be referenced by modules to ensure they implement
  required specific function signatures defined by `@callback`.

  For example, you could specify a `URI.Parser` behaviour as follows:

      defmodule URI.Parser do
        @doc "Defines a default port"
        @callback default_port() :: integer

        @doc "Parses the given URL"
        @callback parse(uri_info :: URI.t) :: URI.t
      end

  And then a module may use it as:

      defmodule URI.HTTP do
        @behaviour URI.Parser
        def default_port(), do: 80
        def parse(info), do: info
      end

  If the behaviour changes or `URI.HTTP` does not implement
  one of the callbacks, a warning will be raised.

  ### `@impl`

  To aid in the correct implementation of behaviours, you may optionally declare
  `@impl` for implemented callbacks of a behaviour. This makes callbacks
  explicit and can help you to catch errors in your code (the compiler will warn
  you if you mark a function as `@impl` when in fact it is not a callback, and
  vice versa). It also helps with maintainability by making it clear to other
  developers that the function's purpose is to implement a callback.

  Using `@impl` the example above can be rewritten as:

      defmodule URI.HTTP do
        @behaviour URI.parser

        @impl true
        def default_port(), do: 80

        @impl true
        def parse(info), do: info
      end

  You may pass either `false`, `true`, or a specific behaviour to `@impl`.

      defmodule Foo do
        @behaviour Bar
        @behaviour Baz

        @impl true # will warn if neither Bar nor Baz specify a callback named bar/0
        def bar(), do: :ok

        @impl Baz # will warn if Baz does not specify a callback named baz/0
        def baz(), do: :ok
      end

  ### `@compile`

  Defines options for module compilation. This is used to configure
  both Elixir and Erlang compilers, as any other compilation pass
  added by external tools. For example:

      defmodule MyModule do
        @compile {:inline, my_fun: 1}

        def my_fun(arg) do
          to_string(arg)
        end
      end

  Multiple uses of `@compile` will accumulate instead of overriding
  previous ones. See the "Compile options" section below.

  ### `@deprecated`

  Provides the deprecation reason for a function. For example:

      defmodule Keyword do
        @deprecated "Use Kernel.length/1 instead"
        def size(keyword) do
          length(keyword)
        end
      end

  The mix compiler automatically looks for calls to deprecated modules
  and emit warnings during compilation, computed via `mix xref warnings`.

  We recommend using this feature with care, especially library authors.
  Deprecating code always pushes the burden towards library users. We
  also recommend for deprecated functionality to be maintained for long
  periods of time, even after deprecation, giving developers plenty of
  time to update (except for cases where keeping the deprecated API is
  undesired, such as in the presence of security issues).

  ### `@doc` (and `@since`)

  Provides documentation for the function or macro that follows the
  attribute.

  Accepts a string (often a heredoc) or `false` where `@doc false` will
  make the function/macro invisible to documentation extraction tools
  like ExDoc. For example:

      defmodule MyModule do
        @doc "Hello world"
        @since "1.1.0"
        def hello do
          "world"
        end

        @doc """
        Sums `a` to `b`.
        """
        def sum(a, b) do
          a + b
        end
      end

  `@since` is an optional attribute that annotates which version the
  function was introduced.

  ### `@dialyzer`

  Defines warnings to request or suppress when using a version of
  `:dialyzer` that supports module attributes.

  Accepts an atom, a tuple, or a list of atoms and tuples. For example:

      defmodule MyModule do
        @dialyzer {:nowarn_function, my_fun: 1}

        def my_fun(arg) do
          M.not_a_function(arg)
        end
      end

  For the list of supported warnings, see
  [`:dialyzer` module](http://www.erlang.org/doc/man/dialyzer.html).

  Multiple uses of `@dialyzer` will accumulate instead of overriding
  previous ones.

  ### `@external_resource`

  Specifies an external resource for the current module.

  Sometimes a module embeds information from an external file. This
  attribute allows the module to annotate which external resources
  have been used.

  Tools like Mix may use this information to ensure the module is
  recompiled in case any of the external resources change.

  ### `@file`

  Changes the filename used in stacktraces for the function or macro that
  follows the attribute, such as:

      defmodule MyModule do
        @doc "Hello world"
        @file "hello.ex"
        def hello do
          "world"
        end
      end

  ### `@moduledoc`

  Provides documentation for the current module.

      defmodule MyModule do
        @moduledoc """
        A very useful module.
        """
      end

  Accepts a string (often a heredoc) or `false` where
  `@moduledoc false` will make the module invisible to
  documentation extraction tools like ExDoc.

  ### `@on_definition`

  A hook that will be invoked when each function or macro in the current
  module is defined. Useful when annotating functions.

  Accepts a module or a `{module, function_name}` tuple. See the
  "Compile callbacks" section below.

  ### `@on_load`

  A hook that will be invoked whenever the module is loaded.

  Accepts the function name (as an atom) of a function in the current module or
  `{function_name, 0}` tuple where `function_name` is the name of a function in
  the current module. The function must have arity 0 (no arguments) and has to
  return `:ok`, otherwise the loading of the module will be aborted. For
  example:

      defmodule MyModule do
        @on_load :load_check

        def load_check do
          if some_condition() do
            :ok
          else
            :abort
          end
        end

        def some_condition do
          false
        end
      end

  Modules compiled with HiPE would not call this hook.

  ### `@vsn`

  Specify the module version. Accepts any valid Elixir value, for example:

      defmodule MyModule do
        @vsn "1.0"
      end

  ### Typespec attributes

  The following attributes are part of typespecs and are also built-in in
  Elixir:

    * `@type` - defines a type to be used in `@spec`
    * `@typep` - defines a private type to be used in `@spec`
    * `@opaque` - defines an opaque type to be used in `@spec`
    * `@spec` - provides a specification for a function
    * `@callback` - provides a specification for a behaviour callback
    * `@macrocallback` - provides a specification for a macro behaviour callback
    * `@optional_callbacks` - specifies which behaviour callbacks and macro
      behaviour callbacks are optional
    * `@impl` - declares an implementation of a callback function or macro

  ### Custom attributes

  In addition to the built-in attributes outlined above, custom attributes may
  also be added. A custom attribute is any valid identifier prefixed with an
  `@` and followed by a valid Elixir value:

      defmodule MyModule do
        @custom_attr [some: "stuff"]
      end

  For more advanced options available when defining custom attributes, see
  `register_attribute/3`.

  ## Compile callbacks

  There are three callbacks that are invoked when functions are defined,
  as well as before and immediately after the module bytecode is generated.

  ### `@after_compile`

  A hook that will be invoked right after the current module is compiled.

  Accepts a module or a `{module, function_name}` tuple. The function
  must take two arguments: the module environment and its bytecode.
  When just a module is provided, the function is assumed to be
  `__after_compile__/2`.

  #### Example

      defmodule MyModule do
        @after_compile __MODULE__

        def __after_compile__(env, _bytecode) do
          IO.inspect env
        end
      end

  ### `@before_compile`

  A hook that will be invoked before the module is compiled.

  Accepts a module or a `{module, function_or_macro_name}` tuple. The
  function/macro must take one argument: the module environment. If it's a
  macro, its returned value will be injected at the end of the module definition
  before the compilation starts.

  When just a module is provided, the function/macro is assumed to be
  `__before_compile__/1`.

  *Note*: unlike `@after_compile`, the callback function/macro must
  be placed in a separate module (because when the callback is invoked,
  the current module does not yet exist).

  #### Example

      defmodule A do
        defmacro __before_compile__(_env) do
          quote do
            def hello, do: "world"
          end
        end
      end

      defmodule B do
        @before_compile A
      end

      B.hello()
      #=> "world"

  ### `@on_definition`

  A hook that will be invoked when each function or macro in the current
  module is defined. Useful when annotating functions.

  Accepts a module or a `{module, function_name}` tuple. The function
  must take 6 arguments:

    * the module environment
    * the kind of the function/macro: `:def`, `:defp`, `:defmacro`, or `:defmacrop`
    * the function/macro name
    * the list of quoted arguments
    * the list of quoted guards
    * the quoted function body

  Note the hook receives the quoted arguments and it is invoked before
  the function is stored in the module. So `Module.defines?/2` will return
  `false` for the first clause of every function.

  If the function/macro being defined has multiple clauses, the hook will
  be called for each clause.

  Unlike other hooks, `@on_definition` will only invoke functions and
  never macros. This is to avoid `@on_definition` callbacks from
  redefining functions that have just been defined in favor of more
  explicit approaches.

  When just a module is provided, the function is assumed to be
  `__on_definition__/6`.

  #### Example

      defmodule Hooks do
        def on_def(_env, kind, name, args, guards, body) do
          IO.puts "Defining #{kind} named #{name} with args:"
          IO.inspect args
          IO.puts "and guards"
          IO.inspect guards
          IO.puts "and body"
          IO.puts Macro.to_string(body)
        end
      end

      defmodule MyModule do
        @on_definition {Hooks, :on_def}

        def hello(arg) when is_binary(arg) or is_list(arg) do
          "Hello" <> to_string(arg)
        end

        def hello(_) do
          :ok
        end
      end

  ## Compile options

  The `@compile` attribute accepts different options that are used by both
  Elixir and Erlang compilers. Some of the common use cases are documented
  below:

    * `@compile :debug_info` - includes `:debug_info` regardless of the
      corresponding setting in `Code.compiler_options/1`

    * `@compile {:debug_info, false}` - disables `:debug_info` regardless
      of the corresponding setting in `Code.compiler_options/1`

    * `@compile {:inline, some_fun: 2, other_fun: 3}` - inlines the given
      name/arity pairs

    * `@compile {:autoload, false}` - disables automatic loading of
      modules after compilation. Instead, the module will be loaded after
      it is dispatched to

  You can see a handful more options used by the Erlang compiler in
  the documentation for the [`:compile` module](http://www.erlang.org/doc/man/compile.html).
  '''

  @typep definition :: {atom, arity}
  @typep def_kind :: :def | :defp | :defmacro | :defmacrop

  @doc """
  Provides runtime information about functions and macros defined by the
  module, etc.

  Each module gets an `__info__/1` function when it's compiled. The function
  takes one of the following atoms:

    * `:functions` - keyword list of public functions along with their arities

    * `:macros` - keyword list of public macros along with their arities

    * `:module` - the module atom name

    * `:md5` - the MD5 of the module

    * `:compile` - a list with compiler metadata

    * `:attributes` - a list with all persisted attributes

  """
  def __info__(kind)

  @doc """
  Checks if a module is open.

  A module is "open" if it is currently being defined and its attributes and
  functions can be modified.
  """
  @spec open?(module) :: boolean
  def open?(module) when is_atom(module) do
    :elixir_module.is_open(module)
  end

  @doc """
  Evaluates the quoted contents in the given module's context.

  A list of environment options can also be given as argument.
  See `Code.eval_string/3` for more information.

  Raises an error if the module was already compiled.

  ## Examples

      defmodule Foo do
        contents = quote do: (def sum(a, b), do: a + b)
        Module.eval_quoted __MODULE__, contents
      end

      Foo.sum(1, 2) #=> 3

  For convenience, you can pass any `Macro.Env` struct, such
  as  `__ENV__/0`, as the first argument or as options. Both
  the module and all options will be automatically extracted
  from the environment:

      defmodule Foo do
        contents = quote do: (def sum(a, b), do: a + b)
        Module.eval_quoted __ENV__, contents
      end

      Foo.sum(1, 2) #=> 3

  Note that if you pass a `Macro.Env` struct as first argument
  while also passing `opts`, they will be merged with `opts`
  having precedence.
  """
  @spec eval_quoted(module | Macro.Env.t(), Macro.t(), list, keyword | Macro.Env.t()) :: term
  def eval_quoted(module_or_env, quoted, binding \\ [], opts \\ [])

  def eval_quoted(%Macro.Env{} = env, quoted, binding, opts)
      when is_list(binding) and is_list(opts) do
    eval_quoted(env.module, quoted, binding, Keyword.merge(Map.to_list(env), opts))
  end

  def eval_quoted(module, quoted, binding, %Macro.Env{} = env)
      when is_atom(module) and is_list(binding) do
    eval_quoted(module, quoted, binding, Map.to_list(env))
  end

  def eval_quoted(module, quoted, binding, opts)
      when is_atom(module) and is_list(binding) and is_list(opts) do
    assert_not_compiled!(:eval_quoted, module)
    :elixir_def.reset_last(module)

    {value, binding, _env, _scope} =
      :elixir.eval_quoted(quoted, binding, Keyword.put(opts, :module, module))

    {value, binding}
  end

  @doc """
  Creates a module with the given name and defined by
  the given quoted expressions.

  The line where the module is defined and its file **must**
  be passed as options.

  It returns a tuple of shape `{:module, module, binary, term}`
  where `module` is the module name, `binary` is the module
  byte code and `term` is the result of the last expression in
  `quoted`.

  Similar to `Kernel.defmodule/2`, the binary will only be
  written to disk as a `.beam` file if `Module.create/3` is
  invoked in a file that is currently being compiled.

  ## Examples

      contents =
        quote do
          def world, do: true
        end

      Module.create(Hello, contents, Macro.Env.location(__ENV__))

      Hello.world #=> true

  ## Differences from `defmodule`

  `Module.create/3` works similarly to `Kernel.defmodule/2`
  and return the same results. While one could also use
  `defmodule` to define modules dynamically, this function
  is preferred when the module body is given by a quoted
  expression.

  Another important distinction is that `Module.create/3`
  allows you to control the environment variables used
  when defining the module, while `Kernel.defmodule/2`
  automatically uses the environment it is invoked at.
  """
  @spec create(module, Macro.t(), Macro.Env.t() | keyword) :: {:module, module, binary, term}
  def create(module, quoted, opts)

  def create(module, quoted, %Macro.Env{} = env) when is_atom(module) do
    create(module, quoted, Map.to_list(env))
  end

  def create(module, quoted, opts) when is_atom(module) and is_list(opts) do
    unless Keyword.has_key?(opts, :file) do
      raise ArgumentError, "expected :file to be given as option"
    end

    next = :erlang.unique_integer()
    line = Keyword.get(opts, :line, 0)
    quoted = :elixir_quote.linify_with_context_counter(line, {module, next}, quoted)
    :elixir_module.compile(module, quoted, [], :elixir.env_for_eval(opts))
  end

  @doc """
  Concatenates a list of aliases and returns a new alias.

  ## Examples

      iex> Module.concat([Foo, Bar])
      Foo.Bar

      iex> Module.concat([Foo, "Bar"])
      Foo.Bar

  """
  @spec concat([binary | atom]) :: atom
  def concat(list) when is_list(list) do
    :elixir_aliases.concat(list)
  end

  @doc """
  Concatenates two aliases and returns a new alias.

  ## Examples

      iex> Module.concat(Foo, Bar)
      Foo.Bar

      iex> Module.concat(Foo, "Bar")
      Foo.Bar

  """
  @spec concat(binary | atom, binary | atom) :: atom
  def concat(left, right)
      when (is_binary(left) or is_atom(left)) and (is_binary(right) or is_atom(right)) do
    :elixir_aliases.concat([left, right])
  end

  @doc """
  Concatenates a list of aliases and returns a new alias only if the alias
  was already referenced.

  If the alias was not referenced yet, fails with `ArgumentError`.
  It handles charlists, binaries and atoms.

  ## Examples

      iex> Module.safe_concat([Module, Unknown])
      ** (ArgumentError) argument error

      iex> Module.safe_concat([List, Chars])
      List.Chars

  """
  @spec safe_concat([binary | atom]) :: atom
  def safe_concat(list) when is_list(list) do
    :elixir_aliases.safe_concat(list)
  end

  @doc """
  Concatenates two aliases and returns a new alias only if the alias was
  already referenced.

  If the alias was not referenced yet, fails with `ArgumentError`.
  It handles charlists, binaries and atoms.

  ## Examples

      iex> Module.safe_concat(Module, Unknown)
      ** (ArgumentError) argument error

      iex> Module.safe_concat(List, Chars)
      List.Chars

  """
  @spec safe_concat(binary | atom, binary | atom) :: atom
  def safe_concat(left, right)
      when (is_binary(left) or is_atom(left)) and (is_binary(right) or is_atom(right)) do
    :elixir_aliases.safe_concat([left, right])
  end

  # Build signatures to be stored in docs

  defp build_signature(args, env) do
    {reverse_args, counters} = simplify_args(args, %{}, [], env)
    expand_vars(reverse_args, counters, [])
  end

  defp simplify_args([arg | args], counters, acc, env) do
    {arg, counters} = simplify_arg(arg, counters, env)
    simplify_args(args, counters, [arg | acc], env)
  end

  defp simplify_args([], counters, reverse_args, _env) do
    {reverse_args, counters}
  end

  defp simplify_arg({:\\, _, [left, right]}, acc, env) do
    {left, acc} = simplify_arg(left, acc, env)

    right =
      Macro.prewalk(right, fn
        {:@, _, _} = attr -> Macro.expand_once(attr, env)
        other -> other
      end)

    {{:\\, [], [left, right]}, acc}
  end

  # If the variable is being used explicitly for naming,
  # we always give it a higher priority (nil) even if it
  # starts with underscore.
  defp simplify_arg({:=, _, [{var, _, atom}, _]}, acc, _env) when is_atom(atom) do
    {simplify_var(var, nil), acc}
  end

  defp simplify_arg({:=, _, [_, {var, _, atom}]}, acc, _env) when is_atom(atom) do
    {simplify_var(var, nil), acc}
  end

  # If we have only the variable as argument, it also gets
  # higher priority. However, if the variable starts with an
  # underscore, we give it a secondary context (Elixir) with
  # lower priority.
  defp simplify_arg({var, _, atom}, acc, _env) when is_atom(atom) do
    {simplify_var(var, Elixir), acc}
  end

  defp simplify_arg({:%, _, [left, _]}, acc, env) do
    case Macro.expand_once(left, env) do
      module when is_atom(module) -> autogenerated(acc, simplify_module_name(module))
      _ -> autogenerated(acc, :struct)
    end
  end

  defp simplify_arg({:%{}, _, _}, acc, _env) do
    autogenerated(acc, :map)
  end

  defp simplify_arg({:@, _, _} = attr, acc, env) do
    simplify_arg(Macro.expand_once(attr, env), acc, env)
  end

  defp simplify_arg(other, acc, _env) when is_integer(other), do: autogenerated(acc, :int)
  defp simplify_arg(other, acc, _env) when is_boolean(other), do: autogenerated(acc, :bool)
  defp simplify_arg(other, acc, _env) when is_atom(other), do: autogenerated(acc, :atom)
  defp simplify_arg(other, acc, _env) when is_list(other), do: autogenerated(acc, :list)
  defp simplify_arg(other, acc, _env) when is_float(other), do: autogenerated(acc, :float)
  defp simplify_arg(other, acc, _env) when is_binary(other), do: autogenerated(acc, :binary)
  defp simplify_arg(_, acc, _env), do: autogenerated(acc, :arg)

  defp simplify_var(var, guess_priority) do
    case Atom.to_string(var) do
      "_" -> {:_, [], guess_priority}
      "_" <> rest -> {String.to_atom(rest), [], guess_priority}
      _ -> {var, [], nil}
    end
  end

  defp simplify_module_name(module) when is_atom(module) do
    try do
      split(module)
    rescue
      ArgumentError -> module
    else
      module_name -> String.to_atom(Macro.underscore(List.last(module_name)))
    end
  end

  defp autogenerated(acc, key) do
    case acc do
      %{^key => :once} -> {key, Map.put(acc, key, 2)}
      %{^key => value} -> {key, Map.put(acc, key, value + 1)}
      %{} -> {key, Map.put(acc, key, :once)}
    end
  end

  defp expand_vars([key | keys], counters, acc) when is_atom(key) do
    case counters do
      %{^key => count} when is_integer(count) and count >= 1 ->
        counters = Map.put(counters, key, count - 1)
        expand_vars(keys, counters, [{:"#{key}#{count}", [], Elixir} | acc])

      _ ->
        expand_vars(keys, counters, [{key, [], Elixir} | acc])
    end
  end

  defp expand_vars([arg | args], counters, acc) do
    expand_vars(args, counters, [arg | acc])
  end

  defp expand_vars([], _counters, acc) do
    acc
  end

  # Merge

  defp merge_signatures([h1 | t1], [h2 | t2], i) do
    [merge_signature(h1, h2, i) | merge_signatures(t1, t2, i + 1)]
  end

  defp merge_signatures([], [], _) do
    []
  end

  defp merge_signature({:\\, line, [left, right]}, newer, i) do
    {:\\, line, [merge_signature(left, newer, i), right]}
  end

  defp merge_signature(older, {:\\, _, [left, _]}, i) do
    merge_signature(older, left, i)
  end

  # The older signature, when given, always have higher precedence
  defp merge_signature({_, _, nil} = older, _newer, _), do: older
  defp merge_signature(_older, {_, _, nil} = newer, _), do: newer

  # Both are a guess, so check if they are the same guess
  defp merge_signature({var, _, _} = older, {var, _, _}, _), do: older

  # Otherwise, returns a generic guess
  defp merge_signature({_, line, _}, _newer, i), do: {:"arg#{i}", line, Elixir}

  @doc """
  Checks if the module defines the given function or macro.

  Use `defines?/3` to assert for a specific type.

  This function can only be used on modules that have not yet been compiled.
  Use `Kernel.function_exported?/3` to check compiled modules.

  ## Examples

      defmodule Example do
        Module.defines? __MODULE__, {:version, 0} #=> false
        def version, do: 1
        Module.defines? __MODULE__, {:version, 0} #=> true
      end

  """
  @spec defines?(module, definition) :: boolean
  def defines?(module, {function_or_macro_name, arity} = tuple)
      when is_atom(module) and is_atom(function_or_macro_name) and is_integer(arity) and
             arity >= 0 and arity <= 255 do
    assert_not_compiled!(:defines?, module)
    table = defs_table_for(module)
    :ets.lookup(table, {:def, tuple}) != []
  end

  @doc """
  Checks if the module defines a function or macro of the
  given `kind`.

  `kind` can be any of `:def`, `:defp`, `:defmacro`, or `:defmacrop`.

  This function can only be used on modules that have not yet been compiled.
  Use `Kernel.function_exported?/3` to check compiled modules.

  ## Examples

      defmodule Example do
        Module.defines? __MODULE__, {:version, 0}, :defp #=> false
        def version, do: 1
        Module.defines? __MODULE__, {:version, 0}, :defp #=> false
      end

  """
  @spec defines?(module, definition, def_kind) :: boolean
  def defines?(module, {function_macro_name, arity} = tuple, def_kind)
      when is_atom(module) and is_atom(function_macro_name) and is_integer(arity) and arity >= 0 and
             arity <= 255 and def_kind in [:def, :defp, :defmacro, :defmacrop] do
    assert_not_compiled!(:defines?, module)
    table = defs_table_for(module)

    case :ets.lookup(table, {:def, tuple}) do
      [{_, ^def_kind, _, _, _, _}] -> true
      _ -> false
    end
  end

  @doc """
  Returns all functions defined in `module`.

  ## Examples

      defmodule Example do
        def version, do: 1
        Module.definitions_in __MODULE__ #=> [{:version, 0}]
      end

  """
  @spec definitions_in(module) :: [definition]
  def definitions_in(module) when is_atom(module) do
    assert_not_compiled!(:definitions_in, module)
    table = defs_table_for(module)
    :lists.concat(:ets.match(table, {{:def, :"$1"}, :_, :_, :_, :_, :_}))
  end

  @doc """
  Returns all functions defined in `module`, according
  to its kind.

  ## Examples

      defmodule Example do
        def version, do: 1
        Module.definitions_in __MODULE__, :def  #=> [{:version, 0}]
        Module.definitions_in __MODULE__, :defp #=> []
      end

  """
  @spec definitions_in(module, def_kind) :: [definition]
  def definitions_in(module, def_kind)
      when is_atom(module) and def_kind in [:def, :defp, :defmacro, :defmacrop] do
    assert_not_compiled!(:definitions_in, module)
    table = defs_table_for(module)
    :lists.concat(:ets.match(table, {{:def, :"$1"}, def_kind, :_, :_, :_, :_}))
  end

  @doc """
  Makes the given functions in `module` overridable.

  An overridable function is lazily defined, allowing a
  developer to customize it. See `Kernel.defoverridable/1` for
  more information and documentation.
  """
  @spec make_overridable(module, [definition]) :: :ok
  def make_overridable(module, tuples) when is_atom(module) and is_list(tuples) do
    assert_not_compiled!(:make_overridable, module)
    check_impls_for_overridable(module, tuples)

    func = fn
      {function_name, arity} = tuple
      when is_atom(function_name) and is_integer(arity) and arity >= 0 and arity <= 255 ->
        case :elixir_def.take_definition(module, tuple) do
          false ->
            raise ArgumentError,
                  "cannot make function #{function_name}/#{arity} " <>
                    "overridable because it was not defined"

          clause ->
            neighbours = :elixir_locals.yank(tuple, module)
            overridable_definitions = :elixir_overridable.overridable(module)

            count =
              case :maps.find(tuple, overridable_definitions) do
                {:ok, {count, _, _, _}} -> count + 1
                :error -> 1
              end

            overridable_definitions =
              :maps.put(tuple, {count, clause, neighbours, false}, overridable_definitions)

            :elixir_overridable.overridable(module, overridable_definitions)
        end

      other ->
        raise ArgumentError,
              "each element in tuple list has to be a " <>
                "{function_name :: atom, arity :: 0..255} tuple, got: #{inspect(other)}"
    end

    :lists.foreach(func, tuples)
  end

  @spec make_overridable(module, module) :: :ok
  def make_overridable(module, behaviour) when is_atom(module) and is_atom(behaviour) do
    case check_module_for_overridable(module, behaviour) do
      :ok ->
        :ok

      {:error, error_explanation} ->
        raise ArgumentError,
              "cannot pass module #{inspect(behaviour)} as argument " <>
                "to defoverridable/1 because #{error_explanation}"
    end

    behaviour_callbacks =
      for callback <- behaviour_info(behaviour, :callbacks) do
        {pair, _kind} = normalize_macro_or_function_callback(callback)
        pair
      end

    tuples =
      for definition <- definitions_in(module), definition in behaviour_callbacks, do: definition

    make_overridable(module, tuples)
  end

  defp check_impls_for_overridable(module, tuples) do
    table = data_table_for(module)
    impls = :ets.lookup_element(table, {:elixir, :impls}, 2)

    {overridable_impls, impls} =
      :lists.splitwith(fn {pair, _, _, _, _, _} -> pair in tuples end, impls)

    if overridable_impls != [] do
      :ets.insert(table, {{:elixir, :impls}, impls})
      behaviours = :ets.lookup_element(table, :behaviour, 2)

      callbacks =
        for behaviour <- behaviours,
            function_exported?(behaviour, :behaviour_info, 1),
            callback <- behaviour_info(behaviour, :callbacks),
            {callback, kind} = normalize_macro_or_function_callback(callback),
            do: {callback, {kind, behaviour, true}},
            into: %{}

      check_impls(behaviours, callbacks, overridable_impls)
    end
  end

  defp check_module_for_overridable(module, behaviour) do
    behaviour_definitions = :ets.lookup_element(data_table_for(module), :behaviour, 2)

    cond do
      not Code.ensure_compiled?(behaviour) ->
        {:error, "it was not defined"}

      not function_exported?(behaviour, :behaviour_info, 1) ->
        {:error, "it does not define any callbacks"}

      behaviour not in behaviour_definitions ->
        error_message =
          "its corresponding behaviour is missing. Did you forget to " <>
            "add @behaviour #{inspect(behaviour)}?"

        {:error, error_message}

      true ->
        :ok
    end
  end

  defp normalize_macro_or_function_callback({function_name, arity}) do
    case :erlang.atom_to_list(function_name) do
      # Macros are always provided one extra argument in behaviour_info/1
      'MACRO-' ++ tail ->
        {{:erlang.list_to_atom(tail), arity - 1}, :defmacro}

      _ ->
        {{function_name, arity}, :def}
    end
  end

  defp behaviour_info(module, key) do
    case module.behaviour_info(key) do
      list when is_list(list) -> list
      :undefined -> []
    end
  end

  @doc """
  Returns `true` if `tuple` in `module` is marked as overridable.
  """
  @spec overridable?(module, definition) :: boolean
  def overridable?(module, {function_name, arity} = tuple)
      when is_atom(function_name) and is_integer(arity) and arity >= 0 and arity <= 255 do
    :maps.is_key(tuple, :elixir_overridable.overridable(module))
  end

  @doc """
  Puts a module attribute with `key` and `value` in the given `module`.

  ## Examples

      defmodule MyModule do
        Module.put_attribute __MODULE__, :custom_threshold_for_lib, 10
      end

  """
  @spec put_attribute(module, atom, term) :: :ok
  def put_attribute(module, key, value) when is_atom(module) and is_atom(key) do
    put_attribute(module, key, value, nil, nil)
  end

  @doc """
  Gets the given attribute from a module.

  If the attribute was marked with `accumulate` with
  `Module.register_attribute/3`, a list is always returned.
  `nil` is returned if the attribute has not been marked with
  `accumulate` and has not been set to any value.

  The `@` macro compiles to a call to this function. For example,
  the following code:

      @foo

  Expands to something akin to:

      Module.get_attribute(__MODULE__, :foo)

  ## Examples

      defmodule Foo do
        Module.put_attribute __MODULE__, :value, 1
        Module.get_attribute __MODULE__, :value #=> 1

        Module.register_attribute __MODULE__, :value, accumulate: true
        Module.put_attribute __MODULE__, :value, 1
        Module.get_attribute __MODULE__, :value #=> [1]
      end

  """
  @spec get_attribute(module, atom) :: term
  def get_attribute(module, key) when is_atom(module) and is_atom(key) do
    get_attribute(module, key, nil)
  end

  @doc """
  Deletes the module attribute that matches the given key.

  It returns the deleted attribute value (or `nil` if nothing was set).

  ## Examples

      defmodule MyModule do
        Module.put_attribute __MODULE__, :custom_threshold_for_lib, 10
        Module.delete_attribute __MODULE__, :custom_threshold_for_lib
      end

  """
  @spec delete_attribute(module, atom) :: term
  def delete_attribute(module, key) when is_atom(module) and is_atom(key) do
    assert_not_compiled!(:delete_attribute, module)
    table = data_table_for(module)

    case :ets.take(table, key) do
      [{_, value, _accumulated? = true, _}] ->
        :ets.insert(table, {key, [], true, nil})
        value

      [{_, value, _, _}] ->
        value

      [] ->
        nil
    end
  end

  @doc """
  Registers an attribute.

  By registering an attribute, a developer is able to customize
  how Elixir will store and accumulate the attribute values.

  ## Options

  When registering an attribute, two options can be given:

    * `:accumulate` - several calls to the same attribute will
      accumulate instead of override the previous one. New attributes
      are always added to the top of the accumulated list.

    * `:persist` - the attribute will be persisted in the Erlang
      Abstract Format. Useful when interfacing with Erlang libraries.

  By default, both options are `false`.

  ## Examples

      defmodule MyModule do
        Module.register_attribute __MODULE__,
          :custom_threshold_for_lib,
          accumulate: true, persist: false

        @custom_threshold_for_lib 10
        @custom_threshold_for_lib 20
        @custom_threshold_for_lib #=> [20, 10]
      end

  """
  @spec register_attribute(module, atom, [{:accumulate, boolean}, {:persist, boolean}]) :: :ok
  def register_attribute(module, attribute, options)
      when is_atom(module) and is_atom(attribute) and is_list(options) do
    assert_not_compiled!(:register_attribute, module)
    table = data_table_for(module)

    if Keyword.get(options, :persist) do
      attributes = :ets.lookup_element(table, {:elixir, :persisted_attributes}, 2)
      :ets.insert(table, {{:elixir, :persisted_attributes}, [attribute | attributes]})
    end

    if Keyword.get(options, :accumulate) do
      :ets.insert_new(table, {attribute, [], _accumulated? = true, _unread_line = nil}) ||
        :ets.update_element(table, attribute, {3, true})
    end

    :ok
  end

  @doc """
  Splits the given module name into binary parts.

  `module` has to be an Elixir module, as `split/1` won't work with Erlang-style
  modules (for example, `split(:lists)` raises an error).

  `split/1` also supports splitting the string representation of Elixir modules
  (that is, the result of calling `Atom.to_string/1` with the module name).

  ## Examples

      iex> Module.split(Very.Long.Module.Name.And.Even.Longer)
      ["Very", "Long", "Module", "Name", "And", "Even", "Longer"]
      iex> Module.split("Elixir.String.Chars")
      ["String", "Chars"]

  """
  @spec split(module | String.t()) :: [String.t(), ...]
  def split(module)

  def split(module) when is_atom(module) do
    split(Atom.to_string(module), _original = module)
  end

  def split(module) when is_binary(module) do
    split(module, _original = module)
  end

  defp split("Elixir." <> name, _original) do
    String.split(name, ".")
  end

  defp split(_module, original) do
    raise ArgumentError, "expected an Elixir module, got: #{inspect(original)}"
  end

  @doc false
  # Used internally to compile documentation.
  # This function is private and must be used only internally.
  def compile_definition_attributes(env, kind, name, args, _guards, _body) do
    module = env.module
    table = data_table_for(module)
    arity = length(args)
    pair = {name, arity}

    impl = compile_impl(table, name, env, kind, args)
    compile_doc(table, pair, env, kind, args, impl)
    compile_deprecated(table, pair)

    :ok
  end

  defp compile_doc(table, pair, env, kind, args, impl) do
    {line, doc} = get_doc_info(table, env, impl)

    # TODO: Store @since alongside the docs
    _ = get_since_info(table)

    add_doc(table, line, kind, pair, args, doc, env)
  end

  defp add_doc(_table, line, kind, {name, arity}, _args, doc, env)
       when kind in [:defp, :defmacrop] do
    if doc do
      error_message =
        "#{kind} #{name}/#{arity} is private, " <>
          "@doc attribute is always discarded for private functions/macros/types"

      :elixir_errors.warn(line, env.file, error_message)
    end
  end

  defp add_doc(table, line, kind, pair, args, doc, env) do
    signature = build_signature(args, env)

    case :ets.lookup(table, {:doc, pair}) do
      [] ->
        :ets.insert(table, {{:doc, pair}, line, kind, signature, doc})

      [{doc_tuple, line, _current_kind, current_sign, current_doc}] ->
        signature = merge_signatures(current_sign, signature, 1)
        doc = if is_nil(doc), do: current_doc, else: doc
        :ets.insert(table, {doc_tuple, line, kind, signature, doc})
    end
  end

  defp compile_deprecated(table, pair) do
    if reason = get_deprecated_info(table) do
      :ets.insert(table, {{:deprecated, pair}, reason})
    end
  end

  defp compile_impl(table, name, env, kind, args) do
    %{line: line, file: file} = env

    case :ets.take(table, :impl) do
      [{:impl, value, _, _}] ->
        impls = :ets.lookup_element(table, {:elixir, :impls}, 2)
        {total, defaults} = args_count(args, 0, 0)
        impl = {{name, total}, defaults, kind, line, file, value}
        :ets.insert(table, {{:elixir, :impls}, [impl | impls]})
        value

      [] ->
        false
    end
  end

  defp args_count([{:\\, _, _} | tail], total, defaults) do
    args_count(tail, total + 1, defaults + 1)
  end

  defp args_count([_head | tail], total, defaults) do
    args_count(tail, total + 1, defaults)
  end

  defp args_count([], total, defaults), do: {total, defaults}

  @doc false
  def check_behaviours_and_impls(env, table, all_definitions, overridable_pairs) do
    behaviours = :ets.lookup_element(table, :behaviour, 2)
    impls = :ets.lookup_element(table, {:elixir, :impls}, 2)
    callbacks = check_behaviours(env, behaviours)

    pending_callbacks =
      if impls != [] do
        non_implemented_callbacks = check_impls(behaviours, callbacks, impls)
        warn_missing_impls(env, non_implemented_callbacks, all_definitions, overridable_pairs)
        non_implemented_callbacks
      else
        callbacks
      end

    check_callbacks(env, pending_callbacks, all_definitions)
    :ok
  end

  defp check_behaviours(%{lexical_tracker: pid} = env, behaviours) do
    Enum.reduce(behaviours, %{}, fn behaviour, acc ->
      cond do
        not is_atom(behaviour) ->
          message =
            "@behaviour #{inspect(behaviour)} must be an atom (in module #{inspect(env.module)})"

          :elixir_errors.warn(env.line, env.file, message)
          acc

        not Code.ensure_compiled?(behaviour) ->
          message =
            "module #{inspect(behaviour)} does not exist (in module #{inspect(env.module)})"

          unless standard_behaviour?(behaviour) do
            :elixir_errors.warn(env.line, env.file, message)
          end

          acc

        not function_exported?(behaviour, :behaviour_info, 1) ->
          message =
            "module #{inspect(behaviour)} is not a behaviour (in module #{inspect(env.module)})"

          unless standard_behaviour?(behaviour) do
            :elixir_errors.warn(env.line, env.file, message)
          end

          acc

        true ->
          :elixir_lexical.record_remote(behaviour, nil, pid)
          optional_callbacks = behaviour_info(behaviour, :optional_callbacks)
          callbacks = behaviour_info(behaviour, :callbacks)
          Enum.reduce(callbacks, acc, &add_callback(&1, behaviour, env, optional_callbacks, &2))
      end
    end)
  end

  defp add_callback(original, behaviour, env, optional_callbacks, acc) do
    {callback, kind} = normalize_macro_or_function_callback(original)

    case acc do
      %{^callback => {_kind, conflict, _optional?}} ->
        message =
          "conflicting behaviours found. #{format_definition(kind, callback)} is required by " <>
            "#{inspect(behaviour)} and #{inspect(conflict)} (in module #{inspect(env.module)})"

        :elixir_errors.warn(env.line, env.file, message)

      %{} ->
        :ok
    end

    Map.put(acc, callback, {kind, behaviour, original in optional_callbacks})
  end

  defp standard_behaviour?(behaviour) do
    behaviour in [
      Collectable,
      Enumerable,
      Inspect,
      List.Chars,
      String.Chars
    ]
  end

  defp check_callbacks(env, callbacks, all_definitions) do
    for {callback, {kind, behaviour, optional?}} <- callbacks do
      case :lists.keyfind(callback, 1, all_definitions) do
        false when not optional? ->
          message =
            format_callback(callback, kind, behaviour) <>
              " is not implemented (in module #{inspect(env.module)})"

          :elixir_errors.warn(env.line, env.file, message)

        {_, wrong_kind, _, _} when kind != wrong_kind ->
          message =
            format_callback(callback, kind, behaviour) <>
              " was implemented as \"#{wrong_kind}\" but should have been \"#{kind}\" " <>
              "(in module #{inspect(env.module)})"

          :elixir_errors.warn(env.line, env.file, message)

        _ ->
          :ok
      end
    end

    :ok
  end

  defp format_callback(callback, kind, module) do
    protocol_or_behaviour = if protocol?(module), do: "protocol ", else: "behaviour "

    format_definition(kind, callback) <>
      " required by " <> protocol_or_behaviour <> inspect(module)
  end

  defp protocol?(module) do
    Code.ensure_loaded?(module) and function_exported?(module, :__protocol__, 1) and
      module.__protocol__(:module) == module
  end

  defp check_impls(behaviours, callbacks, impls) do
    Enum.reduce(impls, callbacks, fn {fa, defaults, kind, line, file, value}, acc ->
      case impl_behaviours(fa, defaults, kind, value, behaviours, callbacks) do
        {:ok, impl_behaviours} ->
          Enum.reduce(impl_behaviours, acc, fn {fa, _}, acc -> Map.delete(acc, fa) end)

        {:error, message} ->
          :elixir_errors.warn(line, file, format_impl_warning(fa, kind, message))
          acc
      end
    end)
  end

  defp impl_behaviours({function, arity}, defaults, kind, value, behaviours, callbacks) do
    impls = for n <- arity..(arity - defaults), do: {function, n}
    impl_behaviours(impls, kind, value, behaviours, callbacks)
  end

  defp impl_behaviours(_, kind, _, _, _) when kind in [:defp, :defmacrop] do
    {:error, :private_function}
  end

  defp impl_behaviours(_, _, value, [], _) do
    {:error, {:no_behaviours, value}}
  end

  defp impl_behaviours(impls, _, false, _, callbacks) do
    case impl_callbacks(impls, callbacks) do
      [] -> {:ok, []}
      [impl | _] -> {:error, {:impl_not_defined, impl}}
    end
  end

  defp impl_behaviours(impls, _, true, _, callbacks) do
    case impl_callbacks(impls, callbacks) do
      [] -> {:error, {:impl_defined, callbacks}}
      impls -> {:ok, impls}
    end
  end

  defp impl_behaviours(impls, _, behaviour, behaviours, callbacks) do
    filtered = impl_behaviours(impls, behaviour, callbacks)

    cond do
      filtered != [] ->
        {:ok, filtered}

      behaviour not in behaviours ->
        {:error, {:behaviour_not_declared, behaviour}}

      true ->
        {:error, {:behaviour_not_defined, behaviour, callbacks}}
    end
  end

  defp impl_behaviours(impls, behaviour, callbacks) do
    impl_behaviours(impl_callbacks(impls, callbacks), behaviour)
  end

  defp impl_behaviours([], _) do
    []
  end

  defp impl_behaviours([{_, behaviour} = impl | tail], behaviour) do
    [impl | impl_behaviours(tail, behaviour)]
  end

  defp impl_behaviours([_ | tail], behaviour) do
    impl_behaviours(tail, behaviour)
  end

  defp impl_callbacks([], _) do
    []
  end

  defp impl_callbacks([fa | tail], callbacks) do
    case callbacks[fa] do
      nil -> impl_callbacks(tail, callbacks)
      {_, behaviour, _} -> [{fa, behaviour} | impl_callbacks(tail, callbacks)]
    end
  end

  defp format_impl_warning(fa, kind, :private_function) do
    "#{format_definition(kind, fa)} is private, @impl attribute is always discarded for private functions/macros"
  end

  defp format_impl_warning(fa, kind, {:no_behaviours, value}) do
    "got \"@impl #{inspect(value)}\" for #{format_definition(kind, fa)} but no behaviour was declared"
  end

  defp format_impl_warning(_, kind, {:impl_not_defined, {fa, behaviour}}) do
    "got \"@impl false\" for #{format_definition(kind, fa)} " <>
      "but it is a callback specified in #{inspect(behaviour)}"
  end

  defp format_impl_warning(fa, kind, {:impl_defined, callbacks}) do
    "got \"@impl true\" for #{format_definition(kind, fa)} " <>
      "but no behaviour specifies such callback#{known_callbacks(callbacks)}"
  end

  defp format_impl_warning(fa, kind, {:behaviour_not_declared, behaviour}) do
    "got \"@impl #{inspect(behaviour)}\" for #{format_definition(kind, fa)} " <>
      "but this behaviour was not declared with @behaviour"
  end

  defp format_impl_warning(fa, kind, {:behaviour_not_defined, behaviour, callbacks}) do
    "got \"@impl #{inspect(behaviour)}\" for #{format_definition(kind, fa)} " <>
      "but this behaviour does not specify such callback#{known_callbacks(callbacks)}"
  end

  defp warn_missing_impls(_env, callbacks, _defs, _) when map_size(callbacks) == 0 do
    :ok
  end

  defp warn_missing_impls(env, non_implemented_callbacks, defs, overridable_pairs) do
    for {pair, kind, meta, _clauses} <- defs,
        kind in [:def, :defmacro],
        pair not in overridable_pairs do
      case Map.fetch(non_implemented_callbacks, pair) do
        {:ok, {_, behaviour, _}} ->
          message =
            "module attribute @impl was not set for #{format_definition(kind, pair)} " <>
              "callback (specified in #{inspect(behaviour)}). " <>
              "This either means you forgot to add the \"@impl true\" annotation before the " <>
              "definition or that you are accidentally overriding this callback"

          :elixir_errors.warn(:elixir_utils.get_line(meta), env.file, message)

        :error ->
          :ok
      end
    end

    :ok
  end

  defp format_definition(kind, {name, arity}) do
    format_definition(kind) <> " #{name}/#{arity}"
  end

  defp format_definition(:defmacro), do: "macro"
  defp format_definition(:defmacrop), do: "macro"
  defp format_definition(:def), do: "function"
  defp format_definition(:defp), do: "function"

  defp known_callbacks(callbacks) when map_size(callbacks) == 0 do
    ". There are no known callbacks, please specify the proper @behaviour " <>
      "and make sure it defines callbacks"
  end

  defp known_callbacks(callbacks) do
    formatted_callbacks =
      for {{name, arity}, {kind, module, _}} <- callbacks do
        "\n  * " <> Exception.format_mfa(module, name, arity) <> " (#{format_definition(kind)})"
      end

    ". The known callbacks are:\n#{formatted_callbacks}\n"
  end

  @doc false
  # Used internally to compile types.
  # This function is private and must be used only internally.
  def store_typespec(module, key, value) when is_atom(module) and is_atom(key) do
    assert_not_compiled!(:put_attribute, module)
    table = data_table_for(module)

    typespecs =
      case :ets.lookup(table, key) do
        [{^key, typespecs, _, _}] -> [value | typespecs]
        [] -> [value]
      end

    :ets.insert(table, {key, typespecs, true, nil})
  end

  @doc false
  # Used internally by Kernel's @.
  # This function is private and must be used only internally.
  def get_attribute(module, key, stack) when is_atom(key) do
    assert_not_compiled!(:get_attribute, module)
    table = data_table_for(module)

    case :ets.lookup(table, key) do
      [{^key, val, _, _}] ->
        :ets.update_element(table, key, {4, nil})
        val

      [] when is_list(stack) ->
        # TODO: Consider raising instead of warning on v2.0 as it usually cascades
        error_message =
          "undefined module attribute @#{key}, " <>
            "please remove access to @#{key} or explicitly set it before access"

        IO.warn(error_message, stack)

        nil

      [] ->
        nil
    end
  end

  @doc false
  # Used internally by Kernel's @.
  # This function is private and must be used only internally.
  def put_attribute(module, key, value, stack, unread_line) when is_atom(key) do
    assert_not_compiled!(:put_attribute, module)
    table = data_table_for(module)
    value = preprocess_attribute(key, value)

    # TODO: Remove on Elixir v2.0
    case value do
      {:parse_transform, _} when key == :compile and is_list(stack) ->
        error_message =
          "@compile {:parse_transform, _} is deprecated. " <>
            "Elixir will no longer support Erlang-based transforms in future versions"

        IO.warn(error_message, stack)

      _ ->
        :ok
    end

    case :ets.lookup(table, key) do
      [{^key, {line, <<_::binary>>}, accumulated?, _unread_line}]
      when key in [:doc, :typedoc, :moduledoc] and is_list(stack) ->
        IO.warn("redefining @#{key} attribute previously set at line #{line}", stack)
        :ets.insert(table, {key, value, accumulated?, unread_line})

      [{^key, current, _accumulated? = true, _read?}] ->
        :ets.insert(table, {key, [value | current], true, unread_line})

      _ ->
        :ets.insert(table, {key, value, false, unread_line})
    end

    :ok
  end

  ## Helpers

  defp preprocess_attribute(key, value) when key in [:moduledoc, :typedoc, :doc] do
    case value do
      {line, doc} when is_integer(line) and (is_binary(doc) or is_boolean(doc) or is_nil(doc)) ->
        value

      {line, doc} when is_integer(line) ->
        raise ArgumentError,
              "@#{key} is a built-in module attribute for documentation. It should be " <>
                "a binary, a boolean, or nil, got: #{inspect(doc)}"

      _other ->
        raise ArgumentError,
              "@#{key} is a built-in module attribute for documentation. When set dynamically, " <>
                "it should be {line, doc} (where \"doc\" is a string, boolean, or nil), " <>
                "got: #{inspect(value)}"
    end
  end

  defp preprocess_attribute(:on_load, value) do
    case value do
      _ when is_atom(value) ->
        {value, 0}

      {atom, 0} = tuple when is_atom(atom) ->
        tuple

      _ ->
        raise ArgumentError,
              "@on_load is a built-in module attribute that annotates a function to be invoked " <>
                "when the module is loaded. It should be an atom or a {atom, 0} tuple, " <>
                "got: #{inspect(value)}"
    end
  end

  defp preprocess_attribute(:impl, value) do
    case value do
      _ when is_boolean(value) ->
        value

      module when is_atom(module) and module != nil ->
        # Attempt to compile behaviour but ignore failure (will warn later)
        _ = Code.ensure_compiled(module)
        value

      _ ->
        raise ArgumentError,
              "@impl is a built-in module attribute that marks the next definition " <>
                "as a callback implementation. It should be a module or a boolean, " <>
                "got: #{inspect(value)}"
    end
  end

  defp preprocess_attribute(:before_compile, atom) when is_atom(atom),
    do: {atom, :__before_compile__}

  defp preprocess_attribute(:after_compile, atom) when is_atom(atom),
    do: {atom, :__after_compile__}

  defp preprocess_attribute(:on_definition, atom) when is_atom(atom),
    do: {atom, :__on_definition__}

  defp preprocess_attribute(key, _value)
       when key in [:type, :typep, :export_type, :opaque, :callback, :macrocallback] do
    raise ArgumentError,
          "attributes type, typep, export_type, opaque, callback, and macrocallback" <>
            "must be set directly via the @ notation"
  end

  defp preprocess_attribute(:since, value) when not is_binary(value) do
    raise ArgumentError,
          "@since is a built-in module attribute used for documentation purposes. " <>
            "It should be a string representing the version a function, macro, type or " <>
            "callback was added, got: #{inspect(value)}"
  end

  defp preprocess_attribute(:deprecated, value) when not is_binary(value) do
    raise ArgumentError,
          "@deprecated is a built-in module attribute that annotates a definition as deprecated. " <>
            "It should be a string with the reason for the deprecation, got: #{inspect(value)}"
  end

  defp preprocess_attribute(:file, value) do
    case value do
      _ when is_binary(value) ->
        value

      {file, line} when is_binary(file) and is_integer(line) ->
        value

      _ ->
        raise ArgumentError,
              "@file is a built-in module attribute that annotates the file and line the next " <>
                "definition comes from. It should be a string or a {string, line} tuple as value, " <>
                "got: #{inspect(value)}"
    end
  end

  defp preprocess_attribute(_key, value) do
    value
  end

  defp get_doc_info(table, env, impl) do
    case :ets.take(table, :doc) do
      [{:doc, {_, _} = pair, _, _}] ->
        pair

      [] when impl == false ->
        {env.line, nil}

      [] ->
        {env.line, false}
    end
  end

  defp get_since_info(table) do
    :ets.take(table, :since)
  end

  defp get_deprecated_info(table) do
    case :ets.take(table, :deprecated) do
      [{:deprecated, reason, _, _}] -> reason
      [] -> nil
    end
  end

  defp data_table_for(module) do
    :elixir_module.data_table(module)
  end

  defp defs_table_for(module) do
    :elixir_module.defs_table(module)
  end

  defp assert_not_compiled!(fun, module) do
    open?(module) ||
      raise ArgumentError,
            "could not call #{fun} with argument #{inspect(module)} because the module is already compiled"
  end
end
