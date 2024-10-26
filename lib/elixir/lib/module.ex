defmodule Module do
  @moduledoc ~S'''
  Provides functions to deal with modules during compilation time.

  It allows a developer to dynamically add, delete and register
  attributes, attach documentation and so forth.

  After a module is compiled, using many of the functions in
  this module will raise errors, since it is out of their scope
  to inspect runtime data. Most of the runtime data can be inspected
  via the [`__info__/1`](`c:Module.__info__/1`) function attached to
  each compiled module.

  ## Module attributes

  Each module can be decorated with one or more attributes. The following ones
  are currently defined by Elixir:

  ### `@after_compile`

  A hook that will be invoked right after the current module is compiled.
  Accepts a module or a `{module, function_name}`. See the "Compile callbacks"
  section below.

  ### `@after_verify` (since v1.14.0)

  A hook that will be invoked right after the current module is verified for
  undefined functions, deprecations, etc. Accepts a module or a `{module, function_name}`.
  See the "Compile callbacks" section below.

  ### `@before_compile`

  A hook that will be invoked before the module is compiled.
  Accepts a module or a `{module, function_or_macro_name}` tuple.
  See the "Compile callbacks" section below.

  ### `@behaviour`

  Note the British spelling!

  Behaviours can be referenced by modules to ensure they implement
  required specific function signatures defined by `@callback`.

  For example, you could specify a `URI.Parser` behaviour as follows:

      defmodule URI.Parser do
        @doc "Defines a default port"
        @callback default_port() :: integer

        @doc "Parses the given URL"
        @callback parse(uri_info :: URI.t()) :: URI.t()
      end

  And then a module may use it as:

      defmodule URI.HTTP do
        @behaviour URI.Parser
        def default_port(), do: 80
        def parse(info), do: info
      end

  If the behaviour changes or `URI.HTTP` does not implement
  one of the callbacks, a warning will be raised.

  For detailed documentation, see the
  [behaviour typespec documentation](typespecs.md#behaviours).

  ### `@impl` (since v1.5.0)

  To aid in the correct implementation of behaviours, you may optionally declare
  `@impl` for implemented callbacks of a behaviour. This makes callbacks
  explicit and can help you to catch errors in your code. The compiler will warn
  in these cases:

    * if you mark a function with `@impl` when that function is not a callback.

    * if you don't mark a function with `@impl` when other functions are marked
      with `@impl`. If you mark one function with `@impl`, you must mark all
      other callbacks for that behaviour as `@impl`.

  `@impl` works on a per-context basis. If you generate a function through a macro
  and mark it with `@impl`, that won't affect the module where that function is
  generated in.

  `@impl` also helps with maintainability by making it clear to other developers
  that the function is implementing a callback.

  Using `@impl`, the example above can be rewritten as:

      defmodule URI.HTTP do
        @behaviour URI.Parser

        @impl true
        def default_port(), do: 80

        @impl true
        def parse(info), do: info
      end

  You may pass either `false`, `true`, or a specific behaviour to `@impl`.

      defmodule Foo do
        @behaviour Bar
        @behaviour Baz

        # Will warn if neither Bar nor Baz specify a callback named bar/0.
        @impl true
        def bar(), do: :ok

        # Will warn if Baz does not specify a callback named baz/0.
        @impl Baz
        def baz(), do: :ok
      end

  The code is now more readable, as it is now clear which functions are
  part of your API and which ones are callback implementations. To reinforce this
  idea, `@impl true` automatically marks the function as `@doc false`, disabling
  documentation unless `@doc` is explicitly set.

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

  ### `@deprecated` (since v1.6.0)

  Provides the deprecation reason for a function. For example:

      defmodule Keyword do
        @deprecated "Use Kernel.length/1 instead"
        def size(keyword) do
          length(keyword)
        end
      end

  The Mix compiler automatically looks for calls to deprecated modules
  and emit warnings during compilation.

  Using the `@deprecated` attribute will also be reflected in the
  documentation of the given function and macro. You can choose between
  the `@deprecated` attribute and the documentation metadata to provide
  hard-deprecations (with warnings) and soft-deprecations (without warnings):

  This is a soft-deprecation as it simply annotates the documentation
  as deprecated:

      @doc deprecated: "Use Kernel.length/1 instead"
      def size(keyword)

  This is a hard-deprecation as it emits warnings and annotates the
  documentation as deprecated:

      @deprecated "Use Kernel.length/1 instead"
      def size(keyword)

  Currently `@deprecated` only supports functions and macros. However
  you can use the `:deprecated` key in the annotation metadata to
  annotate the docs of modules, types and callbacks too.

  We recommend using this feature with care, especially library authors.
  Deprecating code always pushes the burden towards library users. We
  also recommend for deprecated functionality to be maintained for long
  periods of time, even after deprecation, giving developers plenty of
  time to update (except for cases where keeping the deprecated API is
  undesired, such as in the presence of security issues).

  ### `@doc` and `@typedoc`

  Provides documentation for the entity that follows the attribute.
  `@doc` is to be used with a function, macro, callback, or
  macrocallback, while `@typedoc` with a type (public or opaque).

  Accepts one of these:

    * a string (often a heredoc)
    * `false`, which will make the entity invisible to documentation-extraction
      tools like [`ExDoc`](https://hexdocs.pm/ex_doc/)
    * a keyword list, since Elixir 1.7.0

  For example:

      defmodule MyModule do
        @typedoc "This type"
        @typedoc since: "1.1.0"
        @type t :: term

        @doc "Hello world"
        @doc since: "1.1.0"
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

  As can be seen in the example above, since Elixir 1.7.0 `@doc` and `@typedoc`
  also accept a keyword list that serves as a way to provide arbitrary metadata
  about the entity. Tools like [`ExDoc`](https://hexdocs.pm/ex_doc/) and
  `IEx` may use this information to display annotations. A common use
  case is the `:since` key, which may be used to annotate in which version the
  function was introduced.

  As illustrated in the example, it is possible to use these attributes
  more than once before an entity. However, the compiler will warn if
  used twice with binaries as that replaces the documentation text from
  the preceding use. Multiple uses with keyword lists will merge the
  lists into one.

  Note that since the compiler also defines some additional metadata,
  there are a few reserved keys that will be ignored and warned if used.
  Currently these are: `:opaque` and `:defaults`.

  Once this module is compiled, this information becomes available via
  the `Code.fetch_docs/1` function.

  ### `@dialyzer`

  Defines warnings to request or suppress when using `:dialyzer`.

  Accepts an atom, a tuple, or a list of atoms and tuples. For example:

      defmodule MyModule do
        @dialyzer {:nowarn_function, [my_fun: 1]}

        def my_fun(arg) do
          M.not_a_function(arg)
        end
      end

  For the list of supported warnings, see [`:dialyzer` module](`:dialyzer`).

  Multiple uses of `@dialyzer` will accumulate instead of overriding
  previous ones.

  ### `@external_resource`

  Specifies an external resource for the current module.

  Sometimes a module embeds information from an external file. This
  attribute allows the module to annotate which external resources
  have been used.

  Tools may use this information to ensure the module is recompiled
  in case any of the external resources change, see for example:
  [`mix compile.elixir`](https://hexdocs.pm/mix/Mix.Tasks.Compile.Elixir.html).

  The specified file path provided is interpreted as relative to
  the folder containing the project's `mix.exs`, which is the
  current working directory, not the file where `@external_resource`
  is declared.

  If the external resource does not exist, the module still has
  a dependency on it, causing the module to be recompiled as soon
  as the file is added.

  For more control over when a module is recompiled, see
  [`__mix_recompile__?/0`](`m:Mix.Tasks.Compile.Elixir#module-__mix_recompile__-0`).

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

  Note that this is only valid for exceptions/diagnostics that come from the
  definition inner scope (which includes its patterns and guards). For example:

      defmodule MyModule do # <---- module definition
        @file "hello.ex"
        defp unused(a) do # <---- function definition
          "world" # <---- function scope
        end

        @file "bye.ex"
        def unused(_), do: true
      end

  If you run this code with the second "unused" definition commented, you will
  see that `hello.ex` is used as the stacktrace when reporting warnings, but if
  you uncomment it you'll see that the error will not mention `bye.ex`, because
  it's a module-level error rather than an expression-level error.

  ### `@moduledoc`

  Provides documentation for the current module.

      defmodule MyModule do
        @moduledoc """
        A very useful module.
        """
        @moduledoc authors: ["Alice", "Bob"]
      end

  Accepts a string (often a heredoc) or `false` where `@moduledoc false`
  will make the module invisible to documentation extraction tools like
  [`ExDoc`](https://hexdocs.pm/ex_doc/).

  Similarly to `@doc` also accepts a keyword list to provide metadata
  about the module. For more details, see the documentation of `@doc`
  above.

  Once this module is compiled, this information becomes available via
  the `Code.fetch_docs/1` function.

  ### `@nifs` (since v1.16.0)

  A list of functions and their arities which will be overridden
  by a native implementation (NIF).

      defmodule MyLibrary.MyModule do
        @nifs [foo: 1, bar: 2]

        def foo(arg1), do: :erlang.nif_error(:not_loaded)
        def bar(arg1, arg2), do: :erlang.nif_error(:not_loaded)
      end

  See the Erlang documentation for more information:
  https://www.erlang.org/doc/man/erl_nif

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
          IO.puts("Defining #{kind} named #{name} with args:")
          IO.inspect(args)
          IO.puts("and guards")
          IO.inspect(guards)
          IO.puts("and body")
          IO.puts(Macro.to_string(body))
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

  ### `@on_load`

  A hook that will be invoked whenever the module is loaded.

  Accepts the function name (as an atom) of a function in the current module.
  The function must have an arity of 0 (no arguments). If the function does
  not return `:ok`, the loading of the module will be aborted.
  For example:

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

  ### `@vsn`

  Specify the module version. Accepts any valid Elixir value, for example:

      defmodule MyModule do
        @vsn "1.0"
      end

  ### Struct attributes

    * `@derive` - derives an implementation for the given protocol for the
      struct defined in the current module

    * `@enforce_keys` - ensures the given keys are always set when building
      the struct defined in the current module

  See `defstruct/1` for more information on building and using structs.

  ### Typespec attributes

  The following attributes are part of typespecs and are also built-in in
  Elixir:

    * `@type` - defines a type to be used in `@spec`
    * `@typep` - defines a private type to be used in `@spec`
    * `@opaque` - defines an opaque type to be used in `@spec`
    * `@spec` - provides a specification for a function
    * `@callback` - provides a specification for a behaviour callback (and generates
      a `behaviour_info/1` function in the module, see below)
    * `@macrocallback` - provides a specification for a macro behaviour callback
    * `@optional_callbacks` - specifies which behaviour callbacks and macro
      behaviour callbacks are optional
    * `@impl` - declares an implementation of a callback function or macro

  For detailed documentation, see the [typespec documentation](typespecs.md).

  ### Custom attributes

  In addition to the built-in attributes outlined above, custom attributes may
  also be added. Custom attributes are expressed using the `@/1` operator followed
  by a valid variable name. The value given to the custom attribute must be a valid
  Elixir value:

      defmodule MyModule do
        @custom_attr [some: "stuff"]
      end

  For more advanced options available when defining custom attributes, see
  `register_attribute/3`.

  ## Compile callbacks

  There are three compilation callbacks, invoked in this order:
  `@before_compile`, `@after_compile`, and `@after_verify`.
  They are described next.

  ### `@before_compile`

  A hook that will be invoked before the module is compiled. This is
  often used to change how the current module is being compiled.

  Accepts a module or a `{module, function_or_macro_name}` tuple. The
  function/macro must take one argument: the module environment. If
  it's a macro, its returned value will be injected at the end of the
  module definition before the compilation starts.

  When just a module is provided, the function/macro is assumed to be
  `__before_compile__/1`.

  Callbacks will run in the order they are registered. Any overridable
  definition will be made concrete before the first callback runs.
  A definition may be made overridable again in another before compile
  callback and it will be made concrete one last time after all callbacks
  run.

  *Note*: the callback function/macro must be placed in a separate module
  (because when the callback is invoked, the current module does not yet exist).

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

  ### `@after_compile`

  A hook that will be invoked right after the current module is compiled.

  Accepts a module or a `{module, function_name}` tuple. The function
  must take two arguments: the module environment and its bytecode.
  When just a module is provided, the function is assumed to be
  `__after_compile__/2`.

  Callbacks will run in the order they are registered.

  `Module` functions expecting not yet compiled modules (such as `definitions_in/1`)
  are still available at the time `@after_compile` is invoked.

  #### Example

      defmodule MyModule do
        @after_compile __MODULE__

        def __after_compile__(env, _bytecode) do
          IO.inspect(env)
        end
      end

  ### `@after_verify`

  A hook that will be invoked right after the current module is verified for
  undefined functions, deprecations, etc. A module is always verified after
  it is compiled. In Mix projects, a module is also verified when any of its
  runtime dependencies change. Therefore this is useful to perform verification
  of the current module while avoiding compile-time dependencies. Given the
  callback is invoked under different scenarios, Elixir provides no guarantees
  of when in the compilation cycle nor in which process the callback runs.

  Accepts a module or a `{module, function_name}` tuple. The function
  must take one argument: the module name. When just a module is provided,
  the function is assumed to be `__after_verify__/1`.

  Callbacks will run in the order they are registered.

  `Module` functions expecting not yet compiled modules are no longer available
  at the time `@after_verify` is invoked.

  #### Example

      defmodule MyModule do
        @after_verify __MODULE__

        def __after_verify__(module) do
          IO.inspect(module)
          :ok
        end
      end

  ## Compile options

  The `@compile` attribute accepts different options that are used by both
  Elixir and Erlang compilers. Some of the common use cases are documented
  below:

    * `@compile :debug_info` - includes `:debug_info` regardless of the
      corresponding setting in `Code.get_compiler_option/1`

    * `@compile {:debug_info, false}` - disables `:debug_info` regardless
      of the corresponding setting in `Code.get_compiler_option/1`. Note
      disabling `:debug_info` is not recommended as it removes the ability
      of the Elixir compiler and other tools to static analyse the code.
      If you want to remove the `:debug_info` while deploying, tools like
      `mix release` already do such by default.

    * `@compile {:inline, some_fun: 2, other_fun: 3}` - inlines the given
      name/arity pairs. Inlining is applied locally, calls from another
      module are not affected by this option

    * `@compile {:autoload, false}` - disables automatic loading of
      modules after compilation. Instead, the module will be loaded after
      it is dispatched to

    * `@compile {:no_warn_undefined, Mod}` or
      `@compile {:no_warn_undefined, {Mod, fun, arity}}` - does not warn if
      the given module or the given `Mod.fun/arity` are not defined

  ## Generated functions

  Sometimes the compiler will generate public functions within modules. These
  are documented below.

  ### `behaviour_info/1`

  This function is generated for modules that define a behaviour, that is,
  that have one or more `@callback` definitions. The signature for this function,
  expressed as a spec, is:

      @spec behaviour_info(:callbacks) :: [function_info]
        when function_info: {function_name :: atom(), arity :: non_neg_integer()}

      @spec behaviour_info(:optional_callbacks) :: [function_info]
        when function_info: {function_name :: atom(), arity :: non_neg_integer()}

  `behaviour_info(:callbacks)` includes optional callbacks.

  For example:

      iex> Enum.sort(GenServer.behaviour_info(:callbacks))
      [
        code_change: 3,
        format_status: 1,
        format_status: 2,
        handle_call: 3,
        handle_cast: 2,
        handle_continue: 2,
        handle_info: 2,
        init: 1,
        terminate: 2
      ]

  ### `module_info/0`

  This function is generated for all modules. It returns all the attributes
  returned by `module_info/1` (see below), but as a single keyword list. See also the
  [Erlang documentation](https://www.erlang.org/doc/system/modules.html#module_info-0).

  ### `module_info/1`

  This function is generated for all modules and returns
  information about the module. The signature for this function,
  expressed as a spec, is:

      @spec module_info(:module) :: module() # Returns the module itself
      @spec module_info(:attributes) :: keyword()
      @spec module_info(:compile) :: keyword()
      @spec module_info(:md5) :: binary()
      @spec module_info(:nifs) :: module()
      @spec module_info(:exports) :: [function_info]
        when function_info: {function_name :: atom(), arity :: non_neg_integer()}
      @spec module_info(:functions) :: [function_info]
        when function_info: {function_name :: atom(), arity :: non_neg_integer()}

  For example:

      iex> URI.module_info(:module)
      URI
      iex> {:decode_www_form, 1} in URI.module_info(:exports)
      true

  For more information about `module_info/1`, also check out the [Erlang
  documentation](https://www.erlang.org/doc/system/modules.html#module_info-1).

  ### `__info__/1`

  This function is generated for all modules. It's similar to `module_info/1` but
  includes some additional Elixir-specific information, such as struct and macro
  information. For documentation, see `c:Module.__info__/1`.

  '''

  @type definition :: {atom, arity}
  @type def_kind :: :def | :defp | :defmacro | :defmacrop

  @extra_error_msg_defines? "Use Kernel.function_exported?/3 and Kernel.macro_exported?/3 " <>
                              "to check for public functions and macros instead"

  @extra_error_msg_definitions_in "Use the Module.__info__/1 callback to get public functions and macros instead"

  @doc """
  Provides runtime information about functions, macros, and other information
  defined by the module.

  Each module gets an `__info__/1` function when it's compiled. The function
  takes one of the following items:

    * `:attributes` - a keyword list with all persisted attributes

    * `:compile` - a list with compiler metadata

    * `:functions` - a keyword list of public functions and their arities

    * `:macros` - a keyword list of public macros and their arities

    * `:md5` - the MD5 of the module

    * `:module` - the module atom name

    * `:struct` - (since v1.14.0) if the module defines a struct and if so each field in order

  """
  @callback __info__(:attributes) :: keyword()
  @callback __info__(:compile) :: [term()]
  @callback __info__(:functions) :: keyword()
  @callback __info__(:macros) :: keyword()
  @callback __info__(:md5) :: binary()
  @callback __info__(:module) :: module()
  @callback __info__(:struct) :: list(%{field: atom(), required: boolean()}) | nil

  @doc """
  Returns information about module attributes used by Elixir.

  See the "Module attributes" section in the module documentation for more
  information on each attribute.

  ## Examples

      iex> map = Module.reserved_attributes()
      iex> Map.has_key?(map, :moduledoc)
      true
      iex> Map.has_key?(map, :doc)
      true

  """
  @doc since: "1.12.0"
  @spec reserved_attributes() :: map
  def reserved_attributes() do
    %{
      after_compile: %{
        doc: "A hook that will be invoked right after the current module is compiled."
      },
      after_verify: %{
        doc: "A hook that will be invoked right after the current module is verified."
      },
      before_compile: %{
        doc: "A hook that will be invoked before the module is compiled."
      },
      behaviour: %{
        doc: "Specifies that the current module implements a given behaviour."
      },
      enforce_keys: %{
        doc:
          "Ensures the given keys are always set when building the struct defined in the current module."
      },
      fallback_to_any: %{
        doc:
          "If set to `true` generates a default protocol implementation for all types (inside `defprotocol`)."
      },
      for: %{
        doc:
          "The current module/type a protocol implementation is being defined for (inside `defimpl`)."
      },
      protocol: %{
        doc: "The current protocol being implemented (inside `defimpl`)."
      },
      on_definition: %{
        doc:
          "A hook that will be invoked when each function or macro in the current module is defined."
      },
      impl: %{
        doc: "Declares an implementation of a callback function or macro."
      },
      compile: %{
        doc: "Defines options for module compilation."
      },
      deprecated: %{
        doc: "Provides the deprecation reason for a function."
      },
      moduledoc: %{
        doc: "Provides documentation for the current module."
      },
      doc: %{
        doc: "Provides documentation for a function/macro/callback."
      },
      typedoc: %{
        doc: "Provides documentation for a type."
      },
      dialyzer: %{
        doc: "Defines Dialyzer warnings to request or suppress."
      },
      external_resource: %{
        doc: "Specifies an external resource for the current module."
      },
      file: %{
        doc:
          "Changes the filename used in stacktraces for the function or macro that follows the attribute."
      },
      on_load: %{
        doc: "A hook that will be invoked whenever the module is loaded."
      },
      vsn: %{
        doc: "Specify the module version."
      },
      type: %{
        doc: "Defines a type to be used in `@spec`."
      },
      typep: %{
        doc: "Defines a private type to be used in `@spec`."
      },
      opaque: %{
        doc: "Defines an opaque type to be used in `@spec`."
      },
      spec: %{
        doc: "Provides a specification for a function."
      },
      callback: %{
        doc: "Provides a specification for a behaviour callback."
      },
      macrocallback: %{
        doc: "Provides a specification for a macro behaviour callback."
      },
      optional_callbacks: %{
        doc: "Specifies which behaviour callbacks and macro behaviour callbacks are optional."
      },
      derive: %{
        doc:
          "Derives an implementation for the given protocol for the struct defined in the current module."
      }
    }
  end

  @doc """
  Checks if a module is open.

  A module is "open" if it is currently being defined and its attributes and
  functions can be modified.
  """
  @spec open?(module) :: boolean
  def open?(module) when is_atom(module) do
    :elixir_module.is_open(module)
  end

  @deprecated "Use Code.eval_quoted/3 instead"
  def eval_quoted(module_or_env, quoted, binding \\ [], opts \\ [])

  def eval_quoted(%Macro.Env{} = env, quoted, binding, opts)
      when is_list(binding) and is_list(opts) do
    validated_eval_quoted(env.module, quoted, binding, struct!(env, opts))
  end

  def eval_quoted(module, quoted, binding, %Macro.Env{} = env)
      when is_atom(module) and is_list(binding) do
    validated_eval_quoted(module, quoted, binding, env)
  end

  def eval_quoted(module, quoted, binding, opts)
      when is_atom(module) and is_list(binding) and is_list(opts) do
    validated_eval_quoted(module, quoted, binding, opts)
  end

  defp validated_eval_quoted(module, quoted, binding, env_or_opts) do
    assert_not_compiled!({:eval_quoted, 4}, module)
    :elixir_def.reset_last(module)
    env = :elixir.env_for_eval(env_or_opts)
    {value, binding, _env} = :elixir.eval_quoted(quoted, binding, %{env | module: module})
    {value, binding}
  end

  @doc """
  Creates a module with the given name and defined by
  the given quoted expressions.

  The line where the module is defined and its file **must**
  be passed as options. See `Code.env_for_eval/1` for a complete
  list of options.

  It returns a tuple of shape `{:module, module, binary, term}`
  where `module` is the module name, `binary` is the module
  bytecode and `term` is the result of the last expression in
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

      Hello.world()
      #=> true

  ## Differences from `defmodule`

  `Module.create/3` works similarly to `Kernel.defmodule/2`
  and return the same results. While one could also use
  `Kernel.defmodule/2` to define modules dynamically, this function
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
    create([line: env.line], module, quoted, env)
  end

  def create(module, quoted, opts) when is_atom(module) and is_list(opts) do
    if not Keyword.has_key?(opts, :file) do
      raise ArgumentError, "expected :file to be given as option"
    end

    meta = Keyword.take(opts, [:line, :generated])
    create(meta, module, quoted, :elixir.env_for_eval(opts))
  end

  defp create(meta, module, quoted, env_or_opts) do
    next = :elixir_module.next_counter(nil)
    quoted = :elixir_quote.linify_with_context_counter(meta, {module, next}, quoted)
    :elixir_module.compile(module, quoted, [], false, :elixir.env_for_eval(env_or_opts))
  end

  @doc """
  Concatenates a list of aliases and returns a new alias.

  It handles binaries and atoms.

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

  It handles binaries and atoms.

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
  It handles binaries and atoms.

  ## Examples

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
  It handles binaries and atoms.

  ## Examples

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
    expand_keys(reverse_args, counters, [])
  end

  defp simplify_args([arg | args], counters, acc, env) do
    {arg, counters} = simplify_arg(arg, counters, env)
    simplify_args(args, counters, [arg | acc], env)
  end

  defp simplify_args([], counters, reverse_args, _env) do
    {reverse_args, counters}
  end

  defp simplify_arg({:\\, _, [left, right]}, counters, env) do
    {left, counters} = simplify_arg(left, counters, env)

    right =
      Macro.prewalk(right, fn
        {:@, _, _} = attr -> Macro.expand_once(attr, env)
        other -> other
      end)

    {{:\\, [], [left, right]}, counters}
  end

  # If the variable is being used explicitly for naming,
  # we always give it a higher priority (nil) even if it
  # starts with underscore.
  defp simplify_arg({:=, _, [{var, _, atom}, _]}, counters, _env) when is_atom(atom) do
    {simplify_var(var, nil), counters}
  end

  defp simplify_arg({:=, _, [_, {var, _, atom}]}, counters, _env) when is_atom(atom) do
    {simplify_var(var, nil), counters}
  end

  # If we have only the variable as argument, it also gets
  # higher priority. However, if the variable starts with an
  # underscore, we give it a secondary context (Elixir) with
  # lower priority.
  defp simplify_arg({var, _, atom}, counters, _env) when is_atom(atom) do
    {simplify_var(var, Elixir), counters}
  end

  defp simplify_arg({:%, _, [left, _]}, counters, env) do
    case Macro.expand_once(left, env) do
      module when is_atom(module) -> autogenerated_key(counters, simplify_module_name(module))
      _ -> autogenerated_key(counters, :struct)
    end
  end

  defp simplify_arg({:%{}, _, _}, counters, _env) do
    autogenerated_key(counters, :map)
  end

  defp simplify_arg({:@, _, _} = attr, counters, env) do
    simplify_arg(Macro.expand_once(attr, env), counters, env)
  end

  defp simplify_arg({:var!, _, [{var, _, atom} | _]}, counters, _env) when is_atom(atom) do
    {simplify_var(var, Elixir), counters}
  end

  defp simplify_arg(other, counters, _env) when is_integer(other),
    do: autogenerated_key(counters, :int)

  defp simplify_arg(other, counters, _env) when is_boolean(other),
    do: autogenerated_key(counters, :bool)

  defp simplify_arg(other, counters, _env) when is_atom(other),
    do: autogenerated_key(counters, :atom)

  defp simplify_arg(other, counters, _env) when is_list(other),
    do: autogenerated_key(counters, :list)

  defp simplify_arg(other, counters, _env) when is_float(other),
    do: autogenerated_key(counters, :float)

  defp simplify_arg(other, counters, _env) when is_binary(other),
    do: autogenerated_key(counters, :binary)

  defp simplify_arg(_, counters, _env), do: autogenerated_key(counters, :arg)

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

  defp autogenerated_key(counters, key) do
    case counters do
      %{^key => :once} -> {key, %{counters | key => 2}}
      %{^key => value} -> {key, %{counters | key => value + 1}}
      %{} -> {key, Map.put(counters, key, :once)}
    end
  end

  defp expand_keys([{:\\, meta, [key, default]} | keys], counters, acc) when is_atom(key) do
    {var, counters} = expand_key(key, counters)
    expand_keys(keys, counters, [{:\\, meta, [var, default]} | acc])
  end

  defp expand_keys([key | keys], counters, acc) when is_atom(key) do
    {var, counters} = expand_key(key, counters)
    expand_keys(keys, counters, [var | acc])
  end

  defp expand_keys([arg | args], counters, acc) do
    expand_keys(args, counters, [arg | acc])
  end

  defp expand_keys([], _counters, acc) do
    acc
  end

  defp expand_key(key, counters) do
    case counters do
      %{^key => count} when is_integer(count) and count >= 1 ->
        {{:"#{key}#{count}", [], Elixir}, Map.put(counters, key, count - 1)}

      _ ->
        {{key, [], Elixir}, counters}
    end
  end

  # Merge

  defp merge_signatures([h1 | t1], [h2 | t2], i) do
    [merge_signature(h1, h2, i) | merge_signatures(t1, t2, i + 1)]
  end

  defp merge_signatures([], [], _) do
    []
  end

  defp merge_signature({:\\, meta, [left, right]}, newer, i) do
    {:\\, meta, [merge_signature(left, newer, i), right]}
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
  defp merge_signature({_, meta, _}, _newer, i), do: {:"arg#{i}", meta, Elixir}

  @doc """
  Checks if the module defines the given function or macro.

  Use `defines?/3` to assert for a specific type.

  This function can only be used on modules that have not yet been compiled.
  Use `Kernel.function_exported?/3` and `Kernel.macro_exported?/3` to check for
  public functions and macros respectively in compiled modules.

  Note that `defines?` returns `false` for functions and macros that have
  been defined but then marked as overridable and no other implementation
  has been provided. You can check the overridable status by calling
  `overridable?/2`.

  ## Examples

      defmodule Example do
        Module.defines?(__MODULE__, {:version, 0}) #=> false
        def version, do: 1
        Module.defines?(__MODULE__, {:version, 0}) #=> true
      end

  """
  @spec defines?(module, definition) :: boolean
  def defines?(module, {name, arity} = tuple)
      when is_atom(module) and is_atom(name) and is_integer(arity) and arity >= 0 and arity <= 255 do
    assert_not_compiled!(__ENV__.function, module, @extra_error_msg_defines?)
    {set, _bag} = data_tables_for(module)
    :ets.member(set, {:def, tuple})
  end

  @doc """
  Checks if the module defines a function or macro of the
  given `kind`.

  `kind` can be any of `:def`, `:defp`, `:defmacro`, or `:defmacrop`.

  This function can only be used on modules that have not yet been compiled.
  Use `Kernel.function_exported?/3` and `Kernel.macro_exported?/3` to check for
  public functions and macros respectively in compiled modules.

  ## Examples

      defmodule Example do
        Module.defines?(__MODULE__, {:version, 0}, :def) #=> false
        def version, do: 1
        Module.defines?(__MODULE__, {:version, 0}, :def) #=> true
      end

  """
  @spec defines?(module, definition, def_kind) :: boolean
  def defines?(module, {name, arity} = tuple, def_kind)
      when is_atom(module) and is_atom(name) and is_integer(arity) and arity >= 0 and arity <= 255 and
             def_kind in [:def, :defp, :defmacro, :defmacrop] do
    assert_not_compiled!(__ENV__.function, module, @extra_error_msg_defines?)

    {set, _bag} = data_tables_for(module)

    case :ets.lookup(set, {:def, tuple}) do
      [{_, ^def_kind, _, _, _, _}] -> true
      _ -> false
    end
  end

  @doc """
  Checks if the current module defines the given type (private, opaque or not).

  This function is only available for modules being compiled.
  """
  @doc since: "1.7.0"
  @spec defines_type?(module, definition) :: boolean
  def defines_type?(module, definition) when is_atom(module) do
    Kernel.Typespec.defines_type?(module, definition)
  end

  @doc """
  Copies the given spec as a callback.

  Returns `true` if there is such a spec and it was copied as a callback.
  If the function associated to the spec has documentation defined prior to
  invoking this function, the docs are copied too.
  """
  @doc since: "1.7.0"
  @spec spec_to_callback(module, definition) :: boolean
  def spec_to_callback(module, definition) do
    Kernel.Typespec.spec_to_callback(module, definition)
  end

  @doc """
  Returns all module attributes names defined in `module`.

  This function can only be used on modules that have not yet been compiled.

  ## Examples

      defmodule Example do
        @foo 1
        Module.register_attribute(__MODULE__, :bar, accumulate: true)

        :foo in Module.attributes_in(__MODULE__)
        #=> true

        :bar in Module.attributes_in(__MODULE__)
        #=> true
      end

  """
  @doc since: "1.13.0"
  @spec attributes_in(module) :: [atom]
  def attributes_in(module) when is_atom(module) do
    assert_not_compiled!(__ENV__.function, module)
    {set, _} = data_tables_for(module)
    :ets.select(set, [{{:"$1", :_, :_, :_}, [{:is_atom, :"$1"}], [:"$1"]}])
  end

  @doc """
  Returns all overridable definitions in `module`.

  Note a definition is included even if it was was already overridden.
  You can use `defines?/2` to see if a definition exists or one is pending.

  This function can only be used on modules that have not yet been compiled.

  ## Examples

      defmodule Example do
        def foo, do: 1
        def bar, do: 2

        defoverridable foo: 0, bar: 0
        def foo, do: 3

        [bar: 0, foo: 0] = Module.overridables_in(__MODULE__) |> Enum.sort()
      end

  """
  @doc since: "1.13.0"
  @spec overridables_in(module) :: [atom]
  def overridables_in(module) when is_atom(module) do
    assert_not_compiled!(__ENV__.function, module)
    :elixir_overridable.overridables_for(module)
  end

  @doc """
  Returns all functions and macros defined in `module`.

  It returns a list with all defined functions and macros, public and private,
  in the shape of `[{name, arity}, ...]`.

  This function can only be used on modules that have not yet been compiled.
  Use the `c:Module.__info__/1` callback to get the public functions and macros in
  compiled modules.

  ## Examples

      defmodule Example do
        def version, do: 1
        defmacrop test(arg), do: arg
        Module.definitions_in(__MODULE__) #=> [{:version, 0}, {:test, 1}]
      end

  """
  @spec definitions_in(module) :: [definition]
  def definitions_in(module) when is_atom(module) do
    assert_not_compiled!(__ENV__.function, module, @extra_error_msg_definitions_in)
    {_, bag} = data_tables_for(module)
    bag_lookup_element(bag, :defs, 2)
  end

  @doc """
  Returns all functions defined in `module`, according
  to its kind.

  This function can only be used on modules that have not yet been compiled.
  Use the `c:Module.__info__/1` callback to get the public functions and macros in
  compiled modules.

  ## Examples

      defmodule Example do
        def version, do: 1
        Module.definitions_in(__MODULE__, :def)  #=> [{:version, 0}]
        Module.definitions_in(__MODULE__, :defp) #=> []
      end

  """
  @spec definitions_in(module, def_kind) :: [definition]
  def definitions_in(module, kind)
      when is_atom(module) and kind in [:def, :defp, :defmacro, :defmacrop] do
    assert_not_compiled!(__ENV__.function, module, @extra_error_msg_definitions_in)
    {set, _} = data_tables_for(module)
    :ets.select(set, [{{{:def, :"$1"}, kind, :_, :_, :_, :_}, [], [:"$1"]}])
  end

  @doc """
  Returns the definition for the given name-arity pair.

  It returns a tuple with the `version`, the `kind`,
  the definition `metadata`, and a list with each clause.
  Each clause is a four-element tuple with metadata,
  the arguments, the guards, and the clause AST.

  The clauses are returned in the Elixir AST but a subset
  that has already been expanded and normalized. This makes
  it useful for analyzing code but it cannot be reinjected
  into the module as it will have lost some of its original
  context. Given this AST representation is mostly internal,
  it is versioned and it may change at any time. Therefore,
  **use this API with caution**.

  ## Options

    * `:skip_clauses` (since v1.14.0) - returns `[]` instead
      of returning the clauses. This is useful when there is
      only an interest in fetching the kind and the metadata

  """
  @spec get_definition(module, definition, keyword) ::
          {:v1, def_kind, meta :: keyword,
           [{meta :: keyword, arguments :: [Macro.t()], guards :: [Macro.t()], Macro.t()}]}
          | nil
  @doc since: "1.12.0"
  def get_definition(module, {name, arity}, options \\ [])
      when is_atom(module) and is_atom(name) and is_integer(arity) and is_list(options) do
    assert_not_compiled!(__ENV__.function, module, "")
    {set, bag} = data_tables_for(module)

    case :ets.lookup(set, {:def, {name, arity}}) do
      [{_key, kind, meta, _, _, _}] ->
        clauses =
          if options[:skip_clauses],
            do: [],
            else: bag_lookup_element(bag, {:clauses, {name, arity}}, 2)

        {:v1, kind, meta, clauses}

      [] ->
        nil
    end
  end

  @doc """
  Deletes a definition from a module.

  It returns `true` if the definition exists and it was removed,
  otherwise it returns `false`.
  """
  @doc since: "1.12.0"
  @spec delete_definition(module, definition) :: boolean()
  def delete_definition(module, {name, arity})
      when is_atom(module) and is_atom(name) and is_integer(arity) do
    assert_not_readonly!(__ENV__.function, module)

    case :elixir_def.take_definition(module, {name, arity}) do
      false ->
        false

      _ ->
        :elixir_locals.yank({name, arity}, module)
        true
    end
  end

  @doc """
  Makes the given functions in `module` overridable.

  An overridable function is lazily defined, allowing a
  developer to customize it. See `Kernel.defoverridable/1` for
  more information and documentation.

  Once a function or a macro is marked as overridable, it will
  no longer be listed under `definitions_in/1` or return true
  when given to `defines?/2` until another implementation is
  given.
  """
  @spec make_overridable(module, [definition]) :: :ok
  def make_overridable(module, tuples) when is_atom(module) and is_list(tuples) do
    assert_not_readonly!(__ENV__.function, module)

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
            :elixir_overridable.record_overridable(module, tuple, clause, neighbours)
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

    behaviour_callbacks = Module.Behaviour.callbacks(behaviour)

    tuples =
      for definition <- definitions_in(module),
          definition in behaviour_callbacks,
          do: definition

    make_overridable(module, tuples)
  end

  defp check_module_for_overridable(module, behaviour) do
    {_, bag} = data_tables_for(module)
    behaviour_definitions = bag_lookup_element(bag, {:accumulate, :behaviour}, 2)

    cond do
      not Code.ensure_loaded?(behaviour) ->
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

  @doc """
  Returns `true` if `tuple` in `module` was marked as overridable
  at some point.

  Note `overridable?/2` returns `true` even if the definition was
  already overridden. You can use `defines?/2` to see if a definition
  exists or one is pending.
  """
  @spec overridable?(module, definition) :: boolean
  def overridable?(module, {function_name, arity} = tuple)
      when is_atom(function_name) and is_integer(arity) and arity >= 0 and arity <= 255 do
    :elixir_overridable.overridable_for(module, tuple) != :not_overridable
  end

  @doc """
  Puts a module attribute with `key` and `value` in the given `module`.

  ## Examples

      defmodule MyModule do
        Module.put_attribute(__MODULE__, :custom_threshold_for_lib, 10)
      end

  """
  @spec put_attribute(module, atom, term) :: :ok
  def put_attribute(module, key, value) when is_atom(module) and is_atom(key) do
    __put_attribute__(module, key, value, nil, [])
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

  This function can only be used on modules that have not yet been compiled.
  Use the `c:Module.__info__/1` callback to get all persisted attributes, or
  `Code.fetch_docs/1` to retrieve all documentation related attributes in
  compiled modules.

  ## Examples

      defmodule Foo do
        Module.put_attribute(__MODULE__, :value, 1)
        Module.get_attribute(__MODULE__, :value) #=> 1

        Module.get_attribute(__MODULE__, :value, :default) #=> 1
        Module.get_attribute(__MODULE__, :not_found, :default) #=> :default

        Module.register_attribute(__MODULE__, :value, accumulate: true)
        Module.put_attribute(__MODULE__, :value, 1)
        Module.get_attribute(__MODULE__, :value) #=> [1]
      end

  """
  @spec get_attribute(module, atom, term) :: term
  def get_attribute(module, key, default \\ nil) when is_atom(module) and is_atom(key) do
    get_attribute(module, key, nil, true, false, default, {:get_attribute, 2})
  end

  @doc """
  Gets the last set value of a given attribute from a module.

  If the attribute was marked with `accumulate` with
  `Module.register_attribute/3`, the previous value to have been set will be
  returned. If the attribute does not accumulate, this call is the same as
  calling `Module.get_attribute/3`.

  This function can only be used on modules that have not yet been compiled.
  Use the `c:Module.__info__/1` callback to get all persisted attributes, or
  `Code.fetch_docs/1` to retrieve all documentation related attributes in
  compiled modules.

  ## Examples

      defmodule Foo do
        Module.put_attribute(__MODULE__, :value, 1)
        Module.get_last_attribute(__MODULE__, :value) #=> 1

        Module.get_last_attribute(__MODULE__, :not_found, :default) #=> :default

        Module.register_attribute(__MODULE__, :acc, accumulate: true)
        Module.put_attribute(__MODULE__, :acc, 1)
        Module.get_last_attribute(__MODULE__, :acc) #=> 1
        Module.put_attribute(__MODULE__, :acc, 2)
        Module.get_last_attribute(__MODULE__, :acc) #=> 2
      end

  """
  @doc since: "1.15.0"
  @spec get_last_attribute(module, atom, term) :: term
  def get_last_attribute(module, key, default \\ nil) when is_atom(module) and is_atom(key) do
    get_attribute(module, key, nil, true, true, default, {:get_last_attribute, 2})
  end

  @doc """
  Checks if the given attribute has been defined.

  An attribute is defined if it has been registered with `register_attribute/3`
  or assigned a value. If an attribute has been deleted with `delete_attribute/2`
  it is no longer considered defined.

  This function can only be used on modules that have not yet been compiled.

  ## Examples

      defmodule MyModule do
        @value 1
        Module.register_attribute(__MODULE__, :other_value)
        Module.put_attribute(__MODULE__, :another_value, 1)

        Module.has_attribute?(__MODULE__, :value) #=> true
        Module.has_attribute?(__MODULE__, :other_value) #=> true
        Module.has_attribute?(__MODULE__, :another_value) #=> true

        Module.has_attribute?(__MODULE__, :undefined) #=> false

        Module.delete_attribute(__MODULE__, :value)
        Module.has_attribute?(__MODULE__, :value) #=> false
      end

  """
  @doc since: "1.10.0"
  @spec has_attribute?(module, atom) :: boolean
  def has_attribute?(module, key) when is_atom(module) and is_atom(key) do
    assert_not_compiled!(__ENV__.function, module)
    {set, _bag} = data_tables_for(module)

    :ets.member(set, key)
  end

  @doc """
  Deletes the entry (or entries) for the given module attribute.

  It returns the deleted attribute value. If the attribute has not
  been set nor configured to accumulate, it returns `nil`.

  If the attribute is set to accumulate, then this function always
  returns a list. Deleting the attribute removes existing entries
  but the attribute will still accumulate.

  ## Examples

      defmodule MyModule do
        Module.put_attribute(__MODULE__, :custom_threshold_for_lib, 10)
        Module.delete_attribute(__MODULE__, :custom_threshold_for_lib)
      end

  """
  @spec delete_attribute(module, atom) :: term
  def delete_attribute(module, key) when is_atom(module) and is_atom(key) do
    assert_not_readonly!(__ENV__.function, module)
    {set, bag} = data_tables_for(module)

    case :ets.lookup(set, key) do
      [{_, _, :accumulate, traces}] ->
        trace_attribute(true, module, traces, set, key, nil, [])
        reverse_values(:ets.take(bag, {:accumulate, key}), [])

      [{_, value, _, traces}] ->
        trace_attribute(module, key, nil, traces)
        :ets.delete(set, key)
        value

      [] ->
        nil
    end
  end

  defp reverse_values([{_, value} | tail], acc), do: reverse_values(tail, [value | acc])
  defp reverse_values([], acc), do: acc

  @doc """
  Registers an attribute.

  By registering an attribute, a developer is able to customize
  how Elixir will store and accumulate the attribute values.

  ## Options

  When registering an attribute, two options can be given:

    * `:accumulate` - several calls to the same attribute will
      accumulate instead of overriding the previous one. New attributes
      are always added to the top of the accumulated list.

    * `:persist` - the attribute will be persisted in the Erlang
      Abstract Format. Useful when interfacing with Erlang libraries.

  By default, both options are `false`. Once an attribute has been
  set to accumulate or persist, the behaviour cannot be reverted.

  ## Examples

      defmodule MyModule do
        Module.register_attribute(__MODULE__, :custom_threshold_for_lib, accumulate: true)

        @custom_threshold_for_lib 10
        @custom_threshold_for_lib 20
        @custom_threshold_for_lib #=> [20, 10]
      end

  """
  @spec register_attribute(module, atom, [{:accumulate, boolean}, {:persist, boolean}]) :: :ok
  def register_attribute(module, attribute, options)
      when is_atom(module) and is_atom(attribute) and is_list(options) do
    assert_not_readonly!(__ENV__.function, module)
    {set, bag} = data_tables_for(module)

    if Keyword.get(options, :persist) do
      :ets.insert(bag, {:persisted_attributes, attribute})
    end

    if Keyword.get(options, :accumulate) do
      :ets.insert_new(set, {attribute, [], :accumulate, []}) ||
        :ets.update_element(set, attribute, {3, :accumulate})
    else
      :ets.insert_new(bag, {:warn_attributes, attribute})
      :ets.insert_new(set, {attribute, nil, :unset, []})
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
  @deprecated "Use @doc instead"
  def add_doc(module, line, kind, {name, arity}, signature \\ [], doc) do
    assert_not_compiled!(__ENV__.function, module)

    if kind in [:defp, :defmacrop, :typep] do
      if doc, do: {:error, :private_doc}, else: :ok
    else
      {set, _bag} = data_tables_for(module)
      compile_doc(set, nil, line, kind, name, arity, signature, nil, doc, %{}, __ENV__, false)
      :ok
    end
  end

  @doc false
  # Used internally to compile documentation.
  # This function is private and must be used only internally.
  def compile_definition_attributes(env, kind, name, args, _guards, body) do
    %{module: module} = env
    {set, bag} = data_tables_for(module)
    {arity, defaults} = args_count(args, 0, 0)

    context = Keyword.get(:ets.lookup_element(set, {:def, {name, arity}}, 3), :context)
    impl = compile_impl(set, bag, context, name, env, kind, arity, defaults)
    doc_meta = compile_doc_meta(set, bag, name, arity, defaults)

    {line, doc} = get_doc_info(set, env)
    compile_doc(set, context, line, kind, name, arity, args, body, doc, doc_meta, env, impl)

    :ok
  end

  defp compile_doc(_table, _ctx, line, kind, name, arity, _args, _body, doc, _meta, env, _impl)
       when kind in [:defp, :defmacrop] do
    if doc do
      message =
        "#{kind} #{name}/#{arity} is private, " <>
          "@doc attribute is always discarded for private functions/macros/types"

      IO.warn(message, %{env | line: line})
    end
  end

  defp compile_doc(table, ctx, line, kind, name, arity, args, body, doc, doc_meta, env, impl) do
    key = {doc_key(kind), name, arity}
    signature = build_signature(args, env)

    case :ets.lookup(table, key) do
      [] ->
        doc = if is_nil(doc) && impl, do: false, else: doc
        :ets.insert(table, {key, ctx, line, signature, doc, doc_meta})

      [{_, current_ctx, current_line, current_sign, current_doc, current_doc_meta}] ->
        if is_binary(current_doc) and is_binary(doc) and body != nil and is_nil(current_ctx) do
          message = ~s'''
          redefining @doc attribute previously set at line #{current_line}.

          Please remove the duplicate docs. If instead you want to override a \
          previously defined @doc, attach the @doc attribute to a function head \
          (the function signature not followed by any do-block). For example:

              @doc """
              new docs
              """
              def #{name}(...)
          '''

          IO.warn(message, %{env | line: line})
        end

        signature = merge_signatures(current_sign, signature, 1)
        doc = if is_nil(doc), do: current_doc, else: doc
        doc = if is_nil(doc) && impl, do: false, else: doc
        doc_meta = Map.merge(current_doc_meta, doc_meta)
        :ets.insert(table, {key, ctx, current_line, signature, doc, doc_meta})
    end
  end

  defp doc_key(:def), do: :function
  defp doc_key(:defmacro), do: :macro

  defp compile_doc_meta(set, bag, name, arity, defaults) do
    doc_meta = compile_deprecated(%{}, set, bag, name, arity, defaults)
    doc_meta = get_doc_meta(doc_meta, set)
    add_defaults_count(doc_meta, defaults)
  end

  defp get_doc_meta(existing_meta, set) do
    case :ets.take(set, {:doc, :meta}) do
      [{{:doc, :meta}, metadata}] -> Map.merge(existing_meta, metadata)
      [] -> existing_meta
    end
  end

  defp compile_deprecated(doc_meta, set, bag, name, arity, defaults) do
    case :ets.take(set, :deprecated) do
      [{:deprecated, reason, _, _}] when is_binary(reason) ->
        :ets.insert(bag, deprecated_reasons(defaults, name, arity, reason))
        Map.put(doc_meta, :deprecated, reason)

      _ ->
        doc_meta
    end
  end

  defp add_defaults_count(doc_meta, 0), do: doc_meta
  defp add_defaults_count(doc_meta, n), do: Map.put(doc_meta, :defaults, n)

  defp deprecated_reasons(0, name, arity, reason) do
    [deprecated_reason(name, arity, reason)]
  end

  defp deprecated_reasons(defaults, name, arity, reason) do
    [
      deprecated_reason(name, arity - defaults, reason)
      | deprecated_reasons(defaults - 1, name, arity, reason)
    ]
  end

  defp deprecated_reason(name, arity, reason),
    do: {:deprecated, {{name, arity}, reason}}

  defp compile_impl(set, bag, context, name, env, kind, arity, defaults) do
    %{line: line, file: file} = env

    case :ets.take(set, :impl) do
      [{:impl, value, _, _}] ->
        impl = {{name, arity}, context, defaults, kind, line, file, value}
        :ets.insert(bag, {:impls, impl})
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
  def __check_attributes__(env, set, bag) do
    check_derive(env, set, bag)

    behaviours = bag_lookup_element(bag, {:accumulate, :behaviour}, 2)
    force_behaviour_dependencies(behaviours, env)

    :ok
  end

  defp check_derive(env, set, bag) do
    case bag_lookup_element(bag, {:accumulate, :derive}, 2) do
      [] ->
        :ok

      _ ->
        message =
          case :ets.lookup(set, {:elixir, :struct}) do
            [] ->
              "warning: module attribute @derive was set but never used (it must come before defstruct)"

            _ ->
              "warning: module attribute @derive was set after defstruct, all @derive calls must come before defstruct"
          end

        IO.warn(message, env)
    end
  end

  # While `@behaviour MyBehaviour` will naturally introduce a runtime dependency,
  # `@behaviour :"Elixir.MyBehaviour"` or similar would not.
  # We force this dependency by adding the call to `MyBehaviour.behaviour_info/1`
  defp force_behaviour_dependencies(behaviours, env) do
    info_env = %{env | function: {:__info__, 1}}

    for behaviour <- behaviours do
      :elixir_env.trace({:remote_function, [], behaviour, :behaviour_info, 1}, info_env)
    end

    :ok
  end

  @doc false
  # Used internally by Kernel's @.
  # This function is private and must be used only internally.
  def __get_attribute__(module, key, caller_line, trace?) when is_atom(key) do
    get_attribute(module, key, caller_line, trace?, false, nil, {:get_attribute, 2})
  end

  defp get_attribute(
         module,
         key,
         caller_line,
         trace?,
         last_accumulated?,
         default,
         function_name_arity
       ) do
    assert_not_compiled!(
      function_name_arity,
      module,
      "Use the Module.__info__/1 callback or Code.fetch_docs/1 instead"
    )

    {set, bag} = data_tables_for(module)

    case :ets.lookup(set, key) do
      [{_, _, :unset, _}] ->
        default

      [{_, _, :accumulate, traces}] ->
        trace_attribute(trace?, module, traces, set, key, caller_line, [])
        lookup_accumulate_attribute(bag, key, default, last_accumulated?)

      [{_, value, warn_line, traces}] when is_integer(warn_line) ->
        trace_attribute(trace?, module, traces, set, key, caller_line, [{3, :used}])
        value

      [{_, value, _, traces}] ->
        trace_attribute(trace?, module, traces, set, key, caller_line, [])
        value

      [] when is_integer(caller_line) ->
        # TODO: Consider raising instead of warning on v2.0 as it usually cascades
        error_message =
          "undefined module attribute @#{key}, " <>
            "please remove access to @#{key} or explicitly set it before access"

        IO.warn(error_message, attribute_stack(module, caller_line))
        default

      [] ->
        default
    end
  end

  defp lookup_accumulate_attribute(bag, key, _default, false) do
    :lists.reverse(bag_lookup_element(bag, {:accumulate, key}, 2))
  end

  defp lookup_accumulate_attribute(bag, key, default, true) do
    case :ets.select(bag, [{{{:accumulate, key}, :"$1"}, [], [:"$1"]}], 1) do
      {[value], _} -> value
      _ -> default
    end
  end

  defp trace_attribute(module, key, caller_line, traces) do
    :lists.foreach(
      fn {line, lexical_tracker, tracers, aliases} ->
        line = caller_line || line

        env = %{
          Macro.Env.__struct__()
          | line: line,
            file: "@#{key}",
            lexical_tracker: lexical_tracker,
            module: module,
            tracers: tracers
        }

        :lists.foreach(
          fn alias ->
            :elixir_env.trace({:alias_reference, [line: line], alias}, env)
          end,
          aliases
        )
      end,
      traces
    )
  end

  defp trace_attribute(trace?, module, traces, set, key, caller_line, updates) do
    updates =
      if trace? and traces != [] do
        trace_attribute(module, key, caller_line, traces)
        updates ++ [{4, []}]
      else
        updates
      end

    case updates do
      [] -> :ok
      _ -> :ets.update_element(set, key, updates)
    end

    :ok
  end

  @doc false
  # Used internally by Kernel's @.
  # This function is private and must be used only internally.
  def __put_attribute__(module, key, value, warn_line, traces) when is_atom(key) do
    assert_not_readonly!({:put_attribute, 3}, module)
    {set, bag} = data_tables_for(module)
    put_attribute(module, key, value, warn_line, traces, set, bag)
    :ok
  end

  defp put_attribute(_module, :on_load, value, warn_line, traces, set, bag) do
    value =
      case value do
        _ when is_atom(value) ->
          {value, 0}

        {atom, 0} = tuple when is_atom(atom) ->
          tuple

        _ ->
          raise ArgumentError,
                "@on_load is a built-in module attribute that annotates a function to be invoked " <>
                  "when the module is loaded. It should be an atom or an {atom, 0} tuple, " <>
                  "got: #{inspect(value)}"
      end

    try do
      :ets.lookup_element(set, :on_load, 3)
    catch
      :error, :badarg ->
        :ets.insert(set, {:on_load, value, warn_line, traces})
        :ets.insert(bag, {:warn_attributes, :on_load})
    else
      _ -> raise ArgumentError, "the @on_load attribute can only be set once per module"
    end
  end

  # If any of the doc attributes are called with a keyword list that
  # will become documentation metadata. Multiple calls will be merged
  # into the same map overriding duplicate keys.
  defp put_attribute(module, key, {_, metadata}, warn_line, _traces, set, _bag)
       when key in [:doc, :typedoc, :moduledoc] and is_list(metadata) do
    metadata_map = preprocess_doc_meta(metadata, module, warn_line, %{})

    case :ets.insert_new(set, {{key, :meta}, metadata_map}) do
      true ->
        :ok

      false ->
        current_metadata = :ets.lookup_element(set, {key, :meta}, 2)
        :ets.update_element(set, {key, :meta}, {2, Map.merge(current_metadata, metadata_map)})
    end
  end

  # Optimize some attributes by avoiding writing to the attributes key
  # in the bag table since we handle them internally.
  defp put_attribute(module, key, value, warn_line, traces, set, _bag)
       when key in [:doc, :typedoc, :moduledoc, :impl, :deprecated] do
    value = preprocess_attribute(key, value)

    try do
      :ets.lookup_element(set, key, 3)
    catch
      :error, :badarg -> :ok
    else
      unread_line when is_integer(warn_line) and is_integer(unread_line) ->
        message = "redefining @#{key} attribute previously set at line #{unread_line}"
        IO.warn(message, attribute_stack(module, warn_line))

      _ ->
        :ok
    end

    :ets.insert(set, {key, value, warn_line, traces})
  end

  defp put_attribute(_module, key, value, warn_line, traces, set, bag) do
    value = preprocess_attribute(key, value)

    try do
      :ets.lookup_element(set, key, 3)
    catch
      :error, :badarg ->
        :ets.insert(set, {key, value, warn_line, traces})
        :ets.insert(bag, {:warn_attributes, key})
    else
      :accumulate ->
        if traces != [] do
          :ets.update_element(set, key, {4, traces ++ :ets.lookup_element(set, key, 4)})
        end

        :ets.insert(bag, {{:accumulate, key}, value})

      _ ->
        :ets.insert(set, {key, value, warn_line, traces})
    end
  end

  defp attribute_stack(module, line) do
    struct!(Macro.Env, module: module, file: :elixir_module.file(module), line: line)
  end

  ## Helpers

  defp preprocess_attribute(key, value) when key in [:moduledoc, :typedoc, :doc] do
    case value do
      {line, doc} when is_integer(line) and (is_binary(doc) or doc == false or is_nil(doc)) ->
        value

      {line, doc} when is_integer(line) ->
        raise ArgumentError,
              "@#{key} is a built-in module attribute for documentation. It should be either " <>
                "false, nil, a string, or a keyword list, got: #{inspect(doc)}"

      _other ->
        raise ArgumentError,
              "@#{key} is a built-in module attribute for documentation. When set dynamically, " <>
                "it should be {line, doc} (where \"doc\" is either false, nil, a string, or a keyword list), " <>
                "got: #{inspect(value)}"
    end
  end

  defp preprocess_attribute(:behaviour, value) do
    if is_atom(value) do
      Code.ensure_compiled(value)
      value
    else
      raise ArgumentError, "@behaviour expects a module, got: #{inspect(value)}"
    end
  end

  defp preprocess_attribute(:impl, value) do
    if is_boolean(value) or (is_atom(value) and value != nil) do
      value
    else
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

  defp preprocess_attribute(:after_verify, atom) when is_atom(atom),
    do: {atom, :__after_verify__}

  defp preprocess_attribute(:on_definition, atom) when is_atom(atom),
    do: {atom, :__on_definition__}

  defp preprocess_attribute(key, _value)
       when key in [:type, :typep, :opaque, :spec, :callback, :macrocallback] do
    raise ArgumentError,
          "attributes type, typep, opaque, spec, callback, and macrocallback " <>
            "must be set directly via the @ notation"
  end

  defp preprocess_attribute(:external_resource, value) when not is_binary(value) do
    raise ArgumentError,
          "@external_resource is a built-in module attribute used for specifying file " <>
            "dependencies. It should be a string path to a file, got: #{inspect(value)}"
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
                "definition comes from. It should be a string or {string, line} tuple as value, " <>
                "got: #{inspect(value)}"
    end
  end

  defp preprocess_attribute(:nifs, value) do
    if not function_arity_list?(value) do
      raise ArgumentError,
            "@nifs is a built-in module attribute for specifying a list " <>
              "of functions and their arities that are NIFs, got: #{inspect(value)}"
    end

    value
  end

  defp preprocess_attribute(:dialyzer, value) do
    # From https://github.com/erlang/otp/blob/master/lib/stdlib/src/erl_lint.erl
    :lists.foreach(
      fn attr ->
        if not valid_dialyzer_attribute?(attr) do
          raise ArgumentError, "invalid value for @dialyzer attribute: #{inspect(attr)}"
        end
      end,
      List.wrap(value)
    )

    value
  end

  defp preprocess_attribute(_key, value) do
    value
  end

  defp function_arity_list?(fun_arities) do
    is_list(fun_arities) and
      :lists.all(
        fn
          {fun, arity} when is_atom(fun) and is_integer(arity) -> true
          _ -> false
        end,
        fun_arities
      )
  end

  defp valid_dialyzer_attribute?({key, fun_arities}) when is_atom(key) do
    (key == :nowarn_function or valid_dialyzer_attribute?(key)) and
      function_arity_list?(List.wrap(fun_arities))
  end

  defp valid_dialyzer_attribute?(attr) do
    :lists.member(
      attr,
      [:no_return, :no_unused, :no_improper_lists, :no_fun_app] ++
        [:no_match, :no_opaque, :no_fail_call, :no_contracts] ++
        [:no_behaviours, :no_undefined_callbacks, :unmatched_returns] ++
        [:error_handling, :race_conditions, :no_missing_calls] ++
        [:specdiffs, :overspecs, :underspecs, :unknown, :no_underspecs] ++
        [:extra_return, :no_extra_return, :no_missing_return] ++
        [:missing_return, :no_unknown]
    )
  end

  defp preprocess_doc_meta([], _module, _line, map), do: map

  defp preprocess_doc_meta([{key, _} | tail], module, line, map)
       when key in [:opaque, :defaults] do
    message = "ignoring reserved documentation metadata key: #{inspect(key)}"
    IO.warn(message, attribute_stack(module, line))
    preprocess_doc_meta(tail, module, line, map)
  end

  defp preprocess_doc_meta([{key, value} | tail], module, line, map) when is_atom(key) do
    validate_doc_meta(key, value)
    preprocess_doc_meta(tail, module, line, Map.put(map, key, value))
  end

  defp validate_doc_meta(:since, value) when not is_binary(value) do
    raise ArgumentError,
          ":since is a built-in documentation metadata key. It should be a string representing " <>
            "the version in which the documented entity was added, got: #{inspect(value)}"
  end

  defp validate_doc_meta(:deprecated, value) when not is_binary(value) do
    raise ArgumentError,
          ":deprecated is a built-in documentation metadata key. It should be a string " <>
            "representing the replacement for the deprecated entity, got: #{inspect(value)}"
  end

  defp validate_doc_meta(:delegate_to, value) do
    case value do
      {m, f, a} when is_atom(m) and is_atom(f) and is_integer(a) and a >= 0 ->
        :ok

      _ ->
        raise ArgumentError,
              ":delegate_to is a built-in documentation metadata key. It should be a three-element " <>
                "tuple in the form of {module, function, arity}, got: #{inspect(value)}"
    end
  end

  defp validate_doc_meta(_, _), do: :ok

  defp get_doc_info(table, env) do
    case :ets.take(table, :doc) do
      [{:doc, {_, _} = pair, _, _}] ->
        pair

      [] ->
        {env.line, nil}
    end
  end

  defp data_tables_for(module) do
    :elixir_module.data_tables(module)
  end

  defp bag_lookup_element(table, key, pos) do
    :ets.lookup_element(table, key, pos)
  catch
    :error, :badarg -> []
  end

  defp assert_not_compiled!(function_name_arity, module, extra_msg \\ "") do
    open?(module) ||
      raise ArgumentError,
            assert_not_compiled_message(function_name_arity, module, extra_msg)
  end

  defp assert_not_readonly!({function_name, arity}, module) do
    case :elixir_module.mode(module) do
      :all ->
        :ok

      :readonly ->
        raise ArgumentError,
              "could not call Module.#{function_name}/#{arity} because the module " <>
                "#{inspect(module)} is in read-only mode (@after_compile)"

      :closed ->
        raise ArgumentError,
              assert_not_compiled_message({function_name, arity}, module, "")
    end
  end

  defp assert_not_compiled_message({function_name, arity}, module, extra_msg) do
    mfa = "Module.#{function_name}/#{arity}"

    "could not call #{mfa} because the module #{inspect(module)} is already compiled" <>
      case extra_msg do
        "" -> ""
        _ -> ". " <> extra_msg
      end
  end
end
