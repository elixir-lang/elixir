defmodule Module do
  defmacrop is_env(env) do
    quote do
      is_tuple(unquote(env)) and size(unquote(env)) > 1 and elem(unquote(env), 0) == Macro.Env
    end
  end

  @moduledoc %B'''
  This module provides many functions to deal with modules during
  compilation time. It allows a developer to dynamically attach
  documentation, add, delete and register attributes and so forth.

  After a module is compiled, using many of the functions in
  this module will raise errors, since it is out of their purpose
  to inspect runtime data. Most of the runtime data can be inspected
  via the `__info__(attr)` function attached to each compiled module.

  ## Module attributes

  Each module can be decorated with one or more attributes. The following ones
  are currently defined by Elixir:

  * `@after_compile`

      A hook that will be invoked right after the current module is compiled.

      Accepts a module or a tuple `{ <module>, <function atom> }`. The function
      must take two arguments: the module environment and its bytecode.
      When just a module is provided, the function is assumed to be
      `__after_compile__/2`.

      **Example**

          defmodule M do
            @after_compile __MODULE__

            def __after_compile__(env, _bytecode) do
              IO.inspect env
            end
          end

  * `@before_compile`

      A hook that will be invoked before the module is compiled.

      Accepts a module or a tuple `{ <module>, <function/macro atom> }`. The
      function/macro must take one argument: the module environment. If it\'s a
      macro, its returned value will be injected at the end of the module definition
      before the compilation starts.

      When just a module is provided, the function/macro is assumed to be
      `__before_compile__/1`.

      **Example**

          defmodule M do
            @before_compile __MODULE__

            defmacro __before_compile__(_env) do
              quote do
                def hello, do: "world"
              end
            end
          end

  * `@behaviour`   (notice the british spelling)

      Specify an OTP or user-defined behaviour.

      **Example**

          defmodule M do
            @behaviour gen_event

            # ...
          end

  * `@compile`

      Define options for module compilation that are passed to the Erlang
      compiler.

      Accepts an atom, a tuple, or a list of atoms and tuples.

      See http://www.erlang.org/doc/man/compile.html for the list of supported
      options.

      **Example**

            defmodule M do
              @compile { :inline, myfun: 1 }

              def myfun(arg) do
                to_binary(arg)
              end
            end

  * `@doc`

      Provide documentation for the function or macro that follows the
      attribute.

      Accepts a string (often a heredoc) or `false` where `@doc false` will
      make the function/macro invisible to the documentation extraction tools
      like ExDoc.

      Can be invoked more than once.

      **Example**

            defmodule M do
              @doc "Hello world"
              def hello do
                "world"
              end

              @doc """
              Sum.
              """
              def sum(a, b) do
                a + b
              end
            end

  * `@file`

      Change the filename used in stacktraces for the function or macro that
      follows the attribute.

      Accepts a string. Can be used more than once.

      **Example**

            defmodule M do
              @doc "Hello world"
              @file "hello.ex"
              def hello do
                "world"
              end
            end

  * `@moduledoc`

      Provide documentation for the current module.

      Accepts a string (which is often a heredoc) or `false` where
      `@moduledoc false` will make the module invisible to the
      documentation extraction tools like ExDoc.

      **Example**

            defmodule M do
              @moduledoc """
              A very useful module
              """
            end


  * `@on_definition`

      A hook that will be invoked after each function or macro in the current
      module is defined. Useful when annotating functions.

      Accepts a module or a tuple `{ <module>, <function atom> }`. The function
      must take 6 arguments:

        - the module environment
        - kind: `:def`, `:defp`, `:defmacro`, or `:defmacrop`
        - function/macro name
        - list of quoted arguments
        - list of quoted guards
        - quoted function body

      If the function/macro being defined has multiple clauses, the hook will
      be called for each clause.

      When just a module is provided, the function is assumed to be
      `__on_definition__/6`.

      Note that you can\'t provide the current module to `@on_definition`
      because the hook function will not be defined in time. Finally, since
      the `on_definition` hook is executed inside the context of the defined
      function (i.e. `env.function` returns the current function), the hook
      can only be a function, not a macro.

      **Example**

            defmodule H do
              def on_def(_env, kind, name, args, guards, body) do
                IO.puts "Defining #{kind} named #{name} with args:"
                IO.inspect args
                IO.puts "and guards"
                IO.inspect guards
                IO.puts "and body"
                IO.puts Macro.to_string(body)
              end
            end

            defmodule M do
              @on_definition { H, :on_def }

              def hello(arg) when is_binary(arg) or is_list(arg) do
                "Hello" <> to_binary(arg)
              end

              def hello(_) do
                :ok
              end
            end

  * `@on_load`

      A hook that will be invoked whenever the module is loaded.

      Accepts a function atom of a function in the current module. The function
      must have arity 0 (no arguments) and has to return `:ok`, otherwise the
      loading of the module will be aborted.

      **Example**

            defmodule M do
              @on_load :load_check

              def load_check do
                if some_condition() do
                  :ok
                else
                  nil
                end
              end

              def some_condition do
                false
              end
            end

  * `@vsn`

      Specify the module version. Accepts any valid Elixir value.

      **Example**

            defmodule M do
              @vsn "1.0"
            end

  The following attributes are part of typespecs and are also reserved by
  Elixir (see `Kernel.Typespec` for more information about typespecs):

  * `@type`        - defines a type to be used in `@spec`
  * `@typep`       - defines a private type to be used in `@spec`
  * `@opaque`      - defines an opaque type to be used in `@spec`
  * `@spec`        - provides a specification for a function
  * `@callback`    - provides a specification for the behavior callback

  In addition to the built-in attributes outlined above, custom attributes may
  also be added. A custom attribute is any valid identifier prefixed with an
  `@` and followed by a valid Elixir value:

        defmodule M do
          @custom_attr [some: "stuff"]
        end

  For more advanced options available when defining custom attributes, see
  `register_attribute/3`.

  ## Runtime information about a module

  It is possible to query a module at runtime to find out which functions and
  macros it defines, extract its docstrings, etc. See `__info__/1`.
  '''

  @doc """
  Provides runtime information about functions and macros defined by the
  module, enables docstring extraction, etc.

  Each module gets an `__info__/1` function when it's compiled. The function
  takes one of the following atoms:

  * `:functions`  - keyword list of public functions along with their arities

  * `:macros`     - keyword list of public macros along with their arities

  * `:docs`       - list of all docstrings attached to functions and macros
                    using the `@doc` attribute

  * `:moduledoc`  - tuple `{ <line>, <doc> }` where `line` is the line on
                    which module definition starts and `doc` is the string
                    attached to the module using the `@moduledoc` attribute

  * `:module`     - module name (`Module == Module.__info__(:module)`)

  In addition to the above, you may also pass to `__info__/1` any atom supported
  by Erlang's `module_info` function which also gets defined for each compiled
  module. See http://erlang.org/doc/reference_manual/modules.html#id74571 for
  more information.
  """
  def __info__(kind)

  @doc """
  Check if a module is open, i.e. it is currently being defined
  and its attributes and functions can be modified.
  """
  def open?(module) do
    table = data_table_for(module)
    table == :ets.info(table, :name)
  end

  @doc """
  Evaluates the quoted contents in the given module's context.

  A list of environment options can also be given as argument.
  See `Code.eval_string` for more information.

  Raises an error if the module was already compiled.

  ## Examples

      defmodule Foo do
        contents = quote do: (def sum(a, b), do: a + b)
        Module.eval_quoted __MODULE__, contents
      end

      Foo.sum(1, 2) #=> 3

  For convenience, you can my pass `__ENV__` as argument and
  all options will be automatically extracted from the environment:

      defmodule Foo do
        contents = quote do: (def sum(a, b), do: a + b)
        Module.eval_quoted __MODULE__, contents, [], __ENV__
      end

      Foo.sum(1, 2) #=> 3

  """
  def eval_quoted(module, quoted, binding // [], opts // [])

  def eval_quoted(env, quoted, binding, opts) when is_env(env) do
    eval_quoted(env.module, quoted, binding, Keyword.merge(env.to_keywords, opts))
  end

  def eval_quoted(module, quoted, binding, env) when is_env(env) do
    eval_quoted(module, quoted, binding, env.to_keywords)
  end

  def eval_quoted(module, quoted, binding, opts) do
    assert_not_compiled!(:eval_quoted, module)
    :elixir_module.eval_quoted(module, quoted, binding, opts)
  end

  @doc """
  Creates a module with the given name and given by
  the given quoted expressions. The line where the module
  is defined and its file can be given as options.

  ## Examples

      contents =
        quote do
          def world, do: true
        end

      Module.create(Hello, contents, __ENV__.location)

      Hello.world #=> true

  ## Differences with `defmodule`

  `Module.create` works similarly to `defmodule` and
  return the same results. While one could also use
  `defmodule` to define modules dynamically, this
  function is preferred when the module body is given
  by a quoted expression.

  Another important distinction is that `Module.create`
  allows you to control the environment variables used
  when defining the module, while `defmodule` automatically
  shares the same environment.
  """
  def create(module, quoted, opts // [])

  def create(module, quoted, env) when is_env(env) do
    create(module, quoted, env.to_keywords)
  end

  def create(module, quoted, opts) when is_atom(module) do
    line = Keyword.get(opts, :line, 1)
    :elixir_module.compile(line, module, quoted, [], :elixir.scope_for_eval(opts))
  end

  @doc """
  Concatenates the list of aliases and returns a new alias.
  It handles char lists, binaries and atoms.

  ## Examples

      iex> Module.concat([Foo, Bar])
      Foo.Bar
      iex> Module.concat([Foo, "Bar"])
      Foo.Bar
      iex> Module.concat([Foo, 'Bar'])
      Foo.Bar

  """
  def concat(list) when is_list(list) do
    :elixir_aliases.concat(list)
  end

  @doc """
  Concatenates the two given aliases and returns a new alias.
  It handles char lists, binaries and atoms.

  ## Examples

      iex> Module.concat(Foo, Bar)
      Foo.Bar
      iex> Module.concat(Foo, "Bar")
      Foo.Bar
      iex> Module.concat(Foo, 'Bar')
      Foo.Bar

  """
  def concat(left, right) do
    :elixir_aliases.concat([left, right])
  end

  @doc """
  Concatenates the list aliases and returns a new alias only
  if the alias was already referenced. If the alias was not
  referenced yet, fails with ArgumentError.
  It handles char lists, binaries and atoms.

  ## Examples

      iex> Module.safe_concat([Unknown, Module])
      ** (ArgumentError) argument error

      iex> Module.safe_concat([List, Chars])
      List.Chars

  """
  def safe_concat(list) when is_list(list) do
    :elixir_aliases.safe_concat(list)
  end

  @doc """
  Concatenates the two aliases and returns a new alias only
  if the alias was already referenced. If the alias was not
  referenced yet, fails with ArgumentError.
  It handles char lists, binaries and atoms.

  ## Examples

      iex> Module.safe_concat(Unknown, Module)
      ** (ArgumentError) argument error

      iex> Module.safe_concat(List, Chars)
      List.Chars

  """
  def safe_concat(left, right) do
    :elixir_aliases.safe_concat([left, right])
  end

  @doc """
  Gets an anonymous function from the given module, function
  and arity. The module and function are not verified to exist.

      iex> fun = Module.function(Kernel, :is_atom, 1)
      iex> fun.(:hello)
      true

  """
  def function(mod, fun, arity) do
    :erlang.make_fun(mod, fun, arity)
  end

  @doc """
  Attaches documentation to a given function. It expects
  the module the function belongs to, the line (a non negative
  integer), the kind (def or defmacro), a tuple representing
  the function and its arity and the documentation, which should
  be either a binary or a boolean.

  ## Examples

      defmodule MyModule do
        Module.add_doc(__MODULE__, __ENV__.line + 1, :def, { :version, 0 }, [], "Manually added docs")
        def version, do: 1
      end

  """
  def add_doc(_module, _line, kind, _tuple, _signature, doc) when kind in [:defp, :defmacrop] do
    if doc, do: { :error, :private_doc }, else: :ok
  end

  def add_doc(module, line, kind, tuple, signature, doc) when
      kind in [:def, :defmacro] and (is_binary(doc) or is_boolean(doc) or doc == nil) do
    assert_not_compiled!(:add_doc, module)
    table = docs_table_for(module)

    { signature, _ } = Enum.map_reduce signature, 1, fn(x, acc) ->
      { simplify_signature(x, line, acc), acc + 1 }
    end

    case :ets.lookup(table, tuple) do
      [] ->
        :ets.insert(table, { tuple, line, kind, signature, doc })
        :ok
      [{ tuple, line, _old_kind, old_sign, old_doc }] ->
        :ets.insert(table, {
          tuple,
          line,
          kind,
          merge_signatures(old_sign, signature, 1),
          if(nil?(doc), do: old_doc, else: doc)
        })
        :ok
    end
  end

  # Simplify signatures to be stored in docs

  defp simplify_signature({ ://, defline, [left, right ] }, line, i) do
    { ://, defline, [simplify_signature(left, line, i), right] }
  end

  defp simplify_signature({ var, line, atom }, _, _i) when is_atom(atom) do
    case atom_to_list(var) do
      [?_|_]    -> { var, line, :guess }
      _         -> { var, line, nil }
    end
  end

  defp simplify_signature({ :=, _, [_, right] }, line, i) do
    simplify_signature(right, line, i)
  end

  defp simplify_signature(other, line, i) when is_integer(other), do: { :"int#{i}", line, :guess }
  defp simplify_signature(other, line, i) when is_boolean(other), do: { :"bool#{i}", line, :guess }
  defp simplify_signature(other, line, i) when is_atom(other),    do: { :"atom#{i}", line, :guess }
  defp simplify_signature(other, line, i) when is_list(other),    do: { :"list#{i}", line, :guess }
  defp simplify_signature(other, line, i) when is_float(other),   do: { :"float#{i}", line, :guess }
  defp simplify_signature(other, line, i) when is_binary(other),  do: { :"binary#{i}", line, :guess }
  defp simplify_signature(_, line, i), do: { :"arg#{i}", line, :guess }

  # Merge

  defp merge_signatures([h1|t1], [h2|t2], i) do
    [merge_signature(h1, h2, i)|merge_signatures(t1, t2, i + 1)]
  end

  defp merge_signatures([], [], _) do
    []
  end

  defp merge_signature({ ://, line, [left, right] }, newer, i) do
    { ://, line, [merge_signature(left, newer, i), right] }
  end

  defp merge_signature(older, { ://, _, [left, _] }, i) do
    merge_signature(older, left, i)
  end

  # The older signature, when given, always have higher precedence
  defp merge_signature({ _, _, nil } = older, _newer, _),        do: older
  defp merge_signature(_older, { _, _, nil } = newer, _),        do: newer

  # Both are a guess, so check if they are the same guess
  defp merge_signature({ var, _, _ } = older, { var, _, _ }, _), do: older

  # Otherwise, returns a generic guess
  defp merge_signature({ _, line, _ }, _newer, i), do: { :"arg#{i}", line, :guess }

  @doc """
  Checks if the module defines the given function or macro.
  Use `defines?/3` to assert for an specific type.

  ## Examples

      defmodule Example do
        Module.defines? __MODULE__, { :version, 0 } #=> false
        def version, do: 1
        Module.defines? __MODULE__, { :version, 0 } #=> true
      end

  """
  def defines?(module, tuple) when is_tuple(tuple) do
    assert_not_compiled!(:defines?, module)
    table = function_table_for(module)
    :ets.lookup(table, tuple) != []
  end

  @doc """
  Checks if the module defines a function or macro with the
  given `kind`. `kind` can be either `:def`, `:defp`,
  `:defmacro` or `:defmacrop`.

  ## Examples

      defmodule Example do
        Module.defines? __MODULE__, { :version, 0 }, :defp #=> false
        def version, do: 1
        Module.defines? __MODULE__, { :version, 0 }, :defp #=> false
      end

  """
  def defines?(module, tuple, kind) do
    assert_not_compiled!(:defines?, module)
    table = function_table_for(module)
    case :ets.lookup(table, tuple) do
      [{ _, ^kind, _, _, _, _, _ }] -> true
      _ -> false
    end
  end

  @doc """
  Return all functions defined in the given module.

  ## Examples

      defmodule Example do
        def version, do: 1
        Module.definitions_in __MODULE__ #=> [{:version,1}]
      end

  """
  def definitions_in(module) do
    assert_not_compiled!(:definitions_in, module)
    table = function_table_for(module)
    lc { tuple, _, _, _, _, _, _ } inlist :ets.tab2list(table), do: tuple
  end

  @doc """
  Returns all functions defined in te given module according
  to its kind.

  ## Examples

      defmodule Example do
        def version, do: 1
        Module.definitions_in __MODULE__, :def  #=> [{:version,1}]
        Module.definitions_in __MODULE__, :defp #=> []
      end

  """
  def definitions_in(module, kind) do
    assert_not_compiled!(:definitions_in, module)
    table = function_table_for(module)
    lc { tuple, stored_kind, _, _, _, _, _ } inlist :ets.tab2list(table), stored_kind == kind, do: tuple
  end

  @doc """
  Makes the given functions in the given module overridable.
  An overridable function is lazily defined, allowing a
  developer to customize it. See `Kernel.defoverridable` for
  more information and documentation.
  """
  def make_overridable(module, tuples) do
    assert_not_compiled!(:make_overridable, module)

    lc tuple inlist tuples do
      case :elixir_def.lookup_definition(module, tuple) do
        false ->
          { name, arity } = tuple
          raise "Cannot make function #{name}/#{arity} overridable because it was not defined"
        clause ->
          :elixir_def.delete_definition(module, tuple)
          neighbours = Module.DispatchTracker.yank(module, tuple)

          old    = get_attribute(module, :__overridable)
          merged = :orddict.update(tuple, fn({ count, _, _, _ }) ->
            { count + 1, clause, neighbours, false }
          end, { 1, clause, neighbours, false }, old)

          put_attribute(module, :__overridable, merged)
      end
    end
  end

  @doc """
  Returns true if the given tuple in module is marked as overridable.
  """
  def overridable?(module, tuple) do
    !! List.keyfind(get_attribute(module, :__overridable), tuple, 0)
  end

  @doc """
  Puts an Erlang attribute to the given module with the given
  key and value. The semantics of putting the attribute depends
  if the attribute was registered or not via `register_attribute/2`.

  ## Examples

      defmodule MyModule do
        Module.put_attribute __MODULE__, :custom_threshold_for_lib, 10
      end

  """
  def put_attribute(module, key, value) when is_atom(key) do
    assert_not_compiled!(:put_attribute, module)
    table = data_table_for(module)
    value = normalize_attribute(key, value)
    acc   = :ets.lookup_element(table, :__acc_attributes, 2)

    new =
      if :lists.member(key, acc) do
        case :ets.lookup(table, key) do
          [{^key, old}] -> [value|old]
          [] -> [value]
        end
      else
        value
      end

    :ets.insert(table, { key, new })
  end

  @doc """
  Gets the given attribute from a module. If the attribute
  was marked with `accumulate` with `Module.register_attribute`,
  a list is always returned.

  The `@` macro compiles to a call to this function. For example,
  the following code:

      @foo

  Expands to:

      Module.get_attribute(__MODULE__, :foo, true)

  Notice the third argument is used to indicate if a warning
  should be emitted when the attribute was not previously defined.
  This is true for `@foo` attributes but false for direct calls.

  ## Examples

      defmodule Foo do
        Module.put_attribute __MODULE__, :value, 1
        Module.get_attribute __MODULE__, :value #=> 1

        Module.register_attribute __MODULE__, :value, accumulate: true
        Module.put_attribute __MODULE__, :value, 1
        Module.get_attribute __MODULE__, :value #=> [1]
      end

  """
  def get_attribute(module, key, warn // false) when is_atom(key) do
    assert_not_compiled!(:get_attribute, module)
    table = data_table_for(module)

    case :ets.lookup(table, key) do
      [{^key, val}] -> val
      [] ->
        acc = :ets.lookup_element(table, :__acc_attributes, 2)

        if :lists.member(key, acc) do
          []
        else
          warn && :elixir_errors.warn "#{Exception.format_caller} undefined module attribute @#{key}, " <>
            "please remove access to @#{key} or explicitly set it to nil before access\n"
          nil
        end
    end
  end

  @doc """
  Deletes all attributes that matches the given key.

  ## Examples

      defmodule MyModule do
        Module.put_attribute __MODULE__, :custom_threshold_for_lib, 10
        Module.delete_attribute __MODULE__, :custom_threshold_for_lib
      end

  """
  def delete_attribute(module, key) when is_atom(key) do
    assert_not_compiled!(:delete_attribute, module)
    table = data_table_for(module)
    :ets.delete(table, key)
  end

  @doc """
  Registers an attribute. By registering an attribute, a developer
  is able to customize how Elixir will store and accumulate the
  attribute values.

  ## Options

  When registering an attribute, two options can be given:

  * `:accumulate` - Several calls to the same attribute will
    accumulate instead of override the previous one. New attributes
    are always added to the top of the accumulated list.

  * `:persist` - The attribute will be persisted in the Erlang
    Abstract Format. Useful when interfacing with Erlang libraries.

  By default, both options are false.

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
  def register_attribute(module, new, opts) when is_atom(new) do
    assert_not_compiled!(:register_attribute, module)
    table = data_table_for(module)

    if Keyword.get(opts, :persist) do
      old = :ets.lookup_element(table, :__persisted_attributes, 2)
      :ets.insert(table, { :__persisted_attributes,  [new|old] })
    end

    if Keyword.get(opts, :accumulate) do
      old = :ets.lookup_element(table, :__acc_attributes, 2)
      :ets.insert(table, { :__acc_attributes,  [new|old] })
    end
  end

  @doc """
  Split the given module name into binary parts.

  ## Examples

      Module.split Very.Long.Module.Name.And.Even.Longer
      #=> ["Very", "Long", "Module", "Name", "And", "Even", "Longer"]

  """
  def split(module) do
    tl(String.split(Binary.Chars.to_binary(module), "."))
  end

  @doc false
  # Used internally to compile documentation. This function
  # is private and must be used only internally.
  def compile_doc(env, kind, name, args, _guards, _body) do
    module = env.module
    line   = env.line
    arity  = length(args)
    pair   = { name, arity }
    doc    = get_attribute(module, :doc)

    case add_doc(module, line, kind, pair, args, doc) do
      :ok ->
        :ok
      { :error, :private_doc } ->
        :elixir_errors.warn "#{env.file}:#{line} function #{name}/#{arity} is private, @doc's are always discarded for private functions\n"
    end

    delete_attribute(module, :doc)
  end

  @doc false
  # Used internally to compile types. This function
  # is private and must be used only internally.
  def compile_typespec(module, key, value) when is_atom(key) do
    assert_not_compiled!(:put_attribute, module)
    table = data_table_for(module)

    new =
      case :ets.lookup(table, key) do
        [{^key, old}] -> [value|old]
        [] -> [value]
      end

    :ets.insert(table, { key, new })
  end

  ## Helpers

  defp normalize_attribute(:on_load, atom) when is_atom(atom) do
    { atom, 0 }
  end

  defp normalize_attribute(kind, atom) when kind in [:behavior, :behaviour] and is_atom(atom) do
    Code.ensure_compiled(atom)
    atom
  end

  defp normalize_attribute(:file, env) when is_env(env),                    do: { binary_to_list(env.file), env.line }
  defp normalize_attribute(:file, { binary, line }) when is_binary(binary), do: { binary_to_list(binary), line }
  defp normalize_attribute(:file, other) when not is_tuple(other),          do: normalize_attribute(:file, { other, 1 })

  defp normalize_attribute(key, atom) when is_atom(atom) and
      key in [:before_compile, :after_compile, :on_definition] do
    { atom, :"__#{key}__" }
  end

  defp normalize_attribute(key, _value) when key in [:type, :typep, :export_type, :opaque, :callback] do
    raise ArgumentError, message: "Attributes type, typep, export_type, opaque and callback " <>
      "must be set via Kernel.Typespec"
  end

  defp normalize_attribute(_key, value) do
    value
  end

  defp data_table_for(module) do
    module
  end

  defp function_table_for(module) do
    :elixir_def.table(module)
  end

  defp docs_table_for(module) do
    :elixir_module.docs_table(module)
  end

  defp assert_not_compiled!(fun, module) do
    open?(module) ||
      raise ArgumentError,
        message: "could not call #{fun} on module #{inspect module} because it was already compiled"
  end
end
