defmodule Module do
  @moduledoc ~S'''
  This module provides many functions to deal with modules during
  compilation time. It allows a developer to dynamically attach
  documentation, add, delete and register attributes and so forth.

  After a module is compiled, using many of the functions in
  this module will raise errors, since it is out of their scope
  to inspect runtime data. Most of the runtime data can be inspected
  via the `__info__(attr)` function attached to each compiled module.

  ## Module attributes

  Each module can be decorated with one or more attributes. The following ones
  are currently defined by Elixir:

    * `@after_compile`

      A hook that will be invoked right after the current module is compiled.

      Accepts a module or a tuple `{<module>, <function atom>}`. The function
      must take two arguments: the module environment and its bytecode.
      When just a module is provided, the function is assumed to be
      `__after_compile__/2`.

      ### Example

          defmodule M do
            @after_compile __MODULE__

            def __after_compile__(env, _bytecode) do
              IO.inspect env
            end
          end

    * `@before_compile`

      A hook that will be invoked before the module is compiled.

      Accepts a module or a tuple `{<module>, <function/macro atom>}`. The
      function/macro must take one argument: the module environment. If it's a
      macro, its returned value will be injected at the end of the module definition
      before the compilation starts.

      When just a module is provided, the function/macro is assumed to be
      `__before_compile__/1`.

      Note: unlike `@after_compile`, the callback function/macro must
      be placed in a separate module (because when the callback is invoked,
      the current module does not yet exist).

      ### Example

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

    * `@behaviour`   (notice the British spelling)

      Specifies an OTP or user-defined behaviour.

      ### Example

          defmodule M do
            @behaviour gen_event

            # ...
          end

    * `@compile`

      Defines options for module compilation that are passed to the Erlang
      compiler.

      Accepts an atom, a tuple, or a list of atoms and tuples.

      See http://www.erlang.org/doc/man/compile.html for the list of supported
      options.

      Several uses of `@compile` will accumulate instead of overriding
      previous ones.

      ### Example

            defmodule M do
              @compile {:inline, myfun: 1}

              def myfun(arg) do
                to_string(arg)
              end
            end

    * `@doc`

      Provides documentation for the function or macro that follows the
      attribute.

      Accepts a string (often a heredoc) or `false` where `@doc false` will
      make the function/macro invisible to the documentation extraction tools
      like ExDoc.

      Can be invoked more than once.

      ### Example

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

      Changes the filename used in stacktraces for the function or macro that
      follows the attribute.

      Accepts a string. Can be used more than once.

      ### Example

            defmodule M do
              @doc "Hello world"
              @file "hello.ex"
              def hello do
                "world"
              end
            end

    * `@moduledoc`

      Provides documentation for the current module.

      Accepts a string (which is often a heredoc) or `false` where
      `@moduledoc false` will make the module invisible to the
      documentation extraction tools like ExDoc.

      ### Example

            defmodule M do
              @moduledoc """
              A very useful module
              """
            end


    * `@on_definition`

      A hook that will be invoked when each function or macro in the current
      module is defined. Useful when annotating functions.

      Accepts a module or a tuple `{<module>, <function atom>}`. The function
      must take 6 arguments:

        - the module environment
        - kind: `:def`, `:defp`, `:defmacro`, or `:defmacrop`
        - function/macro name
        - list of quoted arguments
        - list of quoted guards
        - quoted function body

      Note the hook receives the quoted arguments and it is invoked before
      the function is stored in the module. So `Module.defines?/2` will return
      `false` for the first clause of every function.

      If the function/macro being defined has multiple clauses, the hook will
      be called for each clause.

      Unlike other hooks, `@on_definition` will only invoke functions
      and never macros. This is because the hook is invoked inside the context
      of the function (and nested function definitions are not allowed in
      Elixir).

      When just a module is provided, the function is assumed to be
      `__on_definition__/6`.

      ### Example

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
              @on_definition {H, :on_def}

              def hello(arg) when is_binary(arg) or is_list(arg) do
                "Hello" <> to_string(arg)
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

      ### Example

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

      ### Example

            defmodule M do
              @vsn "1.0"
            end

    * `@external_resource`

      Specifies an external resource to the current module.

      Many times a module embeds information from an external file. This
      attribute allows the module to annotate which external resources
      have been used.

      Tools like Mix may use this information to ensure the module is
      recompiled in case any of the external resources change.

    * `@dialyzer`

      Defines warnings to request or suppress when using a version of
      `dialyzer` that supports module attributes.

      Accepts an atom, a tuple, or a list of atoms and tuples.

      See http://www.erlang.org/doc/man/dialyzer.html for the list of supported
      warnings.

      Several uses of `@dialyzer` will accumulate instead of overriding
      previous ones.

      ### Example

            defmodule M do
              @dialyzer {:nowarn_function, myfun: 1}

              def myfun(arg) do
                M.not_a_function(arg)
              end
            end

  The following attributes are part of typespecs and are also reserved by
  Elixir (see `Kernel.Typespec` for more information about typespecs):

    * `@type`          - defines a type to be used in `@spec`
    * `@typep`         - defines a private type to be used in `@spec`
    * `@opaque`        - defines an opaque type to be used in `@spec`
    * `@spec`          - provides a specification for a function
    * `@callback`      - provides a specification for a behaviour callback
    * `@macrocallback` - provides a specification for a macro behaviour callback

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

    * `:module`     - module name (`Module == Module.__info__(:module)`)

  In addition to the above, you may also pass to `__info__/1` any atom supported
  by Erlang's `module_info` function which also gets defined for each compiled
  module. See http://www.erlang.org/doc/reference_manual/modules.html#id75777 for
  more information.
  """
  def __info__(kind)

  @doc """
  Checks if a module is open, i.e. it is currently being defined
  and its attributes and functions can be modified.
  """
  def open?(module) do
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

  For convenience, you can pass `__ENV__` as an argument and
  all options will be automatically extracted from the environment:

      defmodule Foo do
        contents = quote do: (def sum(a, b), do: a + b)
        Module.eval_quoted __MODULE__, contents, [], __ENV__
      end

      Foo.sum(1, 2) #=> 3

  """
  def eval_quoted(module, quoted, binding \\ [], opts \\ [])

  def eval_quoted(%Macro.Env{} = env, quoted, binding, opts) do
    eval_quoted(env.module, quoted, binding, Keyword.merge(Map.to_list(env), opts))
  end

  def eval_quoted(module, quoted, binding, %Macro.Env{} = env) do
    eval_quoted(module, quoted, binding, Map.to_list(env))
  end

  def eval_quoted(module, quoted, binding, opts) do
    assert_not_compiled!(:eval_quoted, module)
    :elixir_def.reset_last(module)
    {value, binding, _env, _scope} =
      :elixir.eval_quoted quoted, binding, Keyword.put(opts, :module, module)
    {value, binding}
  end

  @doc """
  Creates a module with the given name and defined by
  the given quoted expressions.

  The line where the module is defined and its file **must**
  be passed as options.

  ## Examples

      contents =
        quote do
          def world, do: true
        end

      Module.create(Hello, contents, Macro.Env.location(__ENV__))

      Hello.world #=> true

  ## Differences from `defmodule`

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
  def create(module, quoted, opts)

  def create(module, quoted, %Macro.Env{} = env) do
    create(module, quoted, Map.to_list(env))
  end

  def create(module, quoted, opts) when is_atom(module) and is_list(opts) do
    unless Keyword.has_key?(opts, :file) do
      raise ArgumentError, "expected :file to be given as option"
    end
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
  def concat(left, right) do
    :elixir_aliases.concat([left, right])
  end

  @doc """
  Concatenates a list of aliases and returns a new alias only
  if the alias was already referenced. If the alias was not
  referenced yet, fails with `ArgumentError`.
  It handles char lists, binaries and atoms.

  ## Examples

      iex> Module.safe_concat([Unknown, Module])
      ** (ArgumentError) argument error

      iex> Module.safe_concat([List, Chars])
      List.Chars

  """
  @spec safe_concat([binary | atom]) :: atom | no_return
  def safe_concat(list) when is_list(list) do
    :elixir_aliases.safe_concat(list)
  end

  @doc """
  Concatenates two aliases and returns a new alias only
  if the alias was already referenced. If the alias was not
  referenced yet, fails with `ArgumentError`.
  It handles char lists, binaries and atoms.

  ## Examples

      iex> Module.safe_concat(Unknown, Module)
      ** (ArgumentError) argument error

      iex> Module.safe_concat(List, Chars)
      List.Chars

  """
  @spec safe_concat(binary | atom, binary | atom) :: atom | no_return
  def safe_concat(left, right) do
    :elixir_aliases.safe_concat([left, right])
  end

  @doc """
  Attaches documentation to a given function or type. It expects
  the module the function/type belongs to, the line (a non negative
  integer), the kind (`def` or `defmacro`), a tuple representing
  the function and its arity, the function signature (the signature
  should be omitted for types) and the documentation, which should
  be either a binary or a boolean.

  ## Examples

      defmodule MyModule do
        Module.add_doc(__MODULE__, __ENV__.line + 1, :def, {:version, 0}, [], "Manually added docs")
        def version, do: 1
      end

  """
  def add_doc(module, line, kind, tuple, signature \\ [], doc)

  def add_doc(_module, _line, kind, _tuple, _signature, doc) when kind in [:defp, :defmacrop, :typep] do
    if doc, do: {:error, :private_doc}, else: :ok
  end

  def add_doc(module, line, kind, tuple, signature, doc) when
      kind in [:def, :defmacro, :type, :opaque] and (is_binary(doc) or is_boolean(doc) or doc == nil) do
    assert_not_compiled!(:add_doc, module)
    table = data_table_for(module)

    signature = simplify_signature(signature)

    case :ets.lookup(table, {:doc, tuple}) do
      [] ->
        :ets.insert(table, {{:doc, tuple}, line, kind, signature, doc})
        :ok
      [{doc_tuple, line, _old_kind, old_sign, old_doc}] ->
        :ets.insert(table, {
          doc_tuple,
          line,
          kind,
          merge_signatures(old_sign, signature, 1),
          if(is_nil(doc), do: old_doc, else: doc)
       })
        :ok
    end
  end

  # Simplify signatures to be stored in docs

  defp simplify_signature(signature) do
    {signature, acc} = :lists.mapfoldl(&simplify_signature/2, [], signature)
    {signature, _} = :lists.mapfoldl(&expand_signature/2, {acc, acc}, signature)
    signature
  end

  defp simplify_signature({:\\, _, [left, right ]}, acc) do
    {left, acc} = simplify_signature(left, acc)
    {{:\\, [], [left, right]}, acc}
  end

  defp simplify_signature({:=, _, [_, right]}, acc) do
    simplify_signature(right, acc)
  end

  defp simplify_signature({var, _, atom}, acc) when is_atom(atom) do
    case Atom.to_string(var) do
      "_" <> rest -> {{String.to_atom(rest), [], Elixir}, acc}
      _           -> {{var, [], nil}, acc}
    end
  end

  defp simplify_signature({:%, _, [left, _]}, acc) when is_atom(left) do
    struct_name = String.to_atom(camelcase_to_underscore(List.last(split(left))))
    autogenerated(acc, struct_name)
  end

  defp simplify_signature({:%{}, _, _}, acc) do
    autogenerated(acc, :map)
  end

  defp simplify_signature(other, acc) when is_integer(other), do: autogenerated(acc, :int)
  defp simplify_signature(other, acc) when is_boolean(other), do: autogenerated(acc, :bool)
  defp simplify_signature(other, acc) when is_atom(other),    do: autogenerated(acc, :atom)
  defp simplify_signature(other, acc) when is_list(other),    do: autogenerated(acc, :list)
  defp simplify_signature(other, acc) when is_float(other),   do: autogenerated(acc, :float)
  defp simplify_signature(other, acc) when is_binary(other),  do: autogenerated(acc, :binary)
  defp simplify_signature(_, acc),                            do: autogenerated(acc, :arg)

  defp autogenerated(acc, key) do
    {key, [key|acc]}
  end

  defp expand_signature(key, {all_keys, acc}) when is_atom(key) do
    case previous_values(key, all_keys, acc) do
      {i, acc} -> {{:"#{key}#{i}", [], Elixir}, {all_keys, acc}}
      :none    -> {{key, [], Elixir}, {all_keys, acc}}
    end
  end

  defp expand_signature(term, {_, _} = acc) do
    {term, acc}
  end

  defp previous_values(key, all_keys, acc) do
    total_occurrences = occurrences(key, all_keys)

    if total_occurrences == 1 do
      :none
    else
      index = total_occurrences - occurrences(key, acc) + 1
      {index, :lists.delete(key, acc)}
    end
  end

  defp occurrences(key, list) do
    length(:lists.filter(fn(el) -> el == key end, list))
  end

  defp camelcase_to_underscore(<<c :: utf8, rest :: binary>>) when c >= ?A and c <= ?Z,
    do: do_camelcase_to_underscore(rest, <<c + 32 :: utf8>>)
  defp do_camelcase_to_underscore(<<c :: utf8, rest :: binary>>, acc) when c >= ?A and c <= ?Z,
    do: do_camelcase_to_underscore(rest, <<acc :: binary, ?_, c + 32 :: utf8>>)
  defp do_camelcase_to_underscore(<<c :: utf8, rest :: binary>>, acc),
    do: do_camelcase_to_underscore(rest, <<acc :: binary, c>>)
  defp do_camelcase_to_underscore(<<>>, acc),
    do: acc

  # Merge

  defp merge_signatures([h1|t1], [h2|t2], i) do
    [merge_signature(h1, h2, i)|merge_signatures(t1, t2, i + 1)]
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
  defp merge_signature({_, _, nil} = older, _newer, _),        do: older
  defp merge_signature(_older, {_, _, nil} = newer, _),        do: newer

  # Both are a guess, so check if they are the same guess
  defp merge_signature({var, _, _} = older, {var, _, _}, _), do: older

  # Otherwise, returns a generic guess
  defp merge_signature({_, line, _}, _newer, i), do: {:"arg#{i}", line, Elixir}

  @doc """
  Checks if the module defines the given function or macro.
  Use `defines?/3` to assert for a specific type.

  ## Examples

      defmodule Example do
        Module.defines? __MODULE__, {:version, 0} #=> false
        def version, do: 1
        Module.defines? __MODULE__, {:version, 0} #=> true
      end

  """
  def defines?(module, tuple) when is_tuple(tuple) do
    assert_not_compiled!(:defines?, module)
    table = defs_table_for(module)
    :ets.lookup(table, tuple) != []
  end

  @doc """
  Checks if the module defines a function or macro of the
  given `kind`. `kind` can be any of `:def`, `:defp`,
  `:defmacro` or `:defmacrop`.

  ## Examples

      defmodule Example do
        Module.defines? __MODULE__, {:version, 0}, :defp #=> false
        def version, do: 1
        Module.defines? __MODULE__, {:version, 0}, :defp #=> false
      end

  """
  def defines?(module, tuple, kind) do
    assert_not_compiled!(:defines?, module)
    table = defs_table_for(module)
    case :ets.lookup(table, tuple) do
      [{_, ^kind, _, _, _, _, _}] -> true
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
  def definitions_in(module) do
    assert_not_compiled!(:definitions_in, module)
    table = defs_table_for(module)
    :lists.concat :ets.match(table, {:'$1', :_, :_, :_, :_, :_, :_})
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
  def definitions_in(module, kind) do
    assert_not_compiled!(:definitions_in, module)
    table = defs_table_for(module)
    :lists.concat :ets.match(table, {:'$1', kind, :_, :_, :_, :_, :_})
  end

  @doc """
  Makes the given functions in `module` overridable.
  An overridable function is lazily defined, allowing a
  developer to customize it. See `Kernel.defoverridable/1` for
  more information and documentation.
  """
  def make_overridable(module, tuples) do
    assert_not_compiled!(:make_overridable, module)

    :lists.foreach(fn tuple ->
      case :elixir_def.lookup_definition(module, tuple) do
        false ->
          {name, arity} = tuple
          raise "cannot make function #{name}/#{arity} overridable because it was not defined"
        clause ->
          :elixir_def.delete_definition(module, tuple)

          neighbours = if :elixir_compiler.get_opt(:internal) do
            []
          else
            Module.LocalsTracker.yank(module, tuple)
          end

          old    = :elixir_def_overridable.overridable(module)
          merged = :orddict.update(tuple, fn({count, _, _, _}) ->
            {count + 1, clause, neighbours, false}
          end, {1, clause, neighbours, false}, old)
          :elixir_def_overridable.overridable(module, merged)
      end
    end, tuples)
  end

  @doc """
  Returns `true` if `tuple` in `module` is marked as overridable.
  """
  def overridable?(module, tuple) do
    !!List.keyfind(:elixir_def_overridable.overridable(module), tuple, 0)
  end

  @doc """
  Puts an Erlang attribute to the given module with the given
  key and value. The semantics of putting the attribute depends
  if the attribute was registered or not via `register_attribute/3`.

  ## Examples

      defmodule MyModule do
        Module.put_attribute __MODULE__, :custom_threshold_for_lib, 10
      end

  """
  def put_attribute(module, key, value) when is_atom(key) do
    assert_not_compiled!(:put_attribute, module)
    table = data_table_for(module)
    value = normalize_attribute(key, value)
    acc   = :ets.lookup_element(table, {:elixir, :acc_attributes}, 2)

    new =
      if :lists.member(key, acc) do
        case :ets.lookup(table, key) do
          [{^key, old}] -> [value|old]
          [] -> [value]
        end
      else
        value
      end

    :ets.insert(table, {key, new})
  end

  @doc """
  Gets the given attribute from a module. If the attribute
  was marked with `accumulate` with `Module.register_attribute/3`,
  a list is always returned.

  The `@` macro compiles to a call to this function. For example,
  the following code:

      @foo

  Expands close to:

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
  @spec get_attribute(atom, atom) :: term
  def get_attribute(module, key) do
    get_attribute(module, key, nil)
  end

  @doc """
  Deletes all attributes that match the given key.

  ## Examples

      defmodule MyModule do
        Module.put_attribute __MODULE__, :custom_threshold_for_lib, 10
        Module.delete_attribute __MODULE__, :custom_threshold_for_lib
      end

  """
  @spec delete_attribute(atom, atom) :: :ok
  def delete_attribute(module, key) when is_atom(key) do
    assert_not_compiled!(:delete_attribute, module)
    table = data_table_for(module)
    :ets.delete(table, key)
    :ok
  end

  @doc """
  Registers an attribute. By registering an attribute, a developer
  is able to customize how Elixir will store and accumulate the
  attribute values.

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
  def register_attribute(module, new, opts) when is_atom(new) do
    assert_not_compiled!(:register_attribute, module)
    table = data_table_for(module)

    if Keyword.get(opts, :persist) do
      old = :ets.lookup_element(table, {:elixir, :persisted_attributes}, 2)
      :ets.insert(table, {{:elixir, :persisted_attributes}, [new|old]})
    end

    if Keyword.get(opts, :accumulate) do
      old = :ets.lookup_element(table, {:elixir, :acc_attributes}, 2)
      :ets.insert(table, {{:elixir, :acc_attributes}, [new|old]})
    end
  end

  @doc """
  Splits the given module name into binary parts.

  ## Examples

      Module.split Very.Long.Module.Name.And.Even.Longer
      #=> ["Very", "Long", "Module", "Name", "And", "Even", "Longer"]

  """
  def split(module) when is_atom(module) do
    split(String.Chars.to_string(module))
  end

  def split("Elixir." <> name) do
    String.split(name, ".")
  end

  @doc false
  # Used internally to compile documentation. This function
  # is private and must be used only internally.
  def compile_doc(env, kind, name, args, _guards, _body) do
    module = env.module
    line   = env.line
    arity  = length(args)
    pair   = {name, arity}
    doc    = get_attribute(module, :doc)

    # Arguments are not expanded for the docs, but we make an exception for
    # module attributes and for structs (aliases to be precise).
    args = Macro.prewalk args, fn
      {:@,  _, _} = attr ->
        Macro.expand_once(attr, env)
      {:%, meta, [aliases, fields]} ->
        {:%, meta, [Macro.expand_once(aliases, env), fields]}
      x ->
        x
    end

    case add_doc(module, line, kind, pair, args, doc) do
      :ok ->
        :ok
      {:error, :private_doc} ->
        :elixir_errors.warn line, env.file,
          "function #{name}/#{arity} is private, " <>
          "@doc's are always discarded for private functions"
    end

    delete_attribute(module, :doc)
  end

  @doc false
  # Used internally to compile types. This function
  # is private and must be used only internally.
  def store_typespec(module, key, value) when is_atom(key) do
    assert_not_compiled!(:put_attribute, module)
    table = data_table_for(module)

    new =
      case :ets.lookup(table, key) do
        [{^key, old}] -> [value|old]
        [] -> [value]
      end

    :ets.insert(table, {key, new})
  end

  @doc false
  def get_attribute(module, key, warn) when is_atom(key) and (is_list(warn) or is_nil(warn)) do
    assert_not_compiled!(:get_attribute, module)
    table = data_table_for(module)

    case :ets.lookup(table, key) do
      [{^key, val}] -> val
      [] ->
        acc = :ets.lookup_element(table, {:elixir, :acc_attributes}, 2)

        cond do
          :lists.member(key, acc) ->
            []
          is_list(warn) ->
            :elixir_errors.warn warn_info(warn), "undefined module attribute @#{key}, " <>
              "please remove access to @#{key} or explicitly set it before access"
            nil
          true ->
            nil
        end
    end
  end

  defp warn_info([entry|_]) do
    opts = elem(entry, tuple_size(entry) - 1)
    Exception.format_file_line(Keyword.get(opts, :file), Keyword.get(opts, :line)) <> " "
  end

  defp warn_info([]) do
    ""
  end

  ## Helpers

  defp normalize_attribute(:on_load, atom) when is_atom(atom) do
    {atom, 0}
  end

  defp normalize_attribute(:behaviour, atom) when is_atom(atom) do
    # Attempt to compile behaviour but ignore failure (will warn later)
    _ = Code.ensure_compiled(atom)
    atom
  end

  defp normalize_attribute(:file, file) when is_binary(file) do
    file
  end

  defp normalize_attribute(:before_compile, atom) when is_atom(atom),
    do: {atom, :__before_compile__}
  defp normalize_attribute(:after_compile, atom) when is_atom(atom),
    do: {atom, :__after_compile__}
  defp normalize_attribute(:on_definition, atom) when is_atom(atom),
    do: {atom, :__on_definition__}

  defp normalize_attribute(key, _value) when key in [:type, :typep, :export_type, :opaque, :callback, :macrocallback] do
    raise ArgumentError, "attributes type, typep, export_type, opaque, callback and macrocallback " <>
      "must be set via Kernel.Typespec"
  end

  defp normalize_attribute(_key, value) do
    value
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
        "could not call #{fun} on module #{inspect module} because it was already compiled"
  end
end
