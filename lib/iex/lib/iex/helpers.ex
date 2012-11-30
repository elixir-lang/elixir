defmodule IEx.Helpers do
  @moduledoc """
  Welcome to Interactive Elixir. You are currently
  seeing the documentation for the module IEx.Helpers
  which provides many helpers to make Elixir's shell
  more joyful to work with.

  This message was triggered by invoking the helper
  `h()`, usually referred as `h/0` (since it expects 0
  arguments).

  There are many other helpers available:

  * `c/2` - compiles a file in the given path
  * `h/0`, `h/1` - prints help/documentation
  * `t/1` — prints type information
  * `s/1` — prints spec information
  * `m/0` - prints loaded modules
  * `r/0`, `r/1` - recompiles and reloads the given module's source file
  * `l/1` - loads given module beam code by purging the current version
  * `v/0` - prints all commands and values
  * `v/1` - retrieves nth value from console
  * `flush/0` — flush all messages sent to the shell

  Help for functions in this module can be consulted
  directly from the command line, as an example, try:

      h(c/2)

  You can also retrieve the documentation for any module
  or function. Try these:

      h(Enum)
      h(Enum.reverse/1)

  """

  @doc """
  Expects a list of files to compile and a path
  to write their object code to. It returns the name
  of the compiled modules.

  ## Examples

      c ["foo.ex"], "ebin"
      #=> Foo

  """
  def c(files, path // ".") do
    tuples = Kernel.ParallelCompiler.files_to_path List.wrap(files), path
    Enum.map tuples, elem(&1, 0)
  end

  @doc """
  Returns the name and module of all modules loaded.
  """
  def m do
    all    = Enum.map :code.all_loaded, fn { mod, file } -> { inspect(mod), file } end
    sorted = Enum.sort all
    size   = Enum.reduce sorted, 0, fn({ mod, _ }, acc) -> max(byte_size(mod), acc) end
    format = "~-#{size}s ~s~n"

    Enum.each sorted, fn({ mod, file }) ->
      :io.format(format, [mod, file])
    end
  end

  @doc """
  Prints commands history and their result.
  """
  def v do
    history = Enum.reverse(Process.get(:iex_history))
    Enum.each(history, print_history(&1))
  end

  defp print_history(config) do
    IO.puts "#{config.counter}: #{config.cache}#=> #{inspect config.result}\n"
  end

  @doc """
  Shows the documentation for IEx.Helpers.
  """
  def h() do
    h(IEx.Helpers, [])
  end

  @doc """
  Shows the documentation for the given module
  or for the given function/arity pair.

  ## Examples

      h(Enum)
      #=> Prints documentation for Enum

  It also accepts functions in the format `fun/arity`
  and `module.fun/arity`, for example:

      h receive/1
      h Enum.all?/2
      h Enum.all?

  """
  defmacro h({ :/, _, [{ { :., _, [mod, fun] }, _, [] }, arity] }) do
    quote do
      h(unquote(mod), unquote(fun), unquote(arity))
    end
  end

  defmacro h({ { :., _, [mod, fun] }, _, [] }) do
    quote do
      h(unquote(mod), unquote(fun))
    end
  end

  defmacro h({ :/, _, [{ fun, _, args }, arity] }) when args == [] or is_atom(args) do
    quote do
      h(unquote(fun), unquote(arity))
    end
  end

  defmacro h({ name, _, args }) when args == [] or is_atom(args) do
    quote do
      h(unquote(__MODULE__), unquote(name))
      h(Kernel, unquote(name))
    end
  end

  defmacro h(other) do
    quote do
      h(unquote(other), [])
    end
  end

  @doc false
  def h(:h, 1) do
    h(__MODULE__, :h, 1)
  end

  def h(function, arity) when is_atom(function) and is_integer(arity) do
    if function_exported?(__MODULE__, function, arity) or
       macro_exported?(__MODULE__, function, arity) do
      h(__MODULE__, function, arity)
    else
      h(Kernel, function, arity)
    end
  end

  def h(module, []) when is_atom(module) do
    case Code.ensure_loaded(module) do
      { :module, _ } ->
        case module.__info__(:moduledoc) do
          { _, binary } when is_binary(binary) ->
            IO.puts "# #{inspect module}\n"
            IO.write binary
          { _, _ } ->
            IO.puts "No docs for #{inspect module} have been found"
          _ ->
            IO.puts "#{inspect module} was not compiled with docs"
        end
      { :error, reason } ->
        IO.puts "Could not load module #{inspect module}: #{reason}"
    end
  end

  def h(module, function) when is_atom(module) and is_atom(function) do
    lc {{f, arity}, _line, _type, _args, doc } inlist module.__info__(:docs),
       f == function and doc != false do
      h(module, function, arity)
    end
    :ok
  end

  def h(_, _) do
    IO.puts "Invalid h helper argument"
    h()
  end

  @doc false
  def h(module, function, arity) when is_atom(module) and is_atom(function) and is_integer(arity) do
    if docs = module.__info__(:docs) do
      doc =
        cond do
          d = find_doc(docs, function, arity)         -> d
          d = find_default_doc(docs, function, arity) -> d
          true                                        -> nil
        end

      if doc do
        IO.write "\n" <> print_signature(doc)
      else
        IO.puts "No docs for #{inspect module}#.{function}/#{arity} have been found"
      end
    else
      IO.puts "#{inspect module} was not compiled with docs"
    end
  end

  defp find_doc(docs, function, arity) do
    List.keyfind(docs, { function, arity }, 0)
  end

  defp find_default_doc(docs, function, min) do
    Enum.find docs, fn(doc) ->
      case elem(doc, 0) do
        { ^function, max } when max > min ->
          defaults = Enum.count elem(doc, 3), match?({ ://, _, _ }, &1)
          min + defaults >= max
        _ ->
          false
      end
    end
  end

  # Get the full signature from a function.
  defp print_signature({ _info, _line, _kind, _args, false }) do
    false
  end

  defp print_signature({ { name, _arity }, _line, kind, args, docs }) do
    args = Enum.map_join(args, ", ", signature_arg(&1))
    IO.puts "* #{kind} #{name}(#{args})"
    docs
  end

  defp signature_arg({ ://, _, [left, right] }) do
    signature_arg(left) <> " // " <> Macro.to_binary(right)
  end

  defp signature_arg({ var, _, _ }) do
    atom_to_binary(var)
  end

  @doc """
  Prints all types for the given module or prints out a specified type's 
  specification

  ## Examples

      t(Enum)
      t(Enum.t/0)
      t(Enum.t)

  """
  defmacro t({ :/, _, [{ { :., _, [mod, fun] }, _, [] }, arity] }) do
    quote do
      t(unquote(mod), unquote(fun), unquote(arity))
    end
  end

  defmacro t({ { :., _, [mod, fun] }, _, [] }) do
    quote do
      t(unquote(mod), unquote(fun))
    end
  end

  defmacro t(module) do
    quote do
      t(unquote(module), [])
    end
  end

  @doc false
  def t(module, type) when is_atom(type) do
    types = lc {_, {t, _, _args}} = typespec inlist Kernel.Typespec.beam_types(module), 
               t == type do
      print_type(typespec)
      typespec
    end

    if types == [] do
       IO.puts "No types for #{inspect module}.#{type} have been found"
    end

    :ok
  end

  def t(module, []) do
    types = lc type inlist Kernel.Typespec.beam_types(module), do: print_type(type)

    if types == [] do
      IO.puts "No types for #{inspect module} have been found"
    end

    :ok
  end

  @doc false
  def t(module, type, arity) do
    types = lc {_, {t, _, args}} = typespec inlist Kernel.Typespec.beam_types(module), 
               length(args) == arity and t == type, do: typespec

    case types do
     [] ->
       IO.puts "No types for #{inspect module}.#{type}/#{arity} have been found"
     [type] ->
       print_type(type)
    end

    :ok
  end

  @doc """
  Prints all specs from a given module.

  ## Examples

      s(Enum)
      s(Enum.all?)
      s(Enum.all?/2)
      s(list_to_atom)
      s(list_to_atom/1)

  """
  defmacro s({ :/, _, [{ { :., _, [mod, fun] }, _, [] }, arity] }) do
    quote do
      s(unquote(mod), unquote(fun), unquote(arity))
    end
  end

  defmacro s({ { :., _, [mod, fun] }, _, [] }) do
    quote do
      s(unquote(mod), unquote(fun))
    end
  end

  defmacro s({ fun, _, args }) when args == [] or is_atom(args) do
    quote do
      s(Kernel, unquote(fun))
    end
  end

  defmacro s({ :/, _, [{ fun, _, args }, arity] }) when args == [] or is_atom(args) do
    quote do
      s(Kernel, unquote(fun), unquote(arity))
    end    
  end

  defmacro s(module) do
    quote do
      s(unquote(module), [])
    end
  end

  @doc false
  def s(module, function) when is_atom(function) do
    specs = lc {{f, _arity}, _spec} = spec inlist Kernel.Typespec.beam_specs(module),
               f == function do
      print_spec(spec)
      spec
    end

    if specs == [] do
      IO.puts "No specs for #{inspect module}.#{function} have been found"
    end

    :ok
  end

  def s(module, []) do
    specs = lc spec inlist Kernel.Typespec.beam_specs(module), do: print_spec(spec)

    if specs == [] do
      IO.puts "No specs for #{inspect module} have been found"
    end

    :ok
  end

  @doc false
  def s(module, function, arity) do
    spec = List.keyfind(Kernel.Typespec.beam_specs(module), { function, arity }, 0)

    if spec do
      print_spec(spec)
    else
      IO.puts "No specs for #{inspect module}.#{function}/#{arity} have been found"
    end

    :ok
  end

  defp print_type({ kind, type }) do
    ast = Kernel.Typespec.type_to_ast(type)
    IO.puts "@#{kind} #{Macro.to_binary(ast)}"
    true
  end

  defp print_spec({ { name, _arity }, specs }) do
    Enum.each specs, fn(spec) ->
      binary = Macro.to_binary Kernel.Typespec.spec_to_ast(name, spec)
      IO.puts "@spec #{binary}"
    end
    true
  end

  @doc """
  Retrieves nth query's value from the history. Use negative
  values to lookup query's value from latest to earliest.
  For instance, v(-1) returns the latest result.
  """
  def v(n) when n < 0 do
    history = Process.get(:iex_history)
    Enum.at!(history, abs(n) - 1).result
  end

  def v(n) when n > 0 do
    history = Process.get(:iex_history) /> Enum.reverse
    Enum.at!(history, n - 1).result
  end

  @doc """
  Reloads all modules that were already reloaded
  at some point with `r/1`.
  """
  def r do
    Enum.map iex_reloaded, r(&1)
  end

  @doc """
  Recompiles and reloads the specified module's source file.

  Please note that all the modules defined in the specified
  files are recompiled and reloaded.
  """
  def r(module) do
    if source = source(module) do
      Process.put(:iex_reloaded, :ordsets.add_element(module, iex_reloaded))
      { module, Code.load_file source }
    else
      :nosource
    end
  end

  @doc """
  Purges and reloads specified module
  """
  def l(module) do
   :code.purge(module)
   :code.load_file(module)
  end

  @doc """
  Flushes all messages sent to the shell and prints them out
  """
  def flush do
    receive do
      msg ->
        IO.inspect(msg)
        flush
    after 0 ->
      :ok
    end  
  end

  defp iex_reloaded do
    Process.get(:iex_reloaded) || :ordsets.new
  end

  defp source(module) do
    compile = module.module_info(:compile)

    # Get the source of the compiled module. Due to a bug in Erlang
    # R15 and before, we need to look for the source first in the
    # options and then into the real source.
    options =
      case List.keyfind(compile, :options, 0) do
        { :options, opts } -> opts
        _ -> []
      end

    source = List.keyfind(options, :source, 0)  || List.keyfind(compile, :source, 0)

    case source do
      { :source, source } -> list_to_binary(source)
      _ -> nil
    end
  end
end
