# Convenience helpers for showing docs, specs and types
# from modules. Invoked directly from IEx.Helpers.
defmodule IEx.Introspection do
  @moduledoc false

  import IEx, only: [dont_display_result: 0]

  alias Kernel.Typespec

  @doc """
  Prints the documentation for the given module.
  """
  def h(module) when is_atom(module) do
    case Code.ensure_loaded(module) do
      {:module, _} ->
        if function_exported?(module, :__info__, 1) do
          case Code.get_docs(module, :moduledoc) do
            {_, binary} when is_binary(binary) ->
              print_doc(inspect(module), binary)
            {_, _} ->
              nodocs(inspect module)
            _ ->
              puts_error("#{inspect module} was not compiled with docs")
          end
        else
          case System.cmd("erl", ["-man", module], stderr_to_stdout: true) do
            {binary, 0} ->
              print_doc(inspect(module), binary)
            _ ->
              puts_error("Could not find erlang man pages. Please install erlang man pages")
          end
        end
      {:error, reason} ->
        puts_error("Could not load module #{inspect module}, got: #{reason}")
    end
    dont_display_result
  end

  def h(_) do
    puts_error("Invalid arguments for h helper")
    dont_display_result
  end

  @doc """
  Prints the documentation for the given function
  with any arity in the list of modules.
  """
  def h(modules, function) when is_list(modules) and is_atom(function) do
    result =
      Enum.find_value modules, false, fn module ->
        h_mod_fun(module, function) == :ok
      end

    unless result, do: nodocs(function)

    dont_display_result
  end

  def h(module, function) when is_atom(module) and is_atom(function) do
    case h_mod_fun(module, function) do
      :ok ->
        :ok
      :no_docs ->
        puts_error("#{inspect module} was not compiled with docs")
      :not_found ->
        nodocs("#{inspect module}.#{function}")
    end

    dont_display_result
  end

  defp h_mod_fun(mod, fun) when is_atom(mod) do
    if docs = Code.get_docs(mod, :docs) do
      result = for {{f, arity}, _line, _type, _args, doc} <- docs, fun == f, doc != false do
        h(mod, fun, arity)
      end

      if result != [], do: :ok, else: :not_found
    else
      :no_docs
    end
  end

  @doc """
  Prints the documentation for the given function
  and arity in the list of modules.
  """
  def h(modules, function, arity) when is_list(modules) and is_atom(function) and is_integer(arity) do
    result =
      Enum.find_value modules, false, fn module ->
        h_mod_fun_arity(module, function, arity) == :ok
      end

    unless result, do: nodocs("#{function}/#{arity}")

    dont_display_result
  end

  def h(module, function, arity) when is_atom(module) and is_atom(function) and is_integer(arity) do
    case h_mod_fun_arity(module, function, arity) do
      :ok ->
        :ok
      :no_docs ->
        puts_error("#{inspect module} was not compiled with docs")
      :not_found ->
        nodocs("#{inspect module}.#{function}/#{arity}")
    end

    dont_display_result
  end

  defp h_mod_fun_arity(mod, fun, arity) when is_atom(mod) do
    if docs = Code.get_docs(mod, :docs) do
      doc = find_doc(docs, fun, arity, 4)
            || find_default_doc(docs, fun, arity)

      if doc do
        if match?({_, _, _, _, nil}, doc) && (callback_module = callback_module(mod, fun, arity)) do
          filter = &match?({^fun, _}, elem(&1, 0))
          print_callback_docs(callback_module, filter, &print_doc/2)
        else
          print_doc(doc)
        end
        :ok
      else
        :not_found
      end
    else
      :no_docs
    end
  end

  defp find_doc(docs, function, arity, pos) do
    if doc = List.keyfind(docs, {function, arity}, 0) do
      case elem(doc, pos) do
        false -> nil
        _ -> doc
      end
    end
  end

  defp find_default_doc(docs, function, min) do
    Enum.find docs, fn(doc) ->
      case elem(doc, 0) do
        {^function, max} when max > min ->
          defaults = Enum.count elem(doc, 3), &match?({:\\, _, _}, &1)
          min + defaults >= max
        _ ->
          false
      end
    end
  end

  defp callback_module(mod, fun, arity) do
    mod.module_info(:attributes)
    |> Keyword.get_values(:behaviour)
    |> Stream.concat()
    |> Stream.filter(fn(module)->
      module.module_info(:attributes)
      |> Enum.filter_map(&match?({:callback, _}, &1), fn {_, [{t,_}|_]} -> t end)
      |> Enum.any?(&match?({^fun, ^arity}, &1))
    end)
    |> Enum.at(0)
  end

  defp print_doc({{fun, _}, _line, kind, args, doc}) do
    args = Enum.map_join(args, ", ", &format_doc_arg(&1))

    print_doc("#{kind} #{fun}(#{args})", doc)
  end

  defp print_doc(heading, doc) do
    doc = doc || ""
    if opts = IEx.Config.ansi_docs do
      IO.ANSI.Docs.print_heading(heading, opts)
      IO.ANSI.Docs.print(doc, opts)
    else
      IO.puts "* #{heading}\n"
      IO.puts doc
    end
  end

  defp format_doc_arg({:\\, _, [left, right]}) do
    format_doc_arg(left) <> " \\\\ " <> Macro.to_string(right)
  end

  defp format_doc_arg({var, _, _}) do
    Atom.to_string(var)
  end

  @doc """
  Prints the list of behaviour callbacks for the given module.
  """
  def b(mod) when is_atom(mod) do
    printer = &puts_callback_info/2
    case print_callback_docs(mod, &match?(_, &1), printer) do
      :ok        -> :ok
      :no_beam   -> nobeam(mod)
      :no_docs   -> puts_error("#{inspect mod} was not compiled with docs")
      :not_found -> puts_error("No callbacks for #{inspect mod} were found")
    end

    dont_display_result
  end

  defp puts_callback_info(heading, _doc),
    do: puts_info(heading)

  @doc """
  Prints documentation for the given callback function with any arity.
  """
  def b(mod, fun) when is_atom(mod) and is_atom(fun) do
    filter = &match?({^fun, _}, elem(&1, 0))
    case print_callback_docs(mod, filter, &print_doc/2) do
      :ok        -> :ok
      :no_beam   -> nobeam(mod)
      :no_docs   -> puts_error("#{inspect mod} was not compiled with docs")
      :not_found -> nodocs("#{inspect mod}.#{fun}")
    end

    dont_display_result
  end

  @doc """
  Prints documentation for the given callback function and arity.
  """
  def b(mod, fun, arity) when is_atom(mod) and is_atom(fun) and is_integer(arity) do
    filter = &match?({^fun, ^arity}, elem(&1, 0))
    case print_callback_docs(mod, filter, &print_doc/2) do
      :ok        -> :ok
      :no_beam   -> nobeam(mod)
      :no_docs   -> puts_error("#{inspect mod} was not compiled with docs")
      :not_found -> nodocs("#{inspect mod}.#{fun}/#{arity}")
    end

    dont_display_result
  end

  defp print_callback_docs(mod, filter, printer) do
    case get_callback_docs(mod) do
      {callbacks, docs} ->
        printed =
          Enum.filter_map docs, filter, fn
            {{fun, arity}, _, :macrocallback, doc} ->
              print_callback_doc(fun, :macrocallback, doc, {:"MACRO-#{fun}", arity + 1}, callbacks, printer)
            {{fun, arity}, _, kind, doc} ->
              print_callback_doc(fun, kind, doc, {fun, arity}, callbacks, printer)
          end
        if Enum.any?(printed), do: :ok, else: :not_found

      other -> other
    end
  end

  defp get_callback_docs(mod) do
    callbacks = Typespec.beam_callbacks(mod)
    docs = Code.get_docs(mod, :callback_docs)

    cond do
      is_nil(callbacks) -> :no_beam
      is_nil(docs) -> :no_docs
      true -> {callbacks, docs}
    end
  end

  defp print_callback_doc(name, kind, doc, key, callbacks, printer) do
    {_, [spec | _]} = List.keyfind(callbacks, key, 0)

    definition =
      Typespec.spec_to_ast(name, spec)
      |> Macro.prewalk(&drop_macro_env/1)
      |> Macro.to_string

    printer.("@#{kind} #{definition}", doc)
  end

  defp drop_macro_env({name, meta, [{:::, _, [{:env, _, _}, _ | _]} | args]}),
    do: {name, meta, args}

  defp drop_macro_env(other),
    do: other

  @doc """
  Prints the types for the given module.
  """
  def t(module) when is_atom(module) do
    _ = case Typespec.beam_types(module) do
      nil   -> nobeam(module)
      []    -> notypes(inspect module)
      types -> Enum.each(types, &print_type/1)
    end

    dont_display_result
  end

  @doc """
  Prints the given type in module with any arity.
  """
  def t(module, type) when is_atom(module) and is_atom(type) do
    case Typespec.beam_types(module) do
      nil   -> nobeam(module)
      types ->
        printed =
          for {_, {t, _, _args}} = typespec <- types, t == type do
            print_type_doc(module, t)
            print_type(typespec)
            typespec
          end

        if printed == [] do
          notypes("#{inspect module}.#{type}")
        end
    end

    dont_display_result
  end

  @doc """
  Prints the type in module with given arity.
  """
  def t(module, type, arity) when is_atom(module) and is_atom(type) and is_integer(arity) do
    case Typespec.beam_types(module) do
      nil   -> nobeam(module)
      types ->
        printed =
          for {_, {t, _, args}} = typespec <- types, t == type, length(args) == arity do
            print_type_doc(module, t)
            print_type(typespec)
            typespec
          end

        if printed == [] do
          notypes("#{inspect module}.#{type}")
        end
    end

    dont_display_result
  end

  defp print_type_doc(module, type) do
    docs  = Code.get_docs(module, :type_docs)
    {{_, _}, _, _, description} = Enum.find(docs, fn({{name, _}, _, _, _}) ->
      type == name
    end)
    if description, do: puts_info(description)
  end

  @doc """
  Prints the specs for given module.
  """
  def s(module) when is_atom(module) do
    case beam_specs(module) do
      nil   -> nobeam(module)
      []    -> nospecs(inspect module)
      specs ->
        printed = for {_kind, {{f, _arity}, _spec}} = spec <- specs, f != :"__info__" do
          print_spec(spec)
        end
        if printed == [] do
          nospecs(inspect module)
        end
    end

    dont_display_result
  end

  @doc """
  Prints the specs for given module and function.
  """
  def s(module, function) when is_atom(module) and is_atom(function) do
    case beam_specs(module) do
      nil   -> nobeam(module)
      specs ->
        printed =
          for {_kind, {{f, _arity}, _spec}} = spec <- specs, f == function do
            print_spec(spec)
            spec
          end

        if printed == [] do
          nospecs("#{inspect module}.#{function}")
        end
    end

    dont_display_result
  end

  @doc """
  Prints the spec in given module, with arity.
  """
  def s(module, function, arity) when is_atom(module) and is_atom(function) and is_integer(arity) do
    case beam_specs(module) do
      nil   -> nobeam(module)
      specs ->
        printed =
          for {_kind, {{f, a}, _spec}} = spec <- specs, f == function and a == arity do
            print_spec(spec)
            spec
          end

        if printed == [] do
          nospecs("#{inspect module}.#{function}")
        end
    end

    dont_display_result
  end

  defp beam_specs(module) do
    specs = beam_specs_tag(Typespec.beam_specs(module), :spec)
    callbacks = beam_specs_tag(Typespec.beam_callbacks(module), :callback)
    specs && callbacks && Enum.concat(specs, callbacks)
  end

  defp beam_specs_tag(nil, _), do: nil
  defp beam_specs_tag(specs, tag) do
    Enum.map(specs, &{tag, &1})
  end

  defp print_type({:opaque, type}) do
    {:::, _, [ast, _]} = Typespec.type_to_ast(type)
    puts_info("@opaque #{Macro.to_string(ast)}")
    true
  end

  defp print_type({kind, type}) do
    ast = Typespec.type_to_ast(type)
    puts_info("@#{kind} #{Macro.to_string(ast)}")
    true
  end

  defp print_spec({kind, {{name, _arity}, specs}}) do
    Enum.each specs, fn(spec) ->
      binary = Macro.to_string Typespec.spec_to_ast(name, spec)
      puts_info("@#{kind} #{binary}")
    end
    true
  end

  defp nobeam(module) do
    case Code.ensure_loaded(module) do
      {:module, _} ->
        puts_error("Beam code not available for #{inspect module} or debug info is missing, cannot load typespecs")
      {:error, reason} ->
        puts_error("Could not load module #{inspect module}, got: #{reason}")
    end
  end

  defp nospecs(for), do: no(for, "specification")
  defp notypes(for), do: no(for, "type information")
  defp nodocs(for),  do: no(for, "documentation")

  defp no(for, type) do
    puts_error("No #{type} for #{for} was found")
  end

  defp puts_info(string) do
    IO.puts IEx.color(:eval_info, string)
  end

  defp puts_error(string) do
    IO.puts IEx.color(:eval_error, string)
  end
end
