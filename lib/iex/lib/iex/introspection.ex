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
              no_docs(inspect module)
            _ ->
              puts_error("#{inspect module} was not compiled with docs")
          end
        else
          puts_error("#{inspect module} is an Erlang module and, as such, it does not have Elixir-style docs")
        end
      {:error, reason} ->
        puts_error("Could not load module #{inspect module}, got: #{reason}")
    end
    dont_display_result()
  end

  def h(_) do
    puts_error("Invalid arguments for h helper")
    dont_display_result()
  end

  @doc """
  Prints the documentation for the given function
  with any arity in the list of modules.
  """
  def h(modules, function) when is_list(modules) and is_atom(function) do
    printed? =
      Enum.any?(modules, fn module ->
        h_mod_fun(module, function) == :ok
      end)

    unless printed?, do: no_docs(function)

    dont_display_result()
  end

  def h(module, function) when is_atom(module) and is_atom(function) do
    case h_mod_fun(module, function) do
      :ok ->
        :ok
      :no_docs ->
        puts_error("#{inspect module} was not compiled with docs")
      :not_found ->
        no_docs("#{inspect module}.#{function}")
    end

    dont_display_result()
  end

  defp h_mod_fun(mod, fun) when is_atom(mod) do
    if docs = Code.get_docs(mod, :docs) do
      result = for {{^fun, arity}, _, _, _, _} = doc <- docs, has_content?(doc) do
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
    printed? =
      Enum.any?(modules, fn module ->
        h_mod_fun_arity(module, function, arity) == :ok
      end)

    unless printed?, do: no_docs("#{function}/#{arity}")

    dont_display_result()
  end

  def h(module, function, arity) when is_atom(module) and is_atom(function) and is_integer(arity) do
    case h_mod_fun_arity(module, function, arity) do
      :ok ->
        :ok
      :no_docs ->
        puts_error("#{inspect module} was not compiled with docs")
      :not_found ->
        no_docs("#{inspect module}.#{function}/#{arity}")
    end

    dont_display_result()
  end

  defp h_mod_fun_arity(mod, fun, arity) when is_atom(mod) do
    if docs = Code.get_docs(mod, :docs) do
      if doc = find_doc(docs, fun, arity) do
        if callback_module = is_nil(elem(doc, 4)) and callback_module(mod, fun, arity) do
          filter = &match?({^fun, ^arity}, elem(&1, 0))
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

  defp find_doc(docs, fun, arity) do
    doc = List.keyfind(docs, {fun, arity}, 0) || find_doc_defaults(docs, fun, arity)
    if doc != nil and has_content?(doc), do: doc
  end

  defp find_doc_defaults(docs, function, min) do
    Enum.find(docs, fn doc ->
      case elem(doc, 0) do
        {^function, arity} when arity > min ->
          defaults = Enum.count(elem(doc, 3), &match?({:\\, _, _}, &1))
          arity <= (min + defaults)
        _ ->
          false
      end
    end)
  end

  defp has_content?({_, _, _, _, false}),
    do: false
  defp has_content?({{name, _}, _, _, _, nil}),
    do: hd(Atom.to_charlist(name)) != ?_
  defp has_content?({_, _, _, _, _}),
    do: true

  defp callback_module(mod, fun, arity) do
    predicate = &match?({{^fun, ^arity}, _}, &1)
    mod.module_info(:attributes)
    |> Keyword.get_values(:behaviour)
    |> Stream.concat()
    |> Enum.find(&Enum.any?(Typespec.beam_callbacks(&1), predicate))
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
    printer = fn heading, _doc -> puts_info(heading) end
    case print_callback_docs(mod, fn _ -> true end, printer) do
      :ok        -> :ok
      :no_beam   -> no_beam(mod)
      :no_docs   -> puts_error("#{inspect mod} was not compiled with docs")
      :not_found -> puts_error("No callbacks for #{inspect mod} were found")
    end

    dont_display_result()
  end

  @doc """
  Prints documentation for the given callback function with any arity.
  """
  def b(mod, fun) when is_atom(mod) and is_atom(fun) do
    filter = &match?({^fun, _}, elem(&1, 0))
    case print_callback_docs(mod, filter, &print_doc/2) do
      :ok        -> :ok
      :no_beam   -> no_beam(mod)
      :no_docs   -> puts_error("#{inspect mod} was not compiled with docs")
      :not_found -> no_docs("#{inspect mod}.#{fun}")
    end

    dont_display_result()
  end

  @doc """
  Prints documentation for the given callback function and arity.
  """
  def b(mod, fun, arity) when is_atom(mod) and is_atom(fun) and is_integer(arity) do
    filter = &match?({^fun, ^arity}, elem(&1, 0))
    case print_callback_docs(mod, filter, &print_doc/2) do
      :ok        -> :ok
      :no_beam   -> no_beam(mod)
      :no_docs   -> puts_error("#{inspect mod} was not compiled with docs")
      :not_found -> no_docs("#{inspect mod}.#{fun}/#{arity}")
    end

    dont_display_result()
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

  defp drop_macro_env({name, meta, [{:::, _, [_, {{:., _, [Macro.Env, :t]}, _, _}]} | args]}),
    do: {name, meta, args}

  defp drop_macro_env(other),
    do: other

  @doc """
  Prints the types for the given module.
  """
  def t(module) when is_atom(module) do
    _ = case Typespec.beam_types(module) do
      nil   -> no_beam(module)
      []    -> no_types(inspect module)
      types -> Enum.each(types, &print_type/1)
    end

    dont_display_result()
  end

  @doc """
  Prints the given type in module with any arity.
  """
  def t(module, type) when is_atom(module) and is_atom(type) do
    case Typespec.beam_types(module) do
      nil   -> no_beam(module)
      types ->
        printed =
          for {_, {^type, _, _args}} = typespec <- types do
            print_type_doc(module, type)
            print_type(typespec)
            typespec
          end

        if printed == [] do
          no_types("#{inspect module}.#{type}")
        end
    end

    dont_display_result()
  end

  @doc """
  Prints the type in module with given arity.
  """
  def t(module, type, arity) when is_atom(module) and is_atom(type) and is_integer(arity) do
    case Typespec.beam_types(module) do
      nil   -> no_beam(module)
      types ->
        printed =
          for {_, {^type, _, args}} = typespec <- types, length(args) == arity do
            print_type_doc(module, type)
            print_type(typespec)
            typespec
          end

        if printed == [] do
          no_types("#{inspect module}.#{type}")
        end
    end

    dont_display_result()
  end

  defp print_type_doc(module, type) do
    docs  = Code.get_docs(module, :type_docs)
    {_, _, _, content} = Enum.find(docs, fn({{name, _}, _, _, _}) ->
      type == name
    end)
    if content, do: puts_info(content)
  end

  @doc """
  Prints the specs for given module.
  """
  def s(module) when is_atom(module) do
    case beam_specs(module) do
      nil   -> no_beam(module)
      []    -> no_specs(inspect module)
      specs ->
        printed = for {_kind, {{fun, _arity}, _spec}} = spec <- specs, fun != :__info__ do
          print_spec(spec)
        end
        if printed == [] do
          no_specs(inspect module)
        end
    end

    dont_display_result()
  end

  @doc """
  Prints the specs for given module and function.
  """
  def s(module, function) when is_atom(module) and is_atom(function) do
    case beam_specs(module) do
      nil   -> no_beam(module)
      specs ->
        printed =
          for {_kind, {{^function, _arity}, _spec}} = spec <- specs do
            print_spec(spec)
            spec
          end

        if printed == [] do
          no_specs("#{inspect module}.#{function}")
        end
    end

    dont_display_result()
  end

  @doc """
  Prints the spec in given module, with arity.
  """
  def s(module, function, arity) when is_atom(module) and is_atom(function) and is_integer(arity) do
    case beam_specs(module) do
      nil   -> no_beam(module)
      specs ->
        printed =
          for {_kind, {{^function, ^arity}, _spec}} = spec <- specs do
            print_spec(spec)
            spec
          end

        if printed == [] do
          no_specs("#{inspect module}.#{function}")
        end
    end

    dont_display_result()
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

  defp no_beam(module) do
    case Code.ensure_loaded(module) do
      {:module, _} ->
        puts_error("Beam code not available for #{inspect module} or debug info is missing, cannot load typespecs")
      {:error, reason} ->
        puts_error("Could not load module #{inspect module}, got: #{reason}")
    end
  end

  defp no_specs(for), do: no(for, "specification")
  defp no_types(for), do: no(for, "type information")
  defp no_docs(for),  do: no(for, "documentation")

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
