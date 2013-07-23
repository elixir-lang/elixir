defmodule IEx.Introspection do
  # Convenience helpers for showing docs, specs and types
  # from modules. Invoked directly from IEX.Helpers.
  @moduledoc false

  import IEx, only: [dont_display_result: 0]

  @doc false
  def h(module) when is_atom(module) do
    case Code.ensure_loaded(module) do
      { :module, _ } ->
        case module.__info__(:moduledoc) do
          { _, binary } when is_binary(binary) ->
            IO.write IEx.color(:info, "# #{inspect module}\n\n" <> binary)
          { _, _ } ->
            nodocs(inspect module)
          _ ->
            IO.puts IEx.color(:error, "#{inspect module} was not compiled with docs")
        end
      { :error, reason } ->
        IO.puts IEx.color(:error, "Could not load module #{inspect module}: #{reason}")
    end
    dont_display_result
  end

  def h(_) do
    IO.puts IEx.color(:error, "Invalid arguments for h helper")
    dont_display_result
  end

  @doc false
  def h(modules, function) when is_list(modules) and is_atom(function) do
    result =
      Enum.reduce modules, :not_found, fn
        module, :not_found -> h_mod_fun(module, function)
        _module, acc -> acc
      end

    unless result == :ok, do:
      nodocs(function)

    dont_display_result
  end

  def h(module, function) when is_atom(module) and is_atom(function) do
    case h_mod_fun(module, function) do
      :ok ->
        :ok
      :no_docs ->
        IO.puts IEx.color(:error, "#{inspect module} was not compiled with docs")
      :not_found ->
        nodocs("#{inspect module}.#{function}")
    end

    dont_display_result
  end

  def h(function, arity) when is_atom(function) and is_integer(arity) do
    h([IEx.Helpers, Kernel, Kernel.SpecialForms], function, arity)
    dont_display_result
  end

  def h(_, _) do
    IO.puts IEx.color(:error, "Invalid arguments for h helper")
    dont_display_result
  end

  defp h_mod_fun(mod, fun) when is_atom(mod) and is_atom(fun) do
    if docs = mod.__info__(:docs) do
      result = lc { {f, arity}, _line, _type, _args, doc } inlist docs, fun == f, doc != false do
        h(mod, fun, arity)
        IO.puts ""
      end

      if result != [], do: :ok, else: :not_found
    else
      :no_docs
    end
  end

  @doc false
  def h(modules, function, arity) when is_list(modules) and is_atom(function) and is_integer(arity) do
    result =
      Enum.reduce modules, :not_found, fn
        module, :not_found -> h_mod_fun_arity(module, function, arity)
        _module, acc -> acc
      end

    unless result == :ok, do:
      nodocs("#{function}/#{arity}")

    dont_display_result
  end

  def h(module, function, arity) when is_atom(module) and is_atom(function) and is_integer(arity) do
    case h_mod_fun_arity(module, function, arity) do
      :ok ->
        :ok
      :no_docs ->
        IO.puts IEx.color(:error, "#{inspect module} was not compiled with docs")
      :not_found ->
        nodocs("#{inspect module}.#{function}/#{arity}")
    end

    dont_display_result
  end

  def h(_, _, _) do
    IO.puts IEx.color(:error, "Invalid arguments for h helper")
    dont_display_result
  end

  defp h_mod_fun_arity(mod, fun, arity) when is_atom(mod) and is_atom(fun) and is_integer(arity) do
    if docs = mod.__info__(:docs) do
      doc =
        cond do
          d = find_doc(docs, fun, arity)         -> d
          d = find_default_doc(docs, fun, arity) -> d
          true                                   -> nil
        end

      if doc do
        print_doc(doc)
        :ok
      else
        :not_found
      end
    else
      :no_docs
    end
  end

  defp find_doc(docs, function, arity) do
    if doc = List.keyfind(docs, { function, arity }, 0) do
      case elem(doc, 4) do
        false -> nil
        _ -> doc
      end
    end
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

  defp print_doc({ { fun, _ }, _line, kind, args, doc }) do
    args = Enum.map_join(args, ", ", print_doc_arg(&1))
    IO.puts IEx.color(:info, "* #{kind} #{fun}(#{args})\n")
    if doc, do: IO.write IEx.color(:info, doc)
  end

  defp print_doc_arg({ ://, _, [left, right] }) do
    print_doc_arg(left) <> " // " <> Macro.to_string(right)
  end

  defp print_doc_arg({ var, _, _ }) do
    atom_to_binary(var)
  end

  @doc false
  def t(module) do
    types = lc type inlist Kernel.Typespec.beam_types(module), do: print_type(type)

    if types == [] do
      notypes(inspect module)
    end

    dont_display_result
  end

  @doc false
  def t(module, type) when is_atom(type) do
    types = lc {_, {t, _, _args}} = typespec inlist Kernel.Typespec.beam_types(module),
               t == type do
      print_type(typespec)
      typespec
    end

    if types == [] do
       notypes("#{inspect module}.#{type}")
    end

    dont_display_result
  end

  @doc false
  def t(module, type, arity) do
    types = lc {_, {t, _, args}} = typespec inlist Kernel.Typespec.beam_types(module),
               length(args) == arity and t == type, do: typespec

    case types do
     [] ->
       notypes("#{inspect module}.#{type}/#{arity}")
     [type] ->
       print_type(type)
    end

    dont_display_result
  end

  @doc false
  def s(module) do
    specs = lc spec inlist beam_specs(module), do: print_spec(spec)

    if specs == [] do
      nospecs(inspect module)
    end

    dont_display_result
  end

  @doc false
  def s(module, function) when is_atom(function) do
    specs = lc {_kind, {{f, _arity}, _spec}} = spec inlist beam_specs(module),
               f == function do
      print_spec(spec)
      spec
    end

    if specs == [] do
      nospecs("#{inspect module}.#{function}")
    end

    dont_display_result
  end

  @doc false
  def s(module, function, arity) do
    specs = lc {_kind, {{f, a}, _spec}} = spec inlist beam_specs(module),
               f == function and a == arity do
      print_spec(spec)
      spec
    end

    if specs == [] do
      nodocs("#{inspect module}.#{function}")
    end

    dont_display_result
  end

  defp beam_specs(module) do
    specs = Enum.map(Kernel.Typespec.beam_specs(module), {:spec, &1})
    callbacks = Enum.map(Kernel.Typespec.beam_callbacks(module), {:callback, &1})
    List.concat(specs, callbacks)
  end

  defp print_type({ kind, type }) do
    ast = Kernel.Typespec.type_to_ast(type)
    IO.puts IEx.color(:info, "@#{kind} #{Macro.to_string(ast)}")
    true
  end

  defp print_spec({kind, { { name, _arity }, specs }}) do
    Enum.each specs, fn(spec) ->
      binary = Macro.to_string Kernel.Typespec.spec_to_ast(name, spec)
      IO.puts IEx.color(:info, "@#{kind} #{binary}")
    end
    true
  end

  defp nospecs(for), do: nodocs(for, "specification")
  defp notypes(for), do: nodocs(for, "type information")
  defp nodocs(for, type // "documentation") do
    IO.puts IEx.color(:error, "No #{type} for #{for} was found")
  end
end
