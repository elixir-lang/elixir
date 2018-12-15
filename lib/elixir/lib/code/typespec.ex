defmodule Code.Typespec do
  @moduledoc false

  @doc """
  Converts a spec clause back to Elixir quoted expression.
  """
  @spec spec_to_quoted(atom, tuple) :: {atom, keyword, [Macro.t()]}
  def spec_to_quoted(name, spec)

  def spec_to_quoted(name, {:type, line, :fun, [{:type, _, :product, args}, result]})
      when is_atom(name) do
    meta = [line: line]
    body = {name, meta, Enum.map(args, &typespec_to_quoted/1)}

    vars =
      for type_expr <- args ++ [result],
          var <- collect_vars(type_expr),
          uniq: true,
          do: {var, {:var, meta, nil}}

    spec = {:::, meta, [body, typespec_to_quoted(result)]}

    if vars == [] do
      spec
    else
      {:when, meta, [spec, vars]}
    end
  end

  def spec_to_quoted(name, {:type, line, :fun, []}) when is_atom(name) do
    {:::, [line: line], [{name, [line: line], []}, quote(do: term)]}
  end

  def spec_to_quoted(name, {:type, line, :bounded_fun, [type, constrs]}) when is_atom(name) do
    {:type, _, :fun, [{:type, _, :product, args}, result]} = type

    guards =
      for {:type, _, :constraint, [{:atom, _, :is_subtype}, [{:var, _, var}, type]]} <- constrs do
        {erl_to_ex_var(var), typespec_to_quoted(type)}
      end

    meta = [line: line]
    ignore_vars = Keyword.keys(guards)

    vars =
      for type_expr <- args ++ [result],
          var <- collect_vars(type_expr),
          var not in ignore_vars,
          uniq: true,
          do: {var, {:var, meta, nil}}

    args = for arg <- args, do: typespec_to_quoted(arg)

    when_args = [
      {:::, meta, [{name, [line: line], args}, typespec_to_quoted(result)]},
      guards ++ vars
    ]

    {:when, meta, when_args}
  end

  @doc """
  Converts a type clause back to Elixir AST.
  """
  def type_to_quoted(type)

  def type_to_quoted({{:record, record}, fields, args}) when is_atom(record) do
    fields = for field <- fields, do: typespec_to_quoted(field)
    args = for arg <- args, do: typespec_to_quoted(arg)
    type = {:{}, [], [record | fields]}
    quote(do: unquote(record)(unquote_splicing(args)) :: unquote(type))
  end

  def type_to_quoted({name, type, args}) when is_atom(name) do
    args = for arg <- args, do: typespec_to_quoted(arg)
    quote(do: unquote(name)(unquote_splicing(args)) :: unquote(typespec_to_quoted(type)))
  end

  @doc """
  Returns all types available from the module's BEAM code.

  The result is returned as a list of tuples where the first
  element is the type (`:typep`, `:type` and `:opaque`).

  The module must have a corresponding BEAM file which can be
  located by the runtime system. The types will be in the Erlang
  Abstract Format.
  """
  @spec fetch_types(module | binary) :: {:ok, [tuple]} | :error
  def fetch_types(module) when is_atom(module) or is_binary(module) do
    case typespecs_abstract_code(module) do
      {:ok, abstract_code} ->
        exported_types = for {:attribute, _, :export_type, types} <- abstract_code, do: types
        exported_types = List.flatten(exported_types)

        types =
          for {:attribute, _, kind, {name, _, args} = type} <- abstract_code,
              kind in [:opaque, :type] do
            cond do
              kind == :opaque -> {:opaque, type}
              {name, length(args)} in exported_types -> {:type, type}
              true -> {:typep, type}
            end
          end

        {:ok, types}

      _ ->
        :error
    end
  end

  @doc """
  Returns all specs available from the module's BEAM code.

  The result is returned as a list of tuples where the first
  element is spec name and arity and the second is the spec.

  The module must have a corresponding BEAM file which can be
  located by the runtime system. The types will be in the Erlang
  Abstract Format.
  """
  @spec fetch_specs(module) :: {:ok, [tuple]} | :error
  def fetch_specs(module) when is_atom(module) or is_binary(module) do
    case typespecs_abstract_code(module) do
      {:ok, abstract_code} ->
        {:ok, for({:attribute, _, :spec, value} <- abstract_code, do: value)}

      :error ->
        :error
    end
  end

  @doc """
  Returns all callbacks available from the module's BEAM code.

  The result is returned as a list of tuples where the first
  element is spec name and arity and the second is the spec.

  The module must have a corresponding BEAM file
  which can be located by the runtime system. The types will be
  in the Erlang Abstract Format.
  """
  @spec fetch_callbacks(module) :: {:ok, [tuple]} | :error
  def fetch_callbacks(module) when is_atom(module) or is_binary(module) do
    case typespecs_abstract_code(module) do
      {:ok, abstract_code} ->
        {:ok, for({:attribute, _, :callback, value} <- abstract_code, do: value)}

      :error ->
        :error
    end
  end

  defp typespecs_abstract_code(module) do
    with {module, binary} <- get_module_and_beam(module),
         {:ok, {_, [debug_info: {:debug_info_v1, backend, data}]}} <-
           :beam_lib.chunks(binary, [:debug_info]) do
      case data do
        {:elixir_v1, %{}, specs} ->
          # Fast path to avoid translation to Erlang from Elixir.
          {:ok, specs}

        _ ->
          case backend.debug_info(:erlang_v1, module, data, []) do
            {:ok, abstract_code} -> {:ok, abstract_code}
            _ -> :error
          end
      end
    else
      _ -> :error
    end
  end

  defp get_module_and_beam(module) when is_atom(module) do
    case :code.get_object_code(module) do
      {^module, beam, _filename} -> {module, beam}
      :error -> :error
    end
  end

  defp get_module_and_beam(beam) when is_binary(beam) do
    case :beam_lib.info(beam) do
      [_ | _] = info -> {info[:module], beam}
      _ -> :error
    end
  end

  ## To AST conversion

  defp collect_vars({:ann_type, _line, args}) when is_list(args) do
    []
  end

  defp collect_vars({:type, _line, _kind, args}) when is_list(args) do
    Enum.flat_map(args, &collect_vars/1)
  end

  defp collect_vars({:remote_type, _line, args}) when is_list(args) do
    Enum.flat_map(args, &collect_vars/1)
  end

  defp collect_vars({:typed_record_field, _line, type}) do
    collect_vars(type)
  end

  defp collect_vars({:paren_type, _line, [type]}) do
    collect_vars(type)
  end

  defp collect_vars({:var, _line, var}) do
    [erl_to_ex_var(var)]
  end

  defp collect_vars(_) do
    []
  end

  defp typespec_to_quoted({:user_type, line, name, args}) do
    typespec_to_quoted({:type, line, name, args})
  end

  defp typespec_to_quoted({:type, line, :tuple, :any}) do
    {:tuple, [line: line], []}
  end

  defp typespec_to_quoted({:type, line, :tuple, args}) do
    args = for arg <- args, do: typespec_to_quoted(arg)
    {:{}, [line: line], args}
  end

  defp typespec_to_quoted({:type, _line, :list, [{:type, _, :union, unions} = arg]}) do
    case unpack_typespec_kw(unions, []) do
      {:ok, ast} -> ast
      :error -> [typespec_to_quoted(arg)]
    end
  end

  defp typespec_to_quoted({:type, line, :list, []}) do
    {:list, [line: line], []}
  end

  defp typespec_to_quoted({:type, _line, :list, [arg]}) do
    [typespec_to_quoted(arg)]
  end

  defp typespec_to_quoted({:type, line, :nonempty_list, []}) do
    [{:..., [line: line], nil}]
  end

  defp typespec_to_quoted({:type, line, :nonempty_list, [arg]}) do
    [typespec_to_quoted(arg), {:..., [line: line], nil}]
  end

  defp typespec_to_quoted({:type, line, :map, :any}) do
    {:map, [line: line], []}
  end

  defp typespec_to_quoted({:type, line, :map, fields}) do
    fields =
      Enum.map(fields, fn
        {:type, _, :map_field_assoc, :any} ->
          {{:optional, [], [{:any, [], []}]}, {:any, [], []}}

        {:type, _, :map_field_exact, [{:atom, _, k}, v]} ->
          {k, typespec_to_quoted(v)}

        {:type, _, :map_field_exact, [k, v]} ->
          {{:required, [], [typespec_to_quoted(k)]}, typespec_to_quoted(v)}

        {:type, _, :map_field_assoc, [k, v]} ->
          {{:optional, [], [typespec_to_quoted(k)]}, typespec_to_quoted(v)}
      end)

    {struct, fields} = Keyword.pop(fields, :__struct__)
    map = {:%{}, [line: line], fields}

    if struct do
      {:%, [line: line], [struct, map]}
    else
      map
    end
  end

  defp typespec_to_quoted({:type, line, :binary, [arg1, arg2]}) do
    [arg1, arg2] = for arg <- [arg1, arg2], do: typespec_to_quoted(arg)

    case {typespec_to_quoted(arg1), typespec_to_quoted(arg2)} do
      {arg1, 0} ->
        quote(line: line, do: <<_::unquote(arg1)>>)

      {0, arg2} ->
        quote(line: line, do: <<_::_*unquote(arg2)>>)

      {arg1, arg2} ->
        quote(line: line, do: <<_::unquote(arg1), _::_*unquote(arg2)>>)
    end
  end

  defp typespec_to_quoted({:type, line, :union, args}) do
    args = for arg <- args, do: typespec_to_quoted(arg)
    Enum.reduce(Enum.reverse(args), fn arg, expr -> {:|, [line: line], [arg, expr]} end)
  end

  defp typespec_to_quoted({:type, line, :fun, [{:type, _, :product, args}, result]}) do
    args = for arg <- args, do: typespec_to_quoted(arg)
    [{:->, [line: line], [args, typespec_to_quoted(result)]}]
  end

  defp typespec_to_quoted({:type, line, :fun, [args, result]}) do
    [{:->, [line: line], [[typespec_to_quoted(args)], typespec_to_quoted(result)]}]
  end

  defp typespec_to_quoted({:type, line, :fun, []}) do
    typespec_to_quoted({:type, line, :fun, [{:type, line, :any}, {:type, line, :any, []}]})
  end

  defp typespec_to_quoted({:type, line, :range, [left, right]}) do
    {:.., [line: line], [typespec_to_quoted(left), typespec_to_quoted(right)]}
  end

  defp typespec_to_quoted({:type, _line, nil, []}) do
    []
  end

  defp typespec_to_quoted({:type, line, name, args}) do
    args = for arg <- args, do: typespec_to_quoted(arg)
    {name, [line: line], args}
  end

  defp typespec_to_quoted({:var, line, var}) do
    {erl_to_ex_var(var), line, nil}
  end

  defp typespec_to_quoted({:op, line, op, arg}) do
    {op, [line: line], [typespec_to_quoted(arg)]}
  end

  defp typespec_to_quoted({:remote_type, line, [mod, name, args]}) do
    remote_type(line, mod, name, args)
  end

  defp typespec_to_quoted({:ann_type, line, [var, type]}) do
    {:::, [line: line], [typespec_to_quoted(var), typespec_to_quoted(type)]}
  end

  defp typespec_to_quoted(
         {:typed_record_field, {:record_field, line, {:atom, line1, name}}, type}
       ) do
    typespec_to_quoted({:ann_type, line, [{:var, line1, name}, type]})
  end

  defp typespec_to_quoted({:type, _, :any}) do
    quote(do: ...)
  end

  defp typespec_to_quoted({:paren_type, _, [type]}) do
    typespec_to_quoted(type)
  end

  defp typespec_to_quoted({type, _line, atom}) when is_atom(type) do
    atom
  end

  defp typespec_to_quoted(other), do: other

  ## Helpers

  defp remote_type(line, {:atom, _, :elixir}, {:atom, _, :charlist}, []) do
    typespec_to_quoted({:type, line, :charlist, []})
  end

  defp remote_type(line, {:atom, _, :elixir}, {:atom, _, :nonempty_charlist}, []) do
    typespec_to_quoted({:type, line, :nonempty_charlist, []})
  end

  defp remote_type(line, {:atom, _, :elixir}, {:atom, _, :struct}, []) do
    typespec_to_quoted({:type, line, :struct, []})
  end

  defp remote_type(line, {:atom, _, :elixir}, {:atom, _, :as_boolean}, [arg]) do
    typespec_to_quoted({:type, line, :as_boolean, [arg]})
  end

  defp remote_type(line, {:atom, _, :elixir}, {:atom, _, :nonempty_improper_list}, []) do
    typespec_to_quoted({:type, line, :nonempty_improper_list, [{:any, [], []}, {:any, [], []}]})
  end

  defp remote_type(line, {:atom, _, :elixir}, {:atom, _, :keyword}, args) do
    typespec_to_quoted({:type, line, :keyword, args})
  end

  defp remote_type(line, mod, name, args) do
    args = for arg <- args, do: typespec_to_quoted(arg)
    dot = {:., [line: line], [typespec_to_quoted(mod), typespec_to_quoted(name)]}
    {dot, [line: line], args}
  end

  defp erl_to_ex_var(var) do
    case Atom.to_string(var) do
      <<"_", c::utf8, rest::binary>> ->
        String.to_atom("_#{String.downcase(<<c::utf8>>)}#{rest}")

      <<c::utf8, rest::binary>> ->
        String.to_atom("#{String.downcase(<<c::utf8>>)}#{rest}")
    end
  end

  defp unpack_typespec_kw([{:type, _, :tuple, [{:atom, _, atom}, type]} | t], acc) do
    unpack_typespec_kw(t, [{atom, typespec_to_quoted(type)} | acc])
  end

  defp unpack_typespec_kw([], acc) do
    {:ok, Enum.reverse(acc)}
  end

  defp unpack_typespec_kw(_, _acc) do
    :error
  end
end
