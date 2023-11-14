defmodule Code.Typespec do
  @moduledoc false

  @doc """
  Converts a spec clause back to Elixir quoted expression.
  """
  @spec spec_to_quoted(atom, tuple) :: {atom, keyword, [Macro.t()]}
  def spec_to_quoted(name, spec)

  def spec_to_quoted(name, {:type, anno, :fun, [{:type, _, :product, args}, result]})
      when is_atom(name) do
    meta = meta(anno)
    body = {name, meta, Enum.map(args, &typespec_to_quoted/1)}

    vars =
      for type_expr <- args ++ [result],
          var <- collect_vars(type_expr),
          uniq: true,
          do: {var, {:var, meta, nil}}

    spec = {:"::", meta, [body, typespec_to_quoted(result)]}

    if vars == [] do
      spec
    else
      {:when, meta, [spec, vars]}
    end
  end

  def spec_to_quoted(name, {:type, anno, :fun, []}) when is_atom(name) do
    meta = meta(anno)
    {:"::", meta, [{name, meta, []}, quote(do: term)]}
  end

  def spec_to_quoted(name, {:type, anno, :bounded_fun, [type, constrs]}) when is_atom(name) do
    meta = meta(anno)
    {:type, _, :fun, [{:type, _, :product, args}, result]} = type

    guards =
      for {:type, _, :constraint, [{:atom, _, :is_subtype}, [{:var, _, var}, type]]} <- constrs do
        {erl_to_ex_var(var), typespec_to_quoted(type)}
      end

    ignore_vars = Keyword.keys(guards)

    vars =
      for type_expr <- args ++ [result],
          var <- collect_vars(type_expr),
          var not in ignore_vars,
          uniq: true,
          do: {var, {:var, meta, nil}}

    args = for arg <- args, do: typespec_to_quoted(arg)

    when_args = [
      {:"::", meta, [{name, meta, args}, typespec_to_quoted(result)]},
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
  @spec fetch_specs(module | binary) :: {:ok, [tuple]} | :error
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
  @spec fetch_callbacks(module | binary) :: {:ok, [tuple]} | :error
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
    with {^module, beam, _filename} <- :code.get_object_code(module),
         info_pairs when is_list(info_pairs) <- :beam_lib.info(beam),
         {:ok, ^module} <- Keyword.fetch(info_pairs, :module) do
      {module, beam}
    else
      _ -> :error
    end
  end

  defp get_module_and_beam(beam) when is_binary(beam) do
    case :beam_lib.info(beam) do
      [_ | _] = info -> {info[:module], beam}
      _ -> :error
    end
  end

  ## To AST conversion

  defp collect_vars({:ann_type, _anno, args}) when is_list(args) do
    []
  end

  defp collect_vars({:type, _anno, _kind, args}) when is_list(args) do
    Enum.flat_map(args, &collect_vars/1)
  end

  defp collect_vars({:remote_type, _anno, args}) when is_list(args) do
    Enum.flat_map(args, &collect_vars/1)
  end

  defp collect_vars({:typed_record_field, _anno, type}) do
    collect_vars(type)
  end

  defp collect_vars({:paren_type, _anno, [type]}) do
    collect_vars(type)
  end

  defp collect_vars({:var, _anno, var}) do
    [erl_to_ex_var(var)]
  end

  defp collect_vars(_) do
    []
  end

  defp typespec_to_quoted({:user_type, anno, name, args}) do
    args = for arg <- args, do: typespec_to_quoted(arg)
    {name, meta(anno), args}
  end

  defp typespec_to_quoted({:type, anno, :tuple, :any}) do
    {:tuple, meta(anno), []}
  end

  defp typespec_to_quoted({:type, anno, :tuple, args}) do
    args = for arg <- args, do: typespec_to_quoted(arg)
    {:{}, meta(anno), args}
  end

  defp typespec_to_quoted({:type, _anno, :list, [{:type, _, :union, unions} = arg]}) do
    case unpack_typespec_kw(unions, []) do
      {:ok, ast} -> ast
      :error -> [typespec_to_quoted(arg)]
    end
  end

  defp typespec_to_quoted({:type, anno, :list, []}) do
    {:list, meta(anno), []}
  end

  defp typespec_to_quoted({:type, _anno, :list, [arg]}) do
    [typespec_to_quoted(arg)]
  end

  defp typespec_to_quoted({:type, anno, :nonempty_list, []}) do
    [{:..., meta(anno), nil}]
  end

  defp typespec_to_quoted({:type, anno, :nonempty_list, [arg]}) do
    [typespec_to_quoted(arg), {:..., meta(anno), nil}]
  end

  defp typespec_to_quoted({:type, anno, :map, :any}) do
    {:map, meta(anno), []}
  end

  defp typespec_to_quoted({:type, anno, :map, fields}) do
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

    case List.keytake(fields, :__struct__, 0) do
      {{:__struct__, struct}, fields_pruned} when is_atom(struct) and struct != nil ->
        map_pruned = {:%{}, meta(anno), fields_pruned}
        {:%, meta(anno), [struct, map_pruned]}

      _ ->
        {:%{}, meta(anno), fields}
    end
  end

  defp typespec_to_quoted({:type, anno, :binary, [arg1, arg2]}) do
    [arg1, arg2] = for arg <- [arg1, arg2], do: typespec_to_quoted(arg)
    line = meta(anno)[:line]

    case {typespec_to_quoted(arg1), typespec_to_quoted(arg2)} do
      {arg1, 0} ->
        quote(line: line, do: <<_::unquote(arg1)>>)

      {0, arg2} ->
        quote(line: line, do: <<_::_*unquote(arg2)>>)

      {arg1, arg2} ->
        quote(line: line, do: <<_::unquote(arg1), _::_*unquote(arg2)>>)
    end
  end

  defp typespec_to_quoted({:type, anno, :union, args}) do
    args = for arg <- args, do: typespec_to_quoted(arg)
    Enum.reduce(Enum.reverse(args), fn arg, expr -> {:|, meta(anno), [arg, expr]} end)
  end

  defp typespec_to_quoted({:type, anno, :fun, [{:type, _, :product, args}, result]}) do
    args = for arg <- args, do: typespec_to_quoted(arg)
    [{:->, meta(anno), [args, typespec_to_quoted(result)]}]
  end

  defp typespec_to_quoted({:type, anno, :fun, [args, result]}) do
    [{:->, meta(anno), [[typespec_to_quoted(args)], typespec_to_quoted(result)]}]
  end

  defp typespec_to_quoted({:type, anno, :fun, []}) do
    typespec_to_quoted({:type, anno, :fun, [{:type, anno, :any}, {:type, anno, :any, []}]})
  end

  defp typespec_to_quoted({:type, anno, :range, [left, right]}) do
    {:.., meta(anno), [typespec_to_quoted(left), typespec_to_quoted(right)]}
  end

  defp typespec_to_quoted({:type, _anno, nil, []}) do
    []
  end

  defp typespec_to_quoted({:type, anno, name, args}) do
    args = for arg <- args, do: typespec_to_quoted(arg)
    {name, meta(anno), args}
  end

  defp typespec_to_quoted({:var, anno, var}) do
    {erl_to_ex_var(var), meta(anno), nil}
  end

  defp typespec_to_quoted({:op, anno, op, arg}) do
    {op, meta(anno), [typespec_to_quoted(arg)]}
  end

  defp typespec_to_quoted({:remote_type, anno, [mod, name, args]}) do
    remote_type(anno, mod, name, args)
  end

  defp typespec_to_quoted({:ann_type, anno, [var, type]}) do
    {:"::", meta(anno), [typespec_to_quoted(var), typespec_to_quoted(type)]}
  end

  defp typespec_to_quoted(
         {:typed_record_field, {:record_field, anno1, {:atom, anno2, name}}, type}
       ) do
    typespec_to_quoted({:ann_type, anno1, [{:var, anno2, name}, type]})
  end

  defp typespec_to_quoted({:type, _, :any}) do
    quote(do: ...)
  end

  defp typespec_to_quoted({:paren_type, _, [type]}) do
    typespec_to_quoted(type)
  end

  defp typespec_to_quoted({type, _anno, atom}) when is_atom(type) do
    atom
  end

  defp typespec_to_quoted(other), do: other

  ## Helpers

  defp remote_type(anno, {:atom, _, :elixir}, {:atom, _, :charlist}, []) do
    typespec_to_quoted({:type, anno, :charlist, []})
  end

  defp remote_type(anno, {:atom, _, :elixir}, {:atom, _, :nonempty_charlist}, []) do
    typespec_to_quoted({:type, anno, :nonempty_charlist, []})
  end

  defp remote_type(anno, {:atom, _, :elixir}, {:atom, _, :struct}, []) do
    typespec_to_quoted({:type, anno, :struct, []})
  end

  defp remote_type(anno, {:atom, _, :elixir}, {:atom, _, :as_boolean}, [arg]) do
    typespec_to_quoted({:type, anno, :as_boolean, [arg]})
  end

  defp remote_type(anno, {:atom, _, :elixir}, {:atom, _, :keyword}, args) do
    typespec_to_quoted({:type, anno, :keyword, args})
  end

  defp remote_type(anno, mod, name, args) do
    args = for arg <- args, do: typespec_to_quoted(arg)
    dot = {:., meta(anno), [typespec_to_quoted(mod), typespec_to_quoted(name)]}
    {dot, meta(anno), args}
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

  defp meta(anno) do
    case :erl_anno.location(anno) do
      {line, column} ->
        [line: line, column: column]

      line when is_integer(line) ->
        [line: line]
    end
  end
end
