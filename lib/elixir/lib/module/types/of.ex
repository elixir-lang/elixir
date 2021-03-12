defmodule Module.Types.Of do
  # Typing functionality shared between Expr and Pattern.
  # Generic AST and Enum helpers go to Module.Types.Helpers.
  @moduledoc false

  @prefix quote(do: ...)
  @suffix quote(do: ...)

  alias Module.ParallelChecker

  import Module.Types.Helpers
  import Module.Types.Unify

  # There are important assumptions on how we work with maps.
  #
  # First, the keys in the map must be ordered by subtyping.
  #
  # Second, optional keys must be a superset of the required
  # keys, i.e. %{required(atom) => integer, optional(:foo) => :bar}
  # is forbidden.
  #
  # Third, in order to preserve co/contra-variance, a supertype
  # must satisfy its subtypes. I.e. %{foo: :bar, atom() => :baz}
  # is forbidden, it must be %{foo: :bar, atom() => :baz | :bar}.
  #
  # Once we support user declared maps, we need to validate these
  # assumptions.

  @doc """
  Handles open maps (with dynamic => dynamic).
  """
  def open_map(args, stack, context, of_fun) do
    with {:ok, pairs, context} <- map_pairs(args, stack, context, of_fun) do
      # If we match on a map such as %{"foo" => "bar"}, we cannot
      # assert that %{binary() => binary()}, since we are matching
      # only a single binary of infinite possible values. Therefore,
      # the correct would be to match it to %{binary() => binary() | var}.
      #
      # We can skip this in two cases:
      #
      #   1. If the key is a singleton, then we know that it has no
      #      other value than the current one
      #
      #   2. If the value is a variable, then there is no benefit in
      #      creating another variable, so we can skip it
      #
      # For now, we skip generating the var itself and introduce
      # :dynamic instead.
      pairs =
        for {key, value} <- pairs, not has_unbound_var?(key, context) do
          if singleton?(key, context) or match?({:var, _}, value) do
            {key, value}
          else
            {key, to_union([value, :dynamic], context)}
          end
        end

      triplets = pairs_to_unions(pairs, [], context) ++ [{:optional, :dynamic, :dynamic}]
      {:ok, {:map, triplets}, context}
    end
  end

  @doc """
  Handles closed maps (without dynamic => dynamic).
  """
  def closed_map(args, stack, context, of_fun) do
    with {:ok, pairs, context} <- map_pairs(args, stack, context, of_fun) do
      {:ok, {:map, closed_to_unions(pairs, context)}, context}
    end
  end

  defp map_pairs(pairs, stack, context, of_fun) do
    map_reduce_ok(pairs, context, fn {key, value}, context ->
      with {:ok, key_type, context} <- of_fun.(key, :dynamic, stack, context),
           {:ok, value_type, context} <- of_fun.(value, :dynamic, stack, context),
           do: {:ok, {key_type, value_type}, context}
    end)
  end

  defp closed_to_unions([{key, value}], _context), do: [{:required, key, value}]

  defp closed_to_unions(pairs, context) do
    case Enum.split_with(pairs, fn {key, _value} -> has_unbound_var?(key, context) end) do
      {[], pairs} -> pairs_to_unions(pairs, [], context)
      {[_ | _], pairs} -> pairs_to_unions([{:dynamic, :dynamic} | pairs], [], context)
    end
  end

  defp pairs_to_unions([{key, value} | ahead], behind, context) do
    {matched_ahead, values} = find_matching_values(ahead, key, [], [])

    # In case nothing matches, use the original ahead
    ahead = matched_ahead || ahead

    all_values =
      [value | values] ++
        find_subtype_values(ahead, key, context) ++
        find_subtype_values(behind, key, context)

    pairs_to_unions(ahead, [{key, to_union(all_values, context)} | behind], context)
  end

  defp pairs_to_unions([], acc, context) do
    acc
    |> Enum.sort(&subtype?(elem(&1, 0), elem(&2, 0), context))
    |> Enum.map(fn {key, value} -> {:required, key, value} end)
  end

  defp find_subtype_values(pairs, key, context) do
    for {pair_key, pair_value} <- pairs, subtype?(pair_key, key, context), do: pair_value
  end

  defp find_matching_values([{key, value} | ahead], key, acc, values) do
    find_matching_values(ahead, key, acc, [value | values])
  end

  defp find_matching_values([{_, _} = pair | ahead], key, acc, values) do
    find_matching_values(ahead, key, [pair | acc], values)
  end

  defp find_matching_values([], _key, acc, [_ | _] = values), do: {Enum.reverse(acc), values}
  defp find_matching_values([], _key, _acc, []), do: {nil, []}

  @doc """
  Handles structs.
  """
  def struct(struct, meta, context) do
    context = remote(struct, :__struct__, 0, meta, context)

    entries =
      for key <- Map.keys(struct.__struct__()), key != :__struct__ do
        {:required, {:atom, key}, :dynamic}
      end

    {:ok, {:map, [{:required, {:atom, :__struct__}, {:atom, struct}} | entries]}, context}
  end

  ## Binary

  @doc """
  Handles binaries.

  In the stack, we add nodes such as <<expr>>, <<..., expr>>, etc,
  based on the position of the expression within the binary.
  """
  def binary([], _stack, context, _of_fun) do
    {:ok, context}
  end

  def binary([head], stack, context, of_fun) do
    head_stack = push_expr_stack({:<<>>, get_meta(head), [head]}, stack)
    binary_segment(head, head_stack, context, of_fun)
  end

  def binary([head | tail], stack, context, of_fun) do
    head_stack = push_expr_stack({:<<>>, get_meta(head), [head, @suffix]}, stack)

    case binary_segment(head, head_stack, context, of_fun) do
      {:ok, context} -> binary_many(tail, stack, context, of_fun)
      {:error, reason} -> {:error, reason}
    end
  end

  defp binary_many([last], stack, context, of_fun) do
    last_stack = push_expr_stack({:<<>>, get_meta(last), [@prefix, last]}, stack)
    binary_segment(last, last_stack, context, of_fun)
  end

  defp binary_many([head | tail], stack, context, of_fun) do
    head_stack = push_expr_stack({:<<>>, get_meta(head), [@prefix, head, @suffix]}, stack)

    case binary_segment(head, head_stack, context, of_fun) do
      {:ok, context} -> binary_many(tail, stack, context, of_fun)
      {:error, reason} -> {:error, reason}
    end
  end

  defp binary_segment({:"::", _meta, [expr, specifiers]}, stack, context, of_fun) do
    expected_type =
      collect_binary_specifier(specifiers, &binary_type(stack.context, &1)) || :integer

    utf? = collect_binary_specifier(specifiers, &utf_type?/1)
    float? = collect_binary_specifier(specifiers, &float_type?/1)

    # Special case utf and float specifiers because they can be two types as literals
    # but only a specific type as a variable in a pattern
    cond do
      stack.context == :pattern and utf? and is_binary(expr) ->
        {:ok, context}

      stack.context == :pattern and float? and is_integer(expr) ->
        {:ok, context}

      true ->
        with {:ok, type, context} <- of_fun.(expr, expected_type, stack, context),
             {:ok, _type, context} <- unify(type, expected_type, stack, context),
             do: {:ok, context}
    end
  end

  # Collect binary type specifiers,
  # from `<<pattern::integer-size(10)>>` collect `integer`
  defp collect_binary_specifier({:-, _meta, [left, right]}, fun) do
    collect_binary_specifier(left, fun) || collect_binary_specifier(right, fun)
  end

  defp collect_binary_specifier(other, fun) do
    fun.(other)
  end

  defp binary_type(:expr, {:float, _, _}), do: {:union, [:integer, :float]}
  defp binary_type(:expr, {:utf8, _, _}), do: {:union, [:integer, :binary]}
  defp binary_type(:expr, {:utf16, _, _}), do: {:union, [:integer, :binary]}
  defp binary_type(:expr, {:utf32, _, _}), do: {:union, [:integer, :binary]}
  defp binary_type(:pattern, {:utf8, _, _}), do: :integer
  defp binary_type(:pattern, {:utf16, _, _}), do: :integer
  defp binary_type(:pattern, {:utf32, _, _}), do: :integer
  defp binary_type(:pattern, {:float, _, _}), do: :float
  defp binary_type(_context, {:integer, _, _}), do: :integer
  defp binary_type(_context, {:bits, _, _}), do: :binary
  defp binary_type(_context, {:bitstring, _, _}), do: :binary
  defp binary_type(_context, {:bytes, _, _}), do: :binary
  defp binary_type(_context, {:binary, _, _}), do: :binary
  defp binary_type(_context, _specifier), do: nil

  defp utf_type?({specifier, _, _}), do: specifier in [:utf8, :utf16, :utf32]
  defp utf_type?(_), do: false

  defp float_type?({:float, _, _}), do: true
  defp float_type?(_), do: false

  ## Remote

  @doc """
  Handles remote calls.
  """
  def remote(module, fun, arity, meta, context) when is_atom(module) do
    # TODO: In the future we may want to warn for modules defined
    # in the local context
    if Keyword.get(meta, :context_module, false) and context.module != module do
      context
    else
      ParallelChecker.preload_module(context.cache, module)
      check_export(module, fun, arity, meta, context)
    end
  end

  def remote(_module, _fun, _arity, _meta, context), do: context

  defp check_export(module, fun, arity, meta, context) do
    case ParallelChecker.fetch_export(context.cache, module, fun, arity) do
      {:ok, mode, :def, reason} ->
        check_deprecated(mode, module, fun, arity, reason, meta, context)

      {:ok, mode, :defmacro, reason} ->
        context = warn(meta, context, {:unrequired_module, module, fun, arity})
        check_deprecated(mode, module, fun, arity, reason, meta, context)

      {:error, :module} ->
        if warn_undefined?(module, fun, arity, context) do
          warn(meta, context, {:undefined_module, module, fun, arity})
        else
          context
        end

      {:error, :function} ->
        if warn_undefined?(module, fun, arity, context) do
          exports = ParallelChecker.all_exports(context.cache, module)
          warn(meta, context, {:undefined_function, module, fun, arity, exports})
        else
          context
        end
    end
  end

  defp check_deprecated(:elixir, module, fun, arity, reason, meta, context) do
    if reason do
      warn(meta, context, {:deprecated, module, fun, arity, reason})
    else
      context
    end
  end

  defp check_deprecated(:erlang, module, fun, arity, _reason, meta, context) do
    case :otp_internal.obsolete(module, fun, arity) do
      {:deprecated, string} when is_list(string) ->
        reason = string |> List.to_string() |> String.capitalize()
        warn(meta, context, {:deprecated, module, fun, arity, reason})

      {:deprecated, string, removal} when is_list(string) and is_list(removal) ->
        reason = string |> List.to_string() |> String.capitalize()
        reason = "It will be removed in #{removal}. #{reason}"
        warn(meta, context, {:deprecated, module, fun, arity, reason})

      _ ->
        context
    end
  end

  # The protocol code dispatches to unknown modules, so we ignore them here.
  #
  #     try do
  #       SomeProtocol.Atom.__impl__
  #     rescue
  #       ...
  #     end
  #
  # But for protocols we don't want to traverse the protocol code anyway.
  # TODO: remove this clause once we no longer traverse the protocol code.
  defp warn_undefined?(_module, :__impl__, 1, _context), do: false
  defp warn_undefined?(_module, :module_info, 0, _context), do: false
  defp warn_undefined?(_module, :module_info, 1, _context), do: false
  defp warn_undefined?(:erlang, :orelse, 2, _context), do: false
  defp warn_undefined?(:erlang, :andalso, 2, _context), do: false

  defp warn_undefined?(_, _, _, %{no_warn_undefined: :all}) do
    false
  end

  defp warn_undefined?(module, fun, arity, context) do
    not Enum.any?(context.no_warn_undefined, &(&1 == module or &1 == {module, fun, arity}))
  end

  defp warn(meta, context, warning) do
    {fun, arity} = context.function
    location = {context.file, meta[:line] || 0, {context.module, fun, arity}}
    %{context | warnings: [{__MODULE__, warning, location} | context.warnings]}
  end

  ## Warning formating

  def format_warning({:undefined_module, module, fun, arity}) do
    [
      Exception.format_mfa(module, fun, arity),
      " is undefined (module ",
      inspect(module),
      " is not available or is yet to be defined)"
    ]
  end

  def format_warning({:undefined_function, module, fun, arity, exports}) do
    [
      Exception.format_mfa(module, fun, arity),
      " is undefined or private",
      UndefinedFunctionError.hint_for_loaded_module(module, fun, arity, exports)
    ]
  end

  def format_warning({:deprecated, module, fun, arity, reason}) do
    [
      Exception.format_mfa(module, fun, arity),
      " is deprecated. ",
      reason
    ]
  end

  def format_warning({:unrequired_module, module, fun, arity}) do
    [
      "you must require ",
      inspect(module),
      " before invoking the macro ",
      Exception.format_mfa(module, fun, arity)
    ]
  end
end
