defmodule Module.Types.Of do
  # Typing functionality shared between Expr and Pattern.
  # Generic AST and Enum helpers go to Module.Types.Helpers.
  @moduledoc false

  # @prefix quote(do: ...)
  # @suffix quote(do: ...)

  alias Module.ParallelChecker
  import Module.Types.{Helpers, Descr}

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
  Handles open maps.
  """
  def open_map(args, stack, context, of_fun) do
    with {:ok, _pairs, context} <- map_pairs(args, stack, context, of_fun) do
      {:ok, map(), context}
    end
  end

  @doc """
  Handles closed maps (without dynamic => dynamic).
  """
  def closed_map(args, stack, context, of_fun) do
    with {:ok, _pairs, context} <- map_pairs(args, stack, context, of_fun) do
      {:ok, map(), context}
    end
  end

  defp map_pairs(pairs, stack, context, of_fun) do
    map_reduce_ok(pairs, context, fn {key, value}, context ->
      with {:ok, key_type, context} <- of_fun.(key, stack, context),
           {:ok, value_type, context} <- of_fun.(value, stack, context),
           do: {:ok, {key_type, value_type}, context}
    end)
  end

  @doc """
  Handles structs.
  """
  def struct(struct, meta, stack, context) do
    context = remote(struct, :__struct__, 0, meta, stack, context)
    {:ok, map(), context}
  end

  ## Binary

  @doc """
  Handles binaries.

  In the stack, we add nodes such as <<expr>>, <<..., expr>>, etc,
  based on the position of the expression within the binary.
  """
  def binary([], _kind, _stack, context, _of_fun) do
    {:ok, context}
  end

  def binary([head], kind, stack, context, of_fun) do
    # stack = push_expr_stack({:<<>>, get_meta(head), [head]}, stack)
    binary_segment(head, kind, stack, context, of_fun)
  end

  def binary([head | tail], kind, stack, context, of_fun) do
    # stack = push_expr_stack({:<<>>, get_meta(head), [head, @suffix]}, stack)

    case binary_segment(head, kind, stack, context, of_fun) do
      {:ok, context} -> binary_many(tail, kind, stack, context, of_fun)
      {:error, reason} -> {:error, reason}
    end
  end

  defp binary_many([last], kind, stack, context, of_fun) do
    # stack = push_expr_stack({:<<>>, get_meta(last), [@prefix, last]}, stack)
    binary_segment(last, kind, stack, context, of_fun)
  end

  defp binary_many([head | tail], kind, stack, context, of_fun) do
    # stack = push_expr_stack({:<<>>, get_meta(head), [@prefix, head, @suffix]}, stack)

    case binary_segment(head, kind, stack, context, of_fun) do
      {:ok, context} -> binary_many(tail, kind, stack, context, of_fun)
      {:error, reason} -> {:error, reason}
    end
  end

  # If the segment is a literal, the compiler has already checked its validity,
  # so we just skip it.
  defp binary_segment({:"::", _meta, [expr, _specifiers]}, _kind, _stack, context, _of_fun)
       when is_binary(expr) or is_number(expr) do
    {:ok, context}
  end

  defp binary_segment({:"::", _meta, [expr, specifiers]}, kind, stack, context, of_fun) do
    # TODO: unpack specifiers once
    expected_type =
      collect_binary_specifier(specifiers, &binary_type(kind, &1)) || :integer

    utf? = collect_binary_specifier(specifiers, &utf_type?/1)
    float? = collect_binary_specifier(specifiers, &float_type?/1)

    # Special case utf and float specifiers because they can be two types as literals
    # but only a specific type as a variable in a pattern
    cond do
      kind == :pattern and utf? and is_binary(expr) ->
        {:ok, context}

      kind == :pattern and float? and is_integer(expr) ->
        {:ok, context}

      true ->
        with {:ok, _type, context} <- of_fun.(expr, expected_type, stack, context),
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
  def remote(module, fun, arity, meta, stack, context) when is_atom(module) do
    if Keyword.get(meta, :context_module, false) do
      context
    else
      ParallelChecker.preload_module(stack.cache, module)
      check_export(module, fun, arity, meta, stack, context)
    end
  end

  def remote(_module, _fun, _arity, _meta, _stack, context), do: context

  defp check_export(module, fun, arity, meta, stack, context) do
    case ParallelChecker.fetch_export(stack.cache, module, fun, arity) do
      {:ok, mode, :def, reason} ->
        check_deprecated(mode, module, fun, arity, reason, meta, stack, context)

      {:ok, mode, :defmacro, reason} ->
        context = warn({:unrequired_module, module, fun, arity}, meta, stack, context)
        check_deprecated(mode, module, fun, arity, reason, meta, stack, context)

      {:error, :module} ->
        if warn_undefined?(module, fun, arity, stack) do
          warn({:undefined_module, module, fun, arity}, meta, stack, context)
        else
          context
        end

      {:error, :function} ->
        if warn_undefined?(module, fun, arity, stack) do
          exports = ParallelChecker.all_exports(stack.cache, module)
          warn({:undefined_function, module, fun, arity, exports}, meta, stack, context)
        else
          context
        end
    end
  end

  defp check_deprecated(:elixir, module, fun, arity, reason, meta, stack, context) do
    if reason do
      warn({:deprecated, module, fun, arity, reason}, meta, stack, context)
    else
      context
    end
  end

  defp check_deprecated(:erlang, module, fun, arity, _reason, meta, stack, context) do
    case :otp_internal.obsolete(module, fun, arity) do
      {:deprecated, string} when is_list(string) ->
        reason = string |> List.to_string() |> :string.titlecase()
        warn({:deprecated, module, fun, arity, reason}, meta, stack, context)

      {:deprecated, string, removal} when is_list(string) and is_list(removal) ->
        reason = string |> List.to_string() |> :string.titlecase()
        reason = "It will be removed in #{removal}. #{reason}"
        warn({:deprecated, module, fun, arity, reason}, meta, stack, context)

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
  defp warn_undefined?(_module, :__impl__, 1, _stack), do: false
  defp warn_undefined?(_module, :module_info, 0, _stack), do: false
  defp warn_undefined?(_module, :module_info, 1, _stack), do: false
  defp warn_undefined?(:erlang, :orelse, 2, _stack), do: false
  defp warn_undefined?(:erlang, :andalso, 2, _stack), do: false

  defp warn_undefined?(_, _, _, %{no_warn_undefined: :all}) do
    false
  end

  defp warn_undefined?(module, fun, arity, stack) do
    not Enum.any?(stack.no_warn_undefined, &(&1 == module or &1 == {module, fun, arity}))
  end

  defp warn(warning, meta, stack, context) do
    warn(__MODULE__, warning, meta, stack, context)
  end

  ## Warning formatting

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
