defmodule Module.Types.Of do
  # Typing functionality shared between Expr and Pattern.
  # Generic AST and Enum helpers go to Module.Types.Helpers.
  @moduledoc false

  alias Module.ParallelChecker
  import Module.Types.{Helpers, Descr}

  @prefix quote(do: ...)
  @suffix quote(do: ...)

  @integer_or_float union(integer(), float())
  @integer_or_binary union(integer(), binary())
  @integer integer()
  @float float()
  @binary binary()

  @doc """
  Handles fetching a map key.
  """
  def map_fetch(expr, type, field, stack, context) when is_atom(field) do
    case map_fetch(type, field) do
      :error ->
        {:ok, dynamic(),
         warn({:badmap, expr, type, field, context}, elem(expr, 1), stack, context)}

      # TODO: on static type checking, we want check it is not optional.
      {_optional?, value_type} ->
        if empty?(value_type) do
          {:ok, dynamic(),
           warn({:badkey, expr, type, field, context}, elem(expr, 1), stack, context)}
        else
          {:ok, value_type, context}
        end
    end
  end

  @doc """
  Handles open maps.
  """
  def open_map(args, stack, context, of_fun) do
    with {:ok, _pairs, context} <- map_pairs(args, stack, context, of_fun) do
      {:ok, open_map(), context}
    end
  end

  @doc """
  Handles closed maps (without dynamic => dynamic).
  """
  def closed_map(args, stack, context, of_fun) do
    with {:ok, _pairs, context} <- map_pairs(args, stack, context, of_fun) do
      {:ok, open_map(), context}
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
  def struct({:%, meta, _}, struct, args, defaults?, stack, context, of_fun)
      when is_atom(struct) do
    context = preload_and_maybe_check_export(struct, :__struct__, 0, meta, stack, context)

    # The compiler has already checked the keys are atoms and which ones are required.
    # TODO: Type check and do not assume dynamic for non-specified keys.
    with {:ok, pairs, context} <-
           map_reduce_ok(args, context, fn {key, value}, context when is_atom(key) ->
             with {:ok, type, context} <- of_fun.(value, stack, context) do
               {:ok, {key, type}, context}
             end
           end) do
      defaults =
        if defaults? do
          dynamic = dynamic()

          for key <- Map.keys(struct.__struct__()), key != :__struct__ do
            {key, dynamic}
          end
        else
          []
        end

      {:ok, closed_map([{:__struct__, atom([struct])} | defaults] ++ pairs), context}
    end
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
    binary_segment(head, kind, [head], stack, context, of_fun)
  end

  def binary([head | tail], kind, stack, context, of_fun) do
    case binary_segment(head, kind, [head, @suffix], stack, context, of_fun) do
      {:ok, context} -> binary_many(tail, kind, stack, context, of_fun)
      {:error, reason} -> {:error, reason}
    end
  end

  defp binary_many([last], kind, stack, context, of_fun) do
    binary_segment(last, kind, [@prefix, last], stack, context, of_fun)
  end

  defp binary_many([head | tail], kind, stack, context, of_fun) do
    case binary_segment(head, kind, [@prefix, head, @suffix], stack, context, of_fun) do
      {:ok, context} -> binary_many(tail, kind, stack, context, of_fun)
      {:error, reason} -> {:error, reason}
    end
  end

  # If the segment is a literal, the compiler has already checked its validity,
  # so we just skip it.
  defp binary_segment({:"::", _meta, [left, _right]}, _kind, _args, _stack, context, _of_fun)
       when is_binary(left) or is_number(left) do
    {:ok, context}
  end

  defp binary_segment({:"::", meta, [left, right]}, kind, args, stack, context, of_fun) do
    expected_type = specifier_info(kind, right)
    expr = {:<<>>, meta, args}

    with {:ok, actual_type, context} <- of_fun.(left, {expected_type, expr}, stack, context) do
      # If we are in a pattern and we have a variable, the refinement
      # will already have checked the type, so we skip the check here
      # as an optimization.
      if (kind == :pattern and is_var(left)) or compatible?(actual_type, expected_type) do
        {:ok, context}
      else
        hints = if meta[:inferred_bitstring_spec], do: [:inferred_bitstring_spec], else: []
        {:error, incompatible_warn(expr, expected_type, actual_type, hints, meta, stack, context)}
      end
    end
  end

  defp specifier_info(kind, {:-, _, [left, _right]}), do: specifier_info(kind, left)
  defp specifier_info(:expr, {:float, _, _}), do: @integer_or_float
  defp specifier_info(:expr, {:utf8, _, _}), do: @integer_or_binary
  defp specifier_info(:expr, {:utf16, _, _}), do: @integer_or_binary
  defp specifier_info(:expr, {:utf32, _, _}), do: @integer_or_binary
  defp specifier_info(:pattern, {:utf8, _, _}), do: @integer
  defp specifier_info(:pattern, {:utf16, _, _}), do: @integer
  defp specifier_info(:pattern, {:utf32, _, _}), do: @integer
  defp specifier_info(:pattern, {:float, _, _}), do: @float
  defp specifier_info(_kind, {:integer, _, _}), do: @integer
  defp specifier_info(_kind, {:bits, _, _}), do: @binary
  defp specifier_info(_kind, {:bitstring, _, _}), do: @binary
  defp specifier_info(_kind, {:bytes, _, _}), do: @binary
  defp specifier_info(_kind, {:binary, _, _}), do: @binary
  defp specifier_info(_kind, _specifier), do: @integer

  ## Remote

  def remote(expr, type, fun, arity, hints, meta, stack, context) do
    case atom_fetch(type) do
      {:ok, mods} ->
        context =
          Enum.reduce(mods, context, fn mod, context ->
            preload_and_maybe_check_export(mod, fun, arity, meta, stack, context)
          end)

        {mods, context}

      :error ->
        warning = {:badmodule, expr, type, fun, arity, hints, context}
        {[], warn(warning, meta, stack, context)}
    end
  end

  def remote(module, fun, arity, meta, stack, context) do
    preload_and_maybe_check_export(module, fun, arity, meta, stack, context)
  end

  defp preload_and_maybe_check_export(module, fun, arity, meta, stack, context)
       when is_atom(module) do
    if Keyword.get(meta, :runtime_module, false) do
      context
    else
      ParallelChecker.preload_module(stack.cache, module)
      check_export(module, fun, arity, meta, stack, context)
    end
  end

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

  ## Warning helpers

  @doc """
  Emits incompatible types warning for the given expression
  """
  def incompatible_warn(expr, expected_type, actual_type, hints \\ [], meta, stack, context) do
    warning = {:incompatible, expr, expected_type, actual_type, hints, context}
    warn(__MODULE__, warning, meta, stack, context)
  end

  defp warn(warning, meta, stack, context) do
    warn(__MODULE__, warning, meta, stack, context)
  end

  ## Warning formatting

  def format_warning({:incompatible, expr, expected_type, actual_type, hints, context}) do
    {traces, trace_hints} = Module.Types.Pattern.format_traces(expr, context)

    [
      """
      incompatible types in expression:

          #{Macro.to_string(expr)}

      expected type:

          #{to_quoted_string(expected_type)}

      but got type:

          #{to_quoted_string(actual_type)}
      """,
      traces,
      format_hints(hints ++ trace_hints),
      "\ntyping violation found at:"
    ]
  end

  def format_warning({:badmap, expr, type, key, context}) do
    {traces, trace_hints} = Module.Types.Pattern.format_traces(expr, context)

    [
      """
      expected a map or struct when accessing .#{key} in expression:

          #{Macro.to_string(expr)}

      but got type:

          #{to_quoted_string(type)}
      """,
      traces,
      format_hints([:dot | trace_hints]),
      "\ntyping violation found at:"
    ]
  end

  def format_warning({:badkey, expr, type, key, context}) do
    {traces, trace_hints} = Module.Types.Pattern.format_traces(expr, context)

    [
      """
      missing key .#{key} in expression:

          #{Macro.to_string(expr)}

      the given type does not have the given key:

          #{to_quoted_string(type)}
      """,
      traces,
      format_hints([:dot | trace_hints]),
      "\ntyping violation found at:"
    ]
  end

  def format_warning({:badmodule, expr, type, fun, arity, hints, context}) do
    {traces, trace_hints} = Module.Types.Pattern.format_traces(expr, context)

    [
      """
      expected a module (an atom) when invoking #{fun}/#{arity} in expression:

          #{Macro.to_string(expr)}

      but got type:

          #{to_quoted_string(type)}
      """,
      traces,
      format_hints(hints ++ trace_hints),
      "\ntyping violation found at:"
    ]
  end

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
