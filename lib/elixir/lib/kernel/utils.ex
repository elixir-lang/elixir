import Kernel, except: [destructure: 2, defdelegate: 2, defstruct: 2]

defmodule Kernel.Utils do
  @moduledoc false

  @doc """
  Callback for destructure.
  """
  def destructure(list, count)
      when is_list(list) and is_integer(count) and count >= 0,
      do: destructure_list(list, count)

  def destructure(nil, count)
      when is_integer(count) and count >= 0,
      do: destructure_nil(count)

  defp destructure_list(_, 0), do: []
  defp destructure_list([], count), do: destructure_nil(count)
  defp destructure_list([h | t], count), do: [h | destructure_list(t, count - 1)]

  defp destructure_nil(0), do: []
  defp destructure_nil(count), do: [nil | destructure_nil(count - 1)]

  @doc """
  Callback for defdelegate.
  """
  def defdelegate(fun, opts) when is_list(opts) do
    # TODO: Remove on v2.0
    append_first? = Keyword.get(opts, :append_first, false)

    {name, args} =
      case Macro.decompose_call(fun) do
        {_, _} = pair -> pair
        _ -> raise ArgumentError, "invalid syntax in defdelegate #{Macro.to_string(fun)}"
      end

    as = Keyword.get(opts, :as, name)
    as_args = build_as_args(args, append_first?)

    {name, args, as, as_args}
  end

  defp build_as_args(args, append_first?) do
    as_args = :lists.map(&build_as_arg/1, args)

    case append_first? do
      true -> tl(as_args) ++ [hd(as_args)]
      false -> as_args
    end
  end

  defp build_as_arg({:\\, _, [arg, _default_arg]}), do: validate_arg(arg)
  defp build_as_arg(arg), do: validate_arg(arg)

  defp validate_arg({name, _, mod} = arg) when is_atom(name) and is_atom(mod) do
    arg
  end

  defp validate_arg(ast) do
    raise ArgumentError,
          "defdelegate/2 only accepts function parameters, got: #{Macro.to_string(ast)}"
  end

  @doc """
  Callback for defstruct.
  """
  def defstruct(module, fields) do
    case fields do
      fs when is_list(fs) ->
        :ok

      other ->
        raise ArgumentError, "struct fields definition must be list, got: #{inspect(other)}"
    end

    mapper = fn
      {key, val} when is_atom(key) ->
        try do
          Macro.escape(val)
        rescue
          e in [ArgumentError] ->
            raise ArgumentError, "invalid value for struct field #{key}, " <> Exception.message(e)
        else
          _ -> {key, val}
        end

      key when is_atom(key) ->
        {key, nil}

      other ->
        raise ArgumentError, "struct field names must be atoms, got: #{inspect(other)}"
    end

    fields = :lists.map(mapper, fields)
    enforce_keys = List.wrap(Module.get_attribute(module, :enforce_keys))

    # TODO: Make it raise on v2.0
    warn_on_duplicate_struct_key(:lists.keysort(1, fields))

    foreach = fn
      key when is_atom(key) ->
        :ok

      key ->
        raise ArgumentError, "keys given to @enforce_keys must be atoms, got: #{inspect(key)}"
    end

    :lists.foreach(foreach, enforce_keys)

    struct = :maps.put(:__struct__, module, :maps.from_list(fields))
    {struct, enforce_keys, Module.get_attribute(module, :derive)}
  end

  defp warn_on_duplicate_struct_key([]) do
    :ok
  end

  defp warn_on_duplicate_struct_key([{key, _} | [{key, _} | _] = rest]) do
    IO.warn("duplicate key #{inspect(key)} found in struct")
    warn_on_duplicate_struct_key(rest)
  end

  defp warn_on_duplicate_struct_key([_ | rest]) do
    warn_on_duplicate_struct_key(rest)
  end

  @doc """
  Announcing callback for defstruct.
  """
  def announce_struct(module) do
    case :erlang.get(:elixir_compiler_pid) do
      :undefined -> :ok
      pid -> send(pid, {:available, :struct, module})
    end
  end

  @doc """
  Callback for raise.
  """
  def raise(msg) when is_binary(msg) do
    RuntimeError.exception(msg)
  end

  def raise(module) when is_atom(module) do
    module.exception([])
  end

  def raise(%_{__exception__: true} = exception) do
    exception
  end

  def raise(other) do
    ArgumentError.exception(
      "raise/1 and reraise/2 expect a module name, string or exception " <>
        "as the first argument, got: #{inspect(other)}"
    )
  end

  @doc """
  Callback for defguard.

  Rewrites an expression so it can be used both inside and outside a guard.

  Take, for example, the expression:

      is_integer(value) and rem(value, 2) == 0

  If we wanted to create a macro, `is_even`, from this expression, that could be
  used in guards, we'd have to take several things into account.

  First, if this expression is being used inside a guard, `value` needs to be
  unquoted each place it occurs, since it has not yet been at that point in our
  macro.

  Secondly, if the expression is being used outside of a guard, we want to unquote
  `value`, but only once, and then re-use the unquoted form throughout the expression.

  This helper does exactly that: takes the AST for an expression and a list of
  variable references it should be aware of, and rewrites it into a new expression
  that checks for its presence in a guard, then unquotes the variable references as
  appropriate.

  The following code

      expression = quote do: is_integer(value) and rem(value, 2) == 0
      variable_references = [value: Elixir]
      Kernel.Utils.defguard(expression, variable_references) |> Macro.to_string() |> IO.puts()

  would print a code similar to:

      case Macro.Env.in_guard?(__CALLER__) do
        true ->
          quote do
            is_integer(unquote(value)) and rem(unquote(value), 2) == 0
          end

        false ->
          quote do
            value = unquote(value)
            is_integer(value) and rem(value, 2) == 0
          end
      end

  """
  defmacro defguard(args, expr) do
    defguard(args, expr, __CALLER__)
  end

  @spec defguard([Macro.t()], Macro.t(), Macro.Env.t()) :: Macro.t()
  def defguard(args, expr, env) do
    {^args, vars} = extract_refs_from_args(args)
    env = :elixir_env.with_vars(%{env | context: :guard}, vars)
    {expr, _scope} = :elixir_expand.expand(expr, env)

    quote do
      case Macro.Env.in_guard?(__CALLER__) do
        true -> unquote(literal_quote(unquote_every_ref(expr, vars)))
        false -> unquote(literal_quote(unquote_refs_once(expr, vars)))
      end
    end
  end

  defp extract_refs_from_args(args) do
    Macro.postwalk(args, [], fn
      {ref, meta, context} = var, acc when is_atom(ref) and is_atom(context) ->
        {var, [{ref, var_context(meta, context)} | acc]}

      node, acc ->
        {node, acc}
    end)
  end

  # Finds every reference to `refs` in `guard` and wraps them in an unquote.
  defp unquote_every_ref(guard, refs) do
    Macro.postwalk(guard, fn
      {ref, meta, context} = var when is_atom(ref) and is_atom(context) ->
        case {ref, var_context(meta, context)} in refs do
          true -> literal_unquote(var)
          false -> var
        end

      node ->
        node
    end)
  end

  # Prefaces `guard` with unquoted versions of `refs`.
  defp unquote_refs_once(guard, refs) do
    {guard, used_refs} =
      Macro.postwalk(guard, %{}, fn
        {ref, meta, context} = var, acc when is_atom(ref) and is_atom(context) ->
          pair = {ref, var_context(meta, context)}

          case pair in refs do
            true ->
              case acc do
                %{^pair => {new_var, _}} ->
                  {new_var, acc}

                %{} ->
                  generated = String.to_atom("arg" <> Integer.to_string(map_size(acc)))
                  new_var = Macro.var(generated, Elixir)
                  {new_var, Map.put(acc, pair, {new_var, var})}
              end

            false ->
              {var, acc}
          end

        node, acc ->
          {node, acc}
      end)

    all_used = for ref <- :lists.reverse(refs), used = :maps.get(ref, used_refs, nil), do: used
    {vars, exprs} = :lists.unzip(all_used)

    quote do
      {unquote_splicing(vars)} = {unquote_splicing(Enum.map(exprs, &literal_unquote/1))}
      unquote(guard)
    end
  end

  defp literal_quote(ast) do
    {:quote, [], [[do: ast]]}
  end

  defp literal_unquote(ast) do
    {:unquote, [], List.wrap(ast)}
  end

  defp var_context(meta, kind) do
    case :lists.keyfind(:counter, 1, meta) do
      {:counter, counter} -> counter
      false -> kind
    end
  end
end
