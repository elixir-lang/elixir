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
  Callback for defdelegate entry point.
  """
  def defdelegate_all(funs, opts, env) do
    to = Keyword.get(opts, :to) || raise ArgumentError, "expected to: to be given as argument"
    as = Keyword.get(opts, :as)

    if to == env.module and is_nil(as) do
      raise ArgumentError,
            "defdelegate function is calling itself, which will lead to an infinite loop. You should either change the value of the :to option or specify the :as option"
    end

    if is_list(funs) do
      IO.warn(
        "passing a list to Kernel.defdelegate/2 is deprecated, please define each delegate separately",
        env
      )
    end

    if Keyword.has_key?(opts, :append_first) do
      IO.warn(
        "Kernel.defdelegate/2 :append_first option is deprecated",
        env
      )
    end

    to
  end

  @doc """
  Callback for each function in defdelegate.
  """
  def defdelegate_each(fun, opts) when is_list(opts) do
    # TODO: Remove on v2.0
    append_first? = Keyword.get(opts, :append_first, false)

    {name, args} =
      case fun do
        {:when, _, [_left, right]} ->
          raise ArgumentError,
                "guards are not allowed in defdelegate/2, got: when #{Macro.to_string(right)}"

        _ ->
          case Macro.decompose_call(fun) do
            {_, _} = pair -> pair
            _ -> raise ArgumentError, "invalid syntax in defdelegate #{Macro.to_string(fun)}"
          end
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
  def defstruct(module, fields, bootstrapped?, env) do
    {set, bag} = :elixir_module.data_tables(module)

    if :ets.member(set, {:elixir, :struct}) do
      raise ArgumentError,
            "defstruct has already been called for " <>
              "#{Kernel.inspect(module)}, defstruct can only be called once per module"
    end

    case fields do
      fs when is_list(fs) ->
        :ok

      other ->
        raise ArgumentError, "struct fields definition must be list, got: #{inspect(other)}"
    end

    mapper = fn
      {key, val} when is_atom(key) ->
        key == :__struct__ and raise(ArgumentError, "cannot set :__struct__ in struct definition")

        try do
          :elixir_quote.escape(val, :none, false)
        rescue
          e in [ArgumentError] ->
            raise ArgumentError, "invalid value for struct field #{key}, " <> Exception.message(e)
        else
          _ -> {key, val}
        end

      key when is_atom(key) ->
        key == :__struct__ and raise(ArgumentError, "cannot set :__struct__ in struct definition")
        {key, nil}

      other ->
        raise ArgumentError, "struct field names must be atoms, got: #{inspect(other)}"
    end

    fields = :lists.map(mapper, fields)

    enforce_keys =
      case :ets.lookup(set, :enforce_keys) do
        [{_, enforce_keys, _, _}] when is_list(enforce_keys) ->
          :ets.update_element(set, :enforce_keys, {3, :used})
          enforce_keys

        [{_, enforce_key, _, _}] ->
          :ets.update_element(set, :enforce_keys, {3, :used})
          [enforce_key]

        [] ->
          []
      end

    # TODO: Make it raise on v2.0
    warn_on_duplicate_struct_key(:lists.keysort(1, fields), env)

    foreach = fn
      key when is_atom(key) ->
        :ok

      key ->
        raise ArgumentError, "keys given to @enforce_keys must be atoms, got: #{inspect(key)}"
    end

    :lists.foreach(foreach, enforce_keys)
    struct = :maps.from_list([__struct__: module] ++ fields)
    escaped_struct = :elixir_quote.escape(struct, :none, false)

    body =
      case bootstrapped? do
        true ->
          case enforce_keys do
            [] ->
              quote do
                Enum.reduce(kv, unquote(escaped_struct), fn {key, val}, map ->
                  %{map | key => val}
                end)
              end

            _ ->
              quote do
                {map, keys} =
                  Enum.reduce(kv, {unquote(escaped_struct), unquote(enforce_keys)}, fn
                    {key, val}, {map, keys} ->
                      {%{map | key => val}, List.delete(keys, key)}
                  end)

                case keys do
                  [] ->
                    map

                  _ ->
                    raise ArgumentError,
                          "the following keys must also be given when building " <>
                            "struct #{inspect(__MODULE__)}: #{inspect(keys)}"
                end
              end
          end

        false ->
          quote do
            :lists.foldl(
              fn {key, val}, acc -> %{acc | key => val} end,
              unquote(escaped_struct),
              kv
            )
          end
      end

    case enforce_keys -- :maps.keys(struct) do
      [] ->
        mapper = fn {key, val} ->
          %{field: key, default: val, required: :lists.member(key, enforce_keys)}
        end

        :ets.insert(set, {{:elixir, :struct}, :lists.map(mapper, fields)})
        derive = :lists.map(fn {_, value} -> value end, :ets.take(bag, {:accumulate, :derive}))
        {struct, :lists.reverse(derive), escaped_struct, quote(do: kv), body}

      error_keys ->
        raise ArgumentError,
              "@enforce_keys required keys (#{inspect(error_keys)}) that are not defined in defstruct: " <>
                "#{inspect(fields)}"
    end
  end

  defp warn_on_duplicate_struct_key([], _) do
    :ok
  end

  defp warn_on_duplicate_struct_key([{key, _} | [{key, _} | _] = rest], env) do
    IO.warn("duplicate key #{inspect(key)} found in struct", env)
    warn_on_duplicate_struct_key(rest, env)
  end

  defp warn_on_duplicate_struct_key([_ | rest], env) do
    warn_on_duplicate_struct_key(rest, env)
  end

  @doc """
  Announcing callback for defstruct.
  """
  def announce_struct(module) do
    case :erlang.get(:elixir_compiler_info) do
      :undefined -> :ok
      {pid, _} -> send(pid, {:available, :struct, module})
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
  `value`, but only once, and then reuse the unquoted form throughout the expression.

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
    {expr, _, _} = :elixir_expand.expand(expr, :elixir_env.env_to_ex(env), env)

    quote do
      case Macro.Env.in_guard?(__CALLER__) do
        true -> unquote(literal_quote(unquote_every_ref(expr, vars)))
        false -> unquote(literal_quote(unquote_refs_once(expr, vars, env.module)))
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
  defp unquote_refs_once(guard, refs, module) do
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
                  generated = String.to_atom("arg" <> Integer.to_string(map_size(acc) + 1))
                  new_var = Macro.unique_var(generated, module)
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
