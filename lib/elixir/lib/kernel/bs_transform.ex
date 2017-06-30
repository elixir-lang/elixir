defmodule Kernel.BSTransform do
  @doc false

  defmacro __using__(_) do
    quote do
      @compile {:core_transform, Kernel.BSTransform}
      import Kernel.BSTransform, only: [init_writable: 1, private_append: 1]
    end
  end

  defmacro init_writable(int) do
    quote generated: true, location: :keep do
      unquote(__MODULE__).__init_writable__(unquote(int))
    end
  end

  defmacro private_append({tag, _, _} = binary) when tag in [:<<>>, :<>] do
    quote generated: true, location: :keep do
      unquote(__MODULE__).__private_append__(unquote(binary))
    end
  end

  defmacro private_append(quoted) do
    raise ArgumentError,
      "expected quoted binary, got: #{Macro.to_string(quoted)}"
  end

  @spec core_transform(:cerl.c_module(), list()) :: :cerl.c_module()
  def core_transform(mod, _opts) do
    defs =
      for {name, fun} <- :cerl.module_defs(mod) do
        {name, fun_update(fun)}
      end
    name = :cerl.module_name(mod)
    exports = :cerl.module_exports(mod)
    attrs = :cerl.module_attrs(mod)
    :cerl.update_c_module(mod, name, exports, attrs, defs)
  end

  defp fun_update(fun) do
    body = :cerl.fun_body(fun)
    {body, []} = :cerl_trees.mapfold(&call_update/2, &vars_update/2, [], body)
    vars = :cerl.fun_vars(fun)
    :cerl.update_c_fun(fun, vars, body)
  end

  defp call_update(node, vars) do
    if call?(node) do
      {node, vars2} = call_update(node)
      {node, vars2 ++ vars}
    else
      {node, vars}
    end
  end

  defp call_update(node) do
    case call_info(node) do
      {:__init_writable__, 1} ->
        update_init_writable(node)
      {:__private_append__, 1} ->
        update_private_append(node)
      {fun, arity} ->
        raise ArgumentError,
          "unknown call: #{inspect :erlang.make_fun(__MODULE__, fun, arity)}"
      nil ->
        raise ArgumentError,
          "expected static call, got: #{:core_pp.format(node)}"
    end
  end

  defp call?(node) do
    if :cerl.is_c_call(node) do
      mod = :cerl.call_module(node)
      :cerl.is_c_atom(mod) and :cerl.atom_val(mod) == __MODULE__
    else
      false
    end
  end

  defp call_info(node) do
    name = :cerl.call_name(node)
    if :cerl.is_c_atom(name) do
      {:cerl.atom_val(name), :cerl.call_arity(node)}
    end
  end

  defp update_init_writable(node) do
    name = :cerl.ann_c_atom(:cerl.get_ann(node), :bs_init_writable)
    {:cerl.update_c_primop(node, name, :cerl.call_args(node)), []}
  end

  defp update_private_append(node) do
    [var] = :cerl.call_args(node)
    if :cerl.is_c_var(var) do
      {var, [var]}
    else
      raise ArgumentError,
        "expected single variable in call arguments, got: #{:core_pp.format(node)}"
    end
  end

  defp vars_update(node, []) do
    {node, []}
  end

  defp vars_update(node, vars) do
    if :cerl.is_c_let(node) do
      Enum.reduce(vars, {node, []}, &add_ann_private/2)
    else
      {node, vars}
    end
  end

  defp add_ann_private(var, {node, acc}) do
    vars = :cerl.let_vars(node)
    arg = :cerl.let_arg(node)
    cond do
      vars == [var] and :cerl.is_c_binary(arg) ->
        arg = :cerl.add_ann([:single_use], arg)
        body = :cerl.let_body(node)
        {:cerl.update_c_let(node, vars, arg, body), acc}
      vars == [var] ->
        raise ArgumentError,
          "expected binary as argument in let, got: #{:core_pp.format(node)}"
      var in vars ->
        raise ArgumentError,
          "expected single variable in let, got: #{:core_pp.format(node)}"
      true ->
        {node, [var | acc]}
    end
  end
end
