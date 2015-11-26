import Kernel, except: [destructure: 2, defdelegate: 2, defstruct: 2]

defmodule Kernel.Utils do
  @moduledoc false

  def destructure(list, count) when is_list(list), do: destructure_list(list, count)
  def destructure(nil, count), do: destructure_nil(count)

  defp destructure_list(_, 0), do: []
  defp destructure_list([], count), do: destructure_nil(count)
  defp destructure_list([h|t], count), do: [h|destructure_list(t, count - 1)]

  defp destructure_nil(0), do: []
  defp destructure_nil(count), do: [nil|destructure_nil(count - 1)]

  def defdelegate(fun, opts, env) do
    append_first = Keyword.get(opts, :append_first, false)

    {name, args} =
      case Macro.decompose_call(fun) do
        {_, _} = pair -> pair
        _ -> raise ArgumentError, "invalid syntax in defdelegate #{Macro.to_string(fun)}"
      end

    :ok = check_defdelegate_args(args, env)

    as_args =
      case append_first and args != [] do
        true  -> tl(args) ++ [hd(args)]
        false -> args
      end

    as = Keyword.get(opts, :as, name)
    {name, args, as, as_args}
  end

  # TODO: Convert this to an error on 1.3
  defp check_defdelegate_args([], _env),
    do: :ok
  defp check_defdelegate_args([{var, _, mod}|rest], env) when is_atom(var) and is_atom(mod),
    do: check_defdelegate_args(rest, env)
  defp check_defdelegate_args([code|_], env) do
    IO.write :stderr, "warning: defdelegate/2 will only accept variable names in upcoming versions, " <>
                      "got: #{Macro.to_string(code)}\n" <> Exception.format_stacktrace(Macro.Env.stacktrace(env))
  end

  def defstruct(module, fields) do
    case fields do
      fs when is_list(fs) -> :ok
      other ->
        raise ArgumentError, "struct fields definition must be list, got: #{inspect other}"
    end

    fields = :lists.map(fn
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
        raise ArgumentError, "struct field names must be atoms, got: #{inspect other}"
    end, fields)

    :maps.put(:__struct__, module, :maps.from_list(fields))
  end
end
