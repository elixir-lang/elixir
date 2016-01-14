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

  def defdelegate(fun, opts) do
    append_first = Keyword.get(opts, :append_first, false)

    {name, args} =
      case Macro.decompose_call(fun) do
        {_, _} = pair -> pair
        _ -> raise ArgumentError, "invalid syntax in defdelegate #{Macro.to_string(fun)}"
      end

    as_args_list = normalize_args(args)
    as = Keyword.get(opts, :as, name)
    :lists.map(fn as_args ->
      formal_args = make_formal_args(as_args)
      as_args = case append_first do
        true  -> tl(as_args) ++ [hd(as_args)]
        false -> as_args
      end
      {name, formal_args, as, as_args}
    end, as_args_list)
  end

  defp make_formal_args(args) do
    fun = &match?({name, _, mod} when is_atom(name) and is_atom(mod), &1)
    :lists.filter(fun, args)
  end

  defp normalize_args(raw_args) do
    :lists.foldr(fn
      ({:\\, _, [arg, default_arg]}, [as_args|_] = as_args_list) ->
        new_as_args = [default_arg|as_args]
        [new_as_args|add_arg(as_args_list, arg)]
      (arg, as_args_list) ->
        add_arg(as_args_list, arg)
    end, [[]], raw_args)
  end

  defp add_arg(as_args_list, {name, _, mod} = arg) when is_atom(name) and is_atom(mod),
    do: :lists.map(&([arg|&1]), as_args_list)
  defp add_arg(_, code) do
    raise ArgumentError,
      "defdelegate/2 only accepts function parameters, got: #{Macro.to_string(code)}"
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
