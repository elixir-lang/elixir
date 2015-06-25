defmodule Kernel.Def do
  @moduledoc false

  @doc """
  Callback invoked at compile time for `defdelegate`.
  """
  def delegate(fun, opts) do
    append_first = Keyword.get(opts, :append_first, false)

    {name, args} =
      case Macro.decompose_call(fun) do
        {_, _} = pair -> pair
        _ -> raise ArgumentError, "invalid syntax in defdelegate #{Macro.to_string(fun)}"
      end

    as_args =
      case append_first and args != [] do
        true  -> tl(args) ++ [hd(args)]
        false -> args
      end

    as = Keyword.get(opts, :as, name)
    {name, args, as, as_args}
  end

  @doc """
  Callback invoked at compile time for `defstruct`.
  """
  def struct(module, fields) do
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
