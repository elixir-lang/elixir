import Kernel, except: [destructure: 2, defdelegate: 2, defstruct: 2]

defmodule Kernel.Utils do
  @moduledoc false

  @doc """
  Callback for destructure.
  """
  def destructure(list, count) when is_list(list) and is_integer(count) and count >= 0,
    do: destructure_list(list, count)
  def destructure(nil, count) when is_integer(count) and count >= 0,
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
    # TODO: Remove by 2.0
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
    raise ArgumentError, "defdelegate/2 only accepts function parameters, got: #{Macro.to_string(ast)}"
  end

  @doc """
  Callback for defstruct.
  """
  def defstruct(module, fields) do
    case fields do
      fs when is_list(fs) ->
        :ok
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

    {:maps.put(:__struct__, module, :maps.from_list(fields)),
     List.wrap(Module.get_attribute(module, :enforce_keys)),
     Module.get_attribute(module, :derive)}
  end

  @doc """
  Announcing callback for defstruct.
  """
  def announce_struct(module) do
    case :erlang.get(:elixir_compiler_pid) do
      :undefined -> :ok
      pid -> send(pid, {:struct_available, module})
    end
  end

  @doc """
  Callback for raise.
  """
  def raise(msg) when is_binary(msg) do
    RuntimeError.exception(msg)
  end
  def raise(atom) when is_atom(atom) do
    atom.exception([])
  end
  def raise(%{__struct__: struct, __exception__: true} = exception) when is_atom(struct) do
    exception
  end
  def raise(other) do
    ArgumentError.exception("raise/1 expects an alias, string or exception as " <>
                            "the first argument, got: #{inspect other}")
  end
end
