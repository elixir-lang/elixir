defmodule Behaviour do
  @moduledoc """
  This module has been deprecated.

  Instead of `defcallback`, one can simply use `@callback`.
  Instead of `defmacrocallback`, one can simply use `@macrocallback`.
  Instead of `__behaviour__(:callbacks)`, one can simply use `behaviour_info(:callbacks)`.
  """

  # TODO: Deprecate by 1.4
  # TODO: Remove by 2.0

  @doc """
  Defines a function callback according to the given type specification.
  """
  defmacro defcallback(spec) do
    do_defcallback(:def, split_spec(spec, quote(do: term)))
  end

  @doc """
  Defines a macro callback according to the given type specification.
  """
  defmacro defmacrocallback(spec) do
    do_defcallback(:defmacro, split_spec(spec, quote(do: Macro.t)))
  end

  defp split_spec({:when, _, [{:::, _, [spec, return]}, guard]}, _default) do
    {spec, return, guard}
  end

  defp split_spec({:when, _, [spec, guard]}, default) do
    {spec, default, guard}
  end

  defp split_spec({:::, _, [spec, return]}, _default) do
    {spec, return, []}
  end

  defp split_spec(spec, default) do
    {spec, default, []}
  end

  defp do_defcallback(kind, {spec, return, guards}) do
    case Macro.decompose_call(spec) do
      {name, args} ->
        do_callback(kind, name, args, return, guards)
      _ ->
        raise ArgumentError, "invalid syntax in #{kind}callback #{Macro.to_string(spec)}"
    end
  end

  defp do_callback(kind, name, args, return, guards) do
    :lists.foreach fn
      {:::, _, [left, right]} ->
        ensure_not_default(left)
        ensure_not_default(right)
        left
      other ->
        ensure_not_default(other)
        other
    end, args

    spec =
      quote do
        unquote(name)(unquote_splicing(args)) :: unquote(return) when unquote(guards)
      end

    case kind do
      :def -> quote(do: @callback unquote(spec))
      :defmacro -> quote(do: @macrocallback unquote(spec))
    end
  end

  defp ensure_not_default({:\\, _, [_, _]}) do
    raise ArgumentError, "default arguments \\\\ not supported in defcallback/defmacrocallback"
  end

  defp ensure_not_default(_), do: :ok

  @doc false
  defmacro __using__(_) do
    quote do
      @doc false
      def __behaviour__(:callbacks) do
        __MODULE__.behaviour_info(:callbacks)
      end

      def __behaviour__(:docs) do
        for {tuple, line, kind, docs} <- Code.get_docs(__MODULE__, :callback_docs) do
          case kind do
            :callback -> {tuple, line, :def, docs}
            :macrocallback -> {tuple, line, :defmacro, docs}
          end
        end
      end

      import unquote(__MODULE__)
    end
  end
end
