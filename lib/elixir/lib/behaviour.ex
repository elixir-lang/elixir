defmodule Behaviour do
  @moduledoc """
  Mechanism for handling behaviours.

  This module is deprecated. Instead of `defcallback/1` and
  `defmacrocallback/1`, the `@callback` and `@macrocallback`
  module attributes can be used (respectively). See the
  documentation for `Module` for more information on these
  attributes.

  Instead of `MyModule.__behaviour__(:callbacks)`,
  `MyModule.behaviour_info(:callbacks)` can be used.
  """

  @moduledoc deprecated: "Use @callback and @macrocallback attributes instead"

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
    do_defcallback(:defmacro, split_spec(spec, quote(do: Macro.t())))
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
    fun = fn
      {:::, _, [left, right]} ->
        ensure_not_default(left)
        ensure_not_default(right)
        left

      other ->
        ensure_not_default(other)
        other
    end

    Enum.each(args, fun)

    spec =
      quote do
        unquote(name)(unquote_splicing(args)) :: unquote(return) when unquote(guards)
      end

    case kind do
      :def -> quote(do: @callback(unquote(spec)))
      :defmacro -> quote(do: @macrocallback(unquote(spec)))
    end
  end

  defp ensure_not_default({:\\, _, [_, _]}) do
    raise ArgumentError, "default arguments \\\\ not supported in defcallback/defmacrocallback"
  end

  defp ensure_not_default(_), do: :ok

  @doc false
  defmacro __using__(_) do
    quote do
      warning =
        "the Behaviour module is deprecated. Instead of using this module, " <>
          "use the @callback and @macrocallback module attributes. See the " <>
          "documentation for Module for more information on these attributes"

      IO.warn(warning)

      @doc false
      def __behaviour__(:callbacks) do
        __MODULE__.behaviour_info(:callbacks)
      end

      def __behaviour__(:docs) do
        {:docs_v1, _, :elixir, _, _, _, docs} = Code.fetch_docs(__MODULE__)

        for {{kind, name, arity}, line, _, doc, _} <- docs, kind in [:callback, :macrocallback] do
          case kind do
            :callback -> {{name, arity}, line, :def, __behaviour__doc_value(doc)}
            :macrocallback -> {{name, arity}, line, :defmacro, __behaviour__doc_value(doc)}
          end
        end
      end

      defp __behaviour__doc_value(:none), do: nil
      defp __behaviour__doc_value(:hidden), do: false
      defp __behaviour__doc_value(%{"en" => doc}), do: doc

      import unquote(__MODULE__)
    end
  end
end
