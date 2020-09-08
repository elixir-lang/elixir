defmodule Module.Types.Remote do
  @moduledoc false

  alias Module.ParallelChecker

  def check(module, fun, arity, meta, context) when is_atom(module) do
    # TODO: In the future we may want to warn for modules defined
    # in the local context
    if Keyword.get(meta, :context_module, false) and context.module != module do
      context
    else
      ParallelChecker.preload_module(context.cache, module)
      check_export(module, fun, arity, meta, context)
    end
  end

  def check(_module, _fun, _arity, _meta, context), do: context

  defp check_export(module, fun, arity, meta, context) do
    case ParallelChecker.fetch_export(context.cache, module, fun, arity) do
      {:ok, :def, reason} ->
        check_deprecated(module, fun, arity, reason, meta, context)

      {:ok, :defmacro, reason} ->
        context = warn(meta, context, {:unrequired_module, module, fun, arity})
        check_deprecated(module, fun, arity, reason, meta, context)

      {:error, :module} ->
        if warn_undefined?(module, fun, arity, context) do
          warn(meta, context, {:undefined_module, module, fun, arity})
        else
          context
        end

      {:error, :function} ->
        if warn_undefined?(module, fun, arity, context) do
          exports = ParallelChecker.all_exports(context.cache, module)
          warn(meta, context, {:undefined_function, module, fun, arity, exports})
        else
          context
        end
    end
  end

  defp check_deprecated(module, fun, arity, reason, meta, context) do
    if reason do
      warn(meta, context, {:deprecated, module, fun, arity, reason})
    else
      context
    end
  end

  # TODO: Properly handle protocols
  defp warn_undefined?(_module, :__impl__, 1, _context), do: false
  defp warn_undefined?(_module, :module_info, 0, _context), do: false
  defp warn_undefined?(_module, :module_info, 1, _context), do: false
  defp warn_undefined?(:erlang, :orelse, 2, _context), do: false
  defp warn_undefined?(:erlang, :andalso, 2, _context), do: false

  defp warn_undefined?(_, _, _, %{no_warn_undefined: :all}) do
    false
  end

  defp warn_undefined?(module, fun, arity, context) do
    not Enum.any?(context.no_warn_undefined, &(&1 == module or &1 == {module, fun, arity}))
  end

  defp warn(meta, context, warning) do
    {fun, arity} = context.function
    location = {context.file, meta[:line] || 0, {context.module, fun, arity}}
    %{context | warnings: [{__MODULE__, warning, location} | context.warnings]}
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
