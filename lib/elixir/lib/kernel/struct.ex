defmodule Kernel.Struct do
  @moduledoc false

  @doc false
  def expand(struct_or_match, {:%{}, map_meta, map_args}, %{context: context} = env) do
    meta = [file: env.file, line: env.line]
    map_args = clean_struct_key_from_map_args(meta, map_args, env)

    {[struct_or_match, map], env} =
      :elixir_expand.expand_args([struct_or_match, {:%{}, map_meta, map_args}], env)

    case validate_struct(struct_or_match, context) do
      true when is_atom(struct_or_match) ->
        # We always record structs when they are expanded
        # as they expect the reference at compile time.
        :elixir_lexical.record_struct(struct_or_match, env.line, env.lexical_tracker)

        case extract_struct_assocs(meta, map, env) do
          # Expand
          {:expand, map_meta, assocs} when context != :match ->
            struct = load_struct(meta, struct_or_match, [assocs], env)
            assert_struct_keys(meta, struct_or_match, struct, assocs, env)
            keys = [:__struct__] ++ for {k, _} <- assocs, do: k
            without_keys = :maps.to_list(:maps.without(keys, struct))
            struct_assocs = :elixir_quote.escape(without_keys, :default, false)
            expand_to_map(meta, struct_or_match, {:%{}, map_meta, struct_assocs ++ assocs})

          # Update or match
          {_, _, assocs} ->
            struct = load_struct(meta, struct_or_match, [], env)
            assert_struct_keys(meta, struct_or_match, struct, assocs, env)
            expand_to_map(meta, struct_or_match, map)
        end

      true ->
        expand_to_map(meta, struct_or_match, map)

      false when context == :match ->
        :elixir_errors.form_error(
          meta,
          env.file,
          env.module,
          {:invalid_struct_name_in_match, map}
        )

      false ->
        :elixir_errors.form_error(
          meta,
          env.file,
          env.module,
          {:invalid_struct_name, struct_or_match}
        )
    end
  end

  defp clean_struct_key_from_map_args(meta, [{:|, pipe_meta, [left, map_assocs]}], env) do
    [{:|, pipe_meta, [left, clean_struct_key_from_map_args(meta, map_assocs, env)]}]
  end

  defp clean_struct_key_from_map_args(meta, map_assocs, env) do
    clean_struct_key_from_map_assocs(meta, map_assocs, env)
  end

  defp clean_struct_key_from_map_assocs(meta, assocs, env) do
    case :lists.keytake(:__struct__, 1, assocs) do
      {:value, _, clean_assocs} ->
        :elixir_errors.form_warn(meta, env.file, env.module, :ignored_struct_key_in_struct)
        clean_assocs

      false ->
        assocs
    end
  end

  defp validate_struct({:^, _, [{var, _, ctx}]}, :match) when is_atom(var) and is_atom(ctx),
    do: true

  defp validate_struct({var, _meta, ctx}, :match) when is_atom(var) and is_atom(ctx), do: true
  defp validate_struct(atom, _) when is_atom(atom), do: true
  defp validate_struct(_, _), do: false

  defp extract_struct_assocs(_, {:%{}, meta, [{:|, _, [_, assocs]}]}, _) do
    {:update, meta, delete_struct_key(assocs)}
  end

  defp extract_struct_assocs(_, {:%{}, meta, assocs}, _) do
    {:expand, meta, delete_struct_key(assocs)}
  end

  defp extract_struct_assocs(meta, other, env) do
    :elixir_errors.form_error(meta, env.file, env.module, {:non_map_after_struct, other})
  end

  defp delete_struct_key(assocs) do
    :lists.keydelete(:__struct__, 1, assocs)
  end

  defp load_struct(meta, name, args, env) do
    # We also include the current module because it won't be present
    # in context module in case the module name is defined dynamically.
    in_context = :lists.member(name, [env.module | env.context_modules])

    arity = length(args)
    local = in_context || (!ensure_loaded(name) && wait_for_struct(name))

    try do
      case local && :elixir_def.local_for(name, :__struct__, arity, [:def]) do
        false ->
          apply(name, :__struct__, args)

        local_fun ->
          # There is an inherent race condition when using local_for.
          # By the time we got to execute the function, the ETS table
          # with temporary definitions for the given module may no longer
          # be available, so any function invocation happening inside the
          # local function will fail. In this case, we need to fall back to
          # the regular dispatching since the module will be available if
          # the table has not been deleted (unless compilation of that
          # module failed which should then cause this call to fail too).
          try do
            apply(local_fun, args)
          catch
            :error, :undef -> apply(name, :__struct__, args)
          end
      end
      |> case do
        %{} = struct ->
          struct

        other ->
          :elixir_errors.form_error(
            meta,
            env.file,
            env.module,
            {:invalid_struct_return_value, name, arity, other}
          )
      end
    catch
      :error, :undef ->
        case in_context && env.function == nil do
          true ->
            :elixir_errors.form_error(meta, env.file, env.module, {:inaccessible_struct, name})

          false ->
            :elixir_errors.form_error(
              meta,
              env.file,
              env.module,
              {:undefined_struct, name, arity}
            )
        end

      kind, reason ->
        info = [
          {name, :__struct__, arity, [{:file, 'expanding struct'}]},
          :elixir_utils.caller(env.line, env.file, env.module, env.function)
        ]

        :erlang.raise(kind, reason, info)
    end
  end

  defp ensure_loaded(module), do: :code.ensure_loaded(module) == {:module, module}

  defp wait_for_struct(module) do
    is_pid(:erlang.get(:elixir_compiler_pid)) &&
      Kernel.ErrorHandler.ensure_compiled(module, :struct)
  end

  defp assert_struct_keys(meta, name, struct, assocs, env) do
    for {key, _} <- assocs, !:maps.is_key(key, struct) do
      :elixir_errors.form_error(meta, env.file, env.module, {:unknown_key_for_struct, name, key})
    end
  end

  defp expand_to_map(meta, struct, {:%{}, _, assocs}) do
    {:%{}, put_struct_meta(meta, struct), put_struct_key(assocs, struct)}
  end

  defp put_struct_meta(meta, struct) when is_atom(struct) do
    [{:struct, struct} | meta]
  end

  defp put_struct_meta(meta, _), do: meta

  defp put_struct_key([{:|, pipe_meta, [left, assocs]}], struct) do
    [{:|, pipe_meta, [left, put_struct_key(assocs, struct)]}]
  end

  defp put_struct_key(assocs, struct) do
    [{:__struct__, struct} | assocs]
  end
end
