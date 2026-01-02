parent = self()
{:ok, checker} = Module.ParallelChecker.start_link()

# Validate we are loading Elixir modules and that they are all in place
[:elixir] = Code.get_compiler_option(:infer_signatures)
true = URI in Application.spec(:elixir, :modules)

{time, modules} =
  :timer.tc(fn ->
    [_ | _] = paths = Path.wildcard(Path.join(__DIR__, "../ebin/Elixir.*.beam"))

    paths
    |> Task.async_stream(
      fn path ->
        path = Path.expand(path)
        Module.ParallelChecker.put(parent, checker)
        cache = Module.ParallelChecker.get()
        binary = File.read!(path)

        {:ok, {_, [{:debug_info, debug_info}, {_, checker_blob}]}} =
          :beam_lib.chunks(binary, [:debug_info, ~c"ExCk"])

        {:debug_info_v1, _backend, {:elixir_v1, module_map, _specs}} = debug_info

        %{module: module, file: file, attributes: attributes, definitions: definitions} =
          module_map

        {_, checker} = :erlang.binary_to_term(checker_blob)
        env = :elixir_env.new()

        # We assume that all private functions have been invoked at this point
        private =
          for {fun_arity, kind, _, _} <- definitions, kind in [:defp, :defmacrop], do: fun_arity

        {signatures, _} =
          Module.Types.infer(module, file, attributes, definitions, private, env, cache)

        checker =
          update_in(checker.exports, fn exports ->
            for {fun, info} <- exports do
              {fun, %{info | sig: Map.get(signatures, fun, info.sig)}}
            end
          end)

        [{"ExCk", checker_chunk}] = :elixir_erl.checker_chunk(checker, [])
        {:ok, ^module, chunks} = :beam_lib.all_chunks(binary)

        {:ok, new_binary} =
          chunks
          |> List.keyreplace(~c"ExCk", 0, {~c"ExCk", checker_chunk})
          |> :beam_lib.build_module()

        File.write!(path, new_binary)
        {module, path}
      end,
      timeout: :infinity
    )
    |> Enum.map(fn {:ok, result} -> result end)
  end)

IO.puts(:stderr, ["Type inferred stdlib in ", Integer.to_string(div(time, 1000)), "ms"])

{time, _} =
  :timer.tc(fn ->
    Module.ParallelChecker.verify(checker, modules)
  end)

IO.puts(:stderr, ["Type checked stdlib in ", Integer.to_string(div(time, 1000)), "ms"])
