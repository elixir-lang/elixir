# We disable type inference across modules by setting
# infer_signatures to [] when compiling Elixir for
# deterministic reasons. Now we do one additional pass
# using the locally inferred types to infer all types
# for stdlib itself.
parent = self()
ebin = Path.expand("../ebin", __DIR__)

# Validate we are loading Elixir modules and that they are all in place
[:elixir] = Code.get_compiler_option(:infer_signatures)

[_ | _] =
  modules =
  for module <- Application.spec(:elixir, :modules),
      match?("Elixir." <> _, Atom.to_string(module)) do
    module
  end

# Do a quick sanity check that some modules are defined
true = URI in modules and Version.Requirement in modules

{time, modules_paths} =
  :timer.tc(fn ->
    {:ok, checker} = Module.ParallelChecker.start_link()

    try do
      modules
      |> Task.async_stream(
        fn module ->
          path = Path.join(ebin, "#{module}.beam")
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

          [{"ExCk", checker_chunk}] = :elixir_erl.checker_chunk(checker, [:deterministic])
          {:ok, ^module, chunks} = :beam_lib.all_chunks(binary)

          {:ok, new_binary} =
            chunks
            |> List.keyreplace(~c"ExCk", 0, {~c"ExCk", checker_chunk})
            |> :beam_lib.build_module()

          {module, path, new_binary}
        end,
        timeout: :infinity
      )
      |> Enum.map(fn {:ok, {module, path, new_binary}} ->
        # Only write to files once we are done to avoid the result
        # of one task affecting other ones
        File.write!(path, new_binary)
        {module, path}
      end)
    after
      Module.ParallelChecker.stop(checker)
    end
  end)

IO.puts(:stderr, ["Type inferred stdlib in ", Integer.to_string(div(time, 1000)), "ms"])

{time, _} =
  :timer.tc(fn ->
    # We start a new one so it uses the new cache
    {:ok, checker} = Module.ParallelChecker.start_link()
    Module.ParallelChecker.verify(checker, modules_paths)
  end)

IO.puts(:stderr, ["Type checked stdlib in ", Integer.to_string(div(time, 1000)), "ms"])
