Module.create(Dynamic, quote(do: :ok), file: "dynamic.ex")
[_ | _] = :code.which(Dynamic)
