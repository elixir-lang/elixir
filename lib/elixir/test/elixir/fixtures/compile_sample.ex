defmodule CompileSample, do: nil

# Assert that modules compiled are loaded into memory
# with the proper destination for the BEAM files.
dest  = __DIR__ |> Path.join("../../../tmp/#{CompileSample}.beam") |> Path.expand
^dest = CompileSample |> :code.which() |> List.to_string
