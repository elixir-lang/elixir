defmodule A do
  IO.puts("A: warnings_as_errors: #{Code.compiler_options()[:warnings_as_errors]}")

  unused_in_lib = true
end
