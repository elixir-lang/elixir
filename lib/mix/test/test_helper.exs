Mix.start
ExUnit.start []

defmodule MixTest.Case do
  defmacro __using__(opts) do
    quote do
      use ExUnit.Case, unquote(opts)
      import MixTest.Case
    end
  end
end

defmodule Mix.Tasks.Hello do
  use Mix.Task
  @shortdoc "This is short documentation, see."
  @moduledoc """
  A test task.
  """
  def run(_) do
    "Hello, World!"
  end
end

defmodule Mix.Tasks.Invalid do
end