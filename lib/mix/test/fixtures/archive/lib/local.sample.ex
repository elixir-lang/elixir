defmodule Mix.Tasks.Local.Sample do
  use Mix.Task

  @shortdoc "A local install sample"
  @moduledoc "A local install sample"

  def run(_) do
    Mix.shell().info("sample")
  end
end
