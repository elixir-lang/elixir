defmodule Mix.Tasks.Iex do
  use Mix.Task
  @moduledoc false

  def run(_args) do
    raise Mix.Error, message: "mix iex is deprecated. Due to the VM constraints, " <>
      "IEx needs to be configured on boot. Use `iex -S mix` instead."
  end
end
