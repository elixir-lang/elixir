defmodule Mix.Tasks.Iex do
  use Mix.Task

  @hidden true
  @moduledoc false

  def run(_) do
    raise Mix.Error, message: "Cannot start IEx after the VM was booted. " <>
                              "To use IEx with Mix, please run: iex -S mix"
  end
end
