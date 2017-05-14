defmodule Mix.Local.Target.Archive do
  @moduledoc "See Mix.Local.Target"
  
  @behaviour Mix.Local.Target

  def name_for(project) do
    version = if (v = project[:version]), do: "-#{v}"
    "#{project[:app]}#{version}.ez"
  end

  def path_for() do
    System.get_env("MIX_ARCHIVES") || Path.join(Mix.Utils.mix_home, "archives")
  end
  
  def printable_name() do
    { "archive", "archives" }
  end

  def task_name(), do: "archive"
end
