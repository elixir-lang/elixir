defmodule Mix.Local.Target.Escript do
  @moduledoc "See Mix.Local.Target"
  
  @behaviour Mix.Local.Target

  def name_for(project) do
    case get_in(project, [:escript, :name]) do
      nil  -> project[:app]
      name -> name
    end
    |> to_string()
  end

  def path_for() do
    Path.join(Mix.Utils.mix_home, "escripts")
  end
  
  def printable_name() do
    { "escript", "escripts" }
  end

  def task_name(), do: "escript"
  
end
