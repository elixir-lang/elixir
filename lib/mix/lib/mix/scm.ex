defmodule Mix.SCM do
  def behaviour_info(:callbacks) do
    [key: 0, available?: 1, get: 2]
  end

  def available do
    [:git]
  end

  def to_module(:git), do: Mix.SCM.Git
end