defmodule Dialyzer.Cond do
  def one_boolean do
    cond do
      true -> :ok
    end
  end

  def two_boolean do
    cond do
      List.flatten([]) == [] -> :ok
      true -> :ok
    end
  end

  def one_otherwise do
    cond do
      :otherwise -> :ok
    end
  end

  def two_otherwise do
    cond do
      List.flatten([]) == [] -> :ok
      :otherwise -> :ok
    end
  end
end
