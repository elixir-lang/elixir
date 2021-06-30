defmodule CoverThresholdOffTest do
  @moduledoc """
  Documentation for `CoverThresholdOffTest`.
  """

  @doc """
  Hello world.

  ## Examples

      iex> CoverThresholdOffTest.hello()
      :world

  """
  def hello do
    :world
  end

  def goodbye(arg) do
    case arg do
      :hello -> :goodbye
      _ -> :eh?
    end
  end
end
