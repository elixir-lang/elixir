defmodule CoverThresholdBelowTest do
  @moduledoc """
  Documentation for `CoverThresholdTest`.
  """

  @doc """
  Hello world.

  ## Examples

      iex> CoverThresholdBelowTest.hello()
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
