defmodule Mix.Task do
  @moduledoc """
  A simple module that provides conveniences for creating tasks.
  """

  @doc """
  Define the required callbacks for Mix.Task behavior.
  """
  def behaviour_info(:callbacks) do
    [run: 1]
  end

  defmacro __using__(_opts) do
    quote do
      @behavior Mix.Task
    end
  end
end