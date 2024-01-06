defmodule Module.Types.Descr do
  @moduledoc false
  def term(), do: :term
  def atom(_atom), do: :atom
  def dynamic(), do: :dynamic
  def integer(), do: :integer
  def float(), do: :float
  def binary(), do: :binary
  def pid(), do: :pid
  def tuple(), do: :tuple
  def empty_list(), do: :list
  def non_empty_list(), do: :list
  def map(), do: :map
end
