defmodule Mix.Shell do
  @moduledoc """
  Defines Mix.Shell contract.
  """

  use Behaviour

  @doc """
  Informs the given message.
  """
  defcallback info(message :: binary) :: any

  @doc """
  Warns about the given error message.
  """
  defcallback error(message :: binary) :: any

  @doc """
  Asks the user for confirmation.
  """
  defcallback yes?(message :: binary) :: any
end