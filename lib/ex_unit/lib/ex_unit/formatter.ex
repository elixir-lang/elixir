defmodule ExUnit.Formatter do
  @moduledoc """
  This module simply defines the callbacks
  expected by an ExUnit.Formatter.
  """

  use Behaviour

  @type test_case :: module
  @type test :: atom
  @type result :: { kind :: atom, reason :: term, stacktrace :: list } | nil

  defcallback suite_started(),  do: any
  defcallback suite_finished(), do: non_neg_integer

  defcallback case_started(test_case),  do: any
  defcallback case_finished(test_case), do: any

  defcallback test_started(test_case, test), do: any
  defcallback test_finished(test_case, test, result), do: any
end
