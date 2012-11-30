defmodule ExUnit.Formatter do
  @moduledoc """
  This module simply defines the callbacks
  expected by an ExUnit.Formatter.
  """

  use Behaviour

  @type test_case :: module
  @type test :: atom
  @type result :: { kind :: atom, reason :: term, stacktrace :: list } | nil

  defcallback suite_started() :: any
  defcallback suite_finished() :: non_neg_integer

  defcallback case_started(test_case) :: any
  defcallback case_finished(test_case) :: any

  defcallback test_started(test_case, test) :: any
  defcallback test_finished(test_case, test, result) :: any
end
