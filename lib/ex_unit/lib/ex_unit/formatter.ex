defmodule ExUnit.Formatter do
  @moduledoc """
  This module simply defines the callbacks
  expected by an ExUnit.Formatter.
  """

  use Behaviour

  @type id :: term
  @type test_case :: module
  @type test :: atom
  @type result :: { kind :: atom, reason :: term, stacktrace :: list } | nil

  defcallback suite_started(opts :: list) :: id
  defcallback suite_finished(id) :: non_neg_integer

  defcallback case_started(id, test_case) :: any
  defcallback case_finished(id, test_case) :: any

  defcallback test_started(id, test_case, test) :: any
  defcallback test_finished(id, test_case, test, result) :: any
end
