defmodule ExUnit.Formatter do
  @moduledoc """
  This module simply defines the callbacks
  expected by an ExUnit.Formatter.
  """

  use Behaviour

  @type id :: term
  @type test_case :: ExUnit.TestCase.t
  @type test :: ExUnit.Test.t
  @type result :: { kind :: atom, reason :: term, stacktrace :: list } | nil
  @type run_us :: pos_integer
  @type load_us :: pos_integer | nil

  defcallback suite_started(opts :: list) :: id
  defcallback suite_finished(id, run_us, load_us) :: non_neg_integer

  defcallback case_started(id, test_case) :: any
  defcallback case_finished(id, test_case) :: any

  defcallback test_started(id, test) :: any
  defcallback test_finished(id, test) :: any
end
