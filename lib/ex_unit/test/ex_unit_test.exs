Code.require_file "../test_helper.exs", __FILE__

defmodule ExUnit.NilFormatter do
  @behaviour ExUnit.Formatter

  def suite_started(_opts) do
    :ok
  end

  def suite_finished(:ok, _ms) do
    1
  end

  def case_started(:ok, _test_case) do
    :ok
  end

  def case_finished(:ok, _test_case) do
    :ok
  end

  def test_started(:ok, _test) do
    :ok
  end

  def test_finished(:ok, _test) do
    :ok
  end
end

defmodule ExUnitTest do
  use ExUnit.Case, async: false

  test "it supports many runs" do
    ExUnit.configure(formatter: ExUnit.NilFormatter)

    defmodule SampleTest do
      use ExUnit.Case, async: false

      test "true" do
        assert true
      end

      test "false" do
        assert false
      end
    end

    assert ExUnit.run == 1
  end
end
