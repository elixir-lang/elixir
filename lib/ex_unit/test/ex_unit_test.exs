Code.require_file "../test_helper.exs", __FILE__

defmodule ExUnit.NilFormatter do
  @behaviour ExUnit.Formatter

  def suite_started(_opts) do
    :ok
  end

  def suite_finished(:ok) do
    1
  end

  def case_started(:ok, _test_case) do
    :ok
  end

  def case_finished(:ok, _test_case) do
    :ok
  end

  def test_started(:ok, _test_case, _test) do
    :ok
  end

  def test_finished(:ok, _test_case, _test, _result) do
    :ok
  end
end

defmodule ExUnitTest do
  use ExUnit.Case, async: false

  test "it runs after_spawn hooks" do
    assert Process.get(:after_spawn) == :ex_unit
  end

  test "it can read user config" do
    File.write("ex_unit.test.config","[extra_option: true]")
    assert ExUnit.user_options("nosuchfile.config") == []
    assert ExUnit.user_options("ex_unit.test.config") == [extra_option: true]
  after
    File.rm("ex_unit.test.config")
  end

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
