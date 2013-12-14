Code.require_file "../test_helper.exs", __DIR__

defmodule ExUnit.RunnerTest do
  use ExUnit.Case, async: true

  test "filters with matching tags are ored and non-matching tags are anded" do
    filters = [ { :os, :unix, :include },
                { :os, :win32, :include },
                { :type, :unit, :include } ]

    assert :ok = ExUnit.Runner.filter_match([], [])
    assert :ok = ExUnit.Runner.filter_match(filters, [])
    assert :ok = ExUnit.Runner.filter_match(filters, [os: :unix])
    assert :ok = ExUnit.Runner.filter_match(filters, [os: :unix, type: :unit])
    refute :ok = ExUnit.Runner.filter_match(filters, [os: :unix, type: :integration])
    assert :ok = ExUnit.Runner.filter_match(filters, [os: :win32])
    assert :ok = ExUnit.Runner.filter_match(filters, [os: :win32, type: :unit])
    refute :ok = ExUnit.Runner.filter_match(filters, [os: :win32, type: :integration])
    assert :ok = ExUnit.Runner.filter_match(filters, [os: :unix, os: :win32])
  end
end
