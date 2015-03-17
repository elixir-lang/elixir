defmodule Logger.FormatterTest do
  use Logger.Case, async: true
  doctest Logger.Formatter

  import Logger.Formatter

  defmodule CompileMod do
    def format(_level, _msg, _ts, _md) do
      true
    end
  end

  test "compile/1 with nil" do
    assert compile(nil) ==
           ["\n", :time, " ", :metadata, "[", :level, "] ", :levelpad, :message, "\n"]
  end

  test "compile/1 with str" do
    assert compile("$level $time $date $metadata $message $node") ==
           Enum.intersperse([:level, :time, :date, :metadata, :message, :node], " ")

    assert_raise ArgumentError, "$bad is an invalid format pattern.", fn ->
      compile("$bad $good")
    end
  end

  test "compile/1 with {mod, fun}" do
    assert compile({CompileMod, :format}) == {CompileMod, :format}
  end

  test "format with {mod, fun}" do
    assert format({CompileMod, :format}, nil, nil, nil, nil) == true
  end

  test "format with format string" do
    compiled = compile("[$level] $message")
    assert format(compiled, :error, "hello", nil, []) ==
           ["[", "error", "] ", "hello"]

    compiled = compile("$node")
    assert format(compiled, :error, nil, nil, []) == [Atom.to_string(node())]

    compiled = compile("$metadata")
    assert IO.chardata_to_string(format(compiled, :error, nil, nil, [meta: :data])) ==
           "meta=data "
    assert IO.chardata_to_string(format(compiled, :error, nil, nil, [])) ==
           ""

    timestamp = {{2014, 12, 30}, {12, 6, 30, 100}}
    compiled = compile("$date $time")
    assert IO.chardata_to_string(format(compiled, :error, nil, timestamp, [])) ==
           "2014-12-30 12:06:30.100"
  end

  test "padding takes account of length of level" do
    compiled = compile("[$level] $levelpad $message")
    assert format(compiled, :error, "hello", nil, []) ==
           ["[", "error", "] ", "", " ", "hello"]

    assert format(compiled, :info, "hello", nil, []) ==
           ["[", "info", "] ", " ", " ", "hello"]

  end
end
