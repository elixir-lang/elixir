defmodule Logger.FormatterTest do
  use Logger.Case, async: true

  doctest Logger.Formatter
  import Logger.Formatter

  test "prune/1" do
    assert prune(1) == "�"
    assert prune(<<"hí", 233>>) == "hí�"
    assert prune(["hi" | 233]) == ["hi" | "�"]
    assert prune([233 | "hi"]) == [233 | "hi"]
    assert prune([[] | []]) == [[]]
  end

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

    assert_raise ArgumentError, "$bad is an invalid format pattern", fn ->
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
    assert format(compiled, :error, "hello", nil, []) == ["[", "error", "] ", "hello"]

    compiled = compile("$node")
    assert format(compiled, :error, nil, nil, []) == [Atom.to_string(node())]

    compiled = compile("$metadata")
    format = format(compiled, :error, nil, nil, meta: :data)
    assert IO.chardata_to_string(format) == "meta=data "

    pid = :erlang.list_to_pid('<0.123.4>')
    format = format(compiled, :error, nil, nil, meta: :data, pid: pid)
    assert IO.chardata_to_string(format) == "meta=data pid=<0.123.4> "

    # Hack to get the same predictable reference for every test run.
    ref =
      :erlang.binary_to_term(
        <<131, 114, 0, 3, 100, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111>> <>
          <<104, 111, 115, 116, 0, 0, 0, 0, 80, 0, 0, 0, 0, 0, 0, 0, 0>>
      )

    # ensure the deserialization worked correctly
    assert "#Reference<0.0.0.80>" == inspect(ref)

    assert IO.chardata_to_string(format(compiled, :error, nil, nil, meta: :data, ref: ref)) ==
             "meta=data ref=<0.0.0.80> "

    assert IO.chardata_to_string(format(compiled, :error, nil, nil, [])) == ""

    timestamp = {{2014, 12, 30}, {12, 6, 30, 100}}
    compiled = compile("$date $time")

    assert IO.chardata_to_string(format(compiled, :error, nil, timestamp, [])) ==
             "2014-12-30 12:06:30.100"
  end

  test "format discards callers/ancestors/crash_reason" do
    compiled = compile("$metadata $message")
    metadata = [callers: :discard, ancestors: :discard, crash_reason: :discard, foo: :bar]

    assert format(compiled, :error, "hello", nil, metadata) ==
             [["foo", 61, "bar", 32], " ", "hello"]
  end

  test "padding takes account of length of level" do
    compiled = compile("[$level] $levelpad $message")
    assert format(compiled, :error, "hello", nil, []) == ["[", "error", "] ", "", " ", "hello"]
    assert format(compiled, :info, "hello", nil, []) == ["[", "info", "] ", " ", " ", "hello"]
  end

  test "format_date/1" do
    date = {2015, 1, 30}
    assert format_date(date) == ["2015", ?-, [?0, "1"], ?-, "30"]
  end

  test "format_time/1" do
    time = {12, 30, 10, 1}
    assert format_time(time) == ["12", ?:, "30", ?:, "10", ?., [?0, ?0, "1"]]

    time = {12, 30, 10, 10}
    assert format_time(time) == ["12", ?:, "30", ?:, "10", ?., [?0, "10"]]
  end
end
