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

  test "truncate/2" do
    # ASCII binaries
    assert truncate("foo", 4) == "foo"
    assert truncate("foo", 3) == "foo"
    assert truncate("foo", 2) == ["fo", " (truncated)"]

    # UTF-8 binaries
    assert truncate("olá", 2) == ["ol", " (truncated)"]
    assert truncate("olá", 3) == ["ol", " (truncated)"]
    assert truncate("olá", 4) == "olá"
    assert truncate("ááááá:", 10) == ["ááááá", " (truncated)"]
    assert truncate("áááááá:", 10) == ["ááááá", " (truncated)"]
    assert truncate("𠜎𠜱𠝹𠱓", 15) == ["𠜎𠜱𠝹", " (truncated)"]

    # Charlists
    assert truncate(~c"olá", 2) == [~c"olá", " (truncated)"]
    assert truncate(~c"olá", 3) == [~c"olá", " (truncated)"]
    assert truncate(~c"olá", 4) == ~c"olá"

    # Chardata
    assert truncate(~c"ol" ++ "á", 2) == [~c"ol" ++ "", " (truncated)"]
    assert truncate(~c"ol" ++ "á", 3) == [~c"ol" ++ "", " (truncated)"]
    assert truncate(~c"ol" ++ "á", 4) == ~c"ol" ++ "á"

    # :infinity
    long_string = String.duplicate("foo", 10000)
    assert truncate(long_string, :infinity) == long_string
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

  describe "log" do
    test "handles :module and :function" do
      {_, formatter} =
        new(
          format: "\n$time $metadata[$level] $message\n",
          metadata: [:module, :function, :mfa],
          colors: [enabled: false]
        )

      assert %{
               level: :warn,
               msg: {:string, "foo"},
               meta: %{
                 mfa: {Logger.Formatter, :compile, 1}
               }
             }
             |> format(formatter)
             |> IO.chardata_to_string() =~
               "module=Logger.Formatter function=compile/1 mfa=Logger.Formatter.compile/1"
    end

    test "handles invalid :time in metadata" do
      {_, formatter} =
        new(
          format: "\n$time $message\n",
          colors: [enabled: false]
        )

      assert %{
               level: :warn,
               msg: {:string, "message"},
               meta: %{
                 mfa: {Logger.Formatter, :compile, 1},
                 time: "invalid"
               }
             }
             |> format(formatter)
             |> IO.chardata_to_string() =~ ~r"\d\d\d message"
    end
  end

  describe "compile + format" do
    defmodule CompileMod do
      def format(_level, _msg, _ts, _md) do
        true
      end
    end

    test "compile with nil" do
      assert compile(nil) ==
               ["\n", :time, " ", :metadata, "[", :level, "] ", :message, "\n"]
    end

    test "compile with string" do
      assert compile("$level $time $date $metadata $message $node") ==
               Enum.intersperse([:level, :time, :date, :metadata, :message, :node], " ")

      assert_raise ArgumentError, "$bad is an invalid format pattern", fn ->
        compile("$bad $good")
      end
    end

    test "compile with {mod, fun}" do
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

      pid = :erlang.list_to_pid(~c"<0.123.0>")
      format = format(compiled, :error, nil, nil, meta: :data, pid: pid)
      assert IO.chardata_to_string(format) == "meta=data pid=<0.123.0> "

      # Hack to get the same predictable reference for every test run.
      ref =
        :erlang.binary_to_term(
          <<131, 114, 0, 3, 100, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111>> <>
            <<104, 111, 115, 116, 0, 0, 0, 0, 80, 0, 0, 0, 0, 0, 0, 0, 0>>
        )

      # Ensure the deserialization worked correctly
      assert "#Reference<0.0.0.80>" == inspect(ref)

      assert IO.chardata_to_string(format(compiled, :error, nil, nil, meta: :data, ref: ref)) ==
               "meta=data ref=<0.0.0.80> "

      port = :erlang.list_to_port(~c"#Port<0.1234>")
      format = format(compiled, :error, nil, nil, meta: :data, port: port)
      assert IO.chardata_to_string(format) == "meta=data port=<0.1234> "

      # Also works with to_string
      format = format(compiled, :error, nil, nil, date: ~D[2020-10-01])
      assert IO.chardata_to_string(format) == "date=2020-10-01 "

      # And with no metadata
      assert IO.chardata_to_string(format(compiled, :error, nil, nil, [])) == ""

      timestamp = {{2014, 12, 30}, {12, 6, 30, 100}}
      compiled = compile("$date $time")

      assert IO.chardata_to_string(format(compiled, :error, nil, timestamp, [])) ==
               "2014-12-30 12:06:30.100"
    end

    test "format discards unknown formats" do
      compiled = compile("$metadata $message")
      metadata = [ancestors: [self()], crash_reason: {:some, :tuple}, foo: :bar]

      assert format(compiled, :error, "hello", nil, metadata) ==
               [["foo", 61, "bar", 32], " ", "hello"]
    end
  end
end
