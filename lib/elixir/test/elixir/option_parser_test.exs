Code.require_file "test_helper.exs", __DIR__

defmodule OptionParserTest do
  use ExUnit.Case, async: true

  test "parses boolean option" do
    assert OptionParser.parse(["--docs"]) == {[docs: true], [], []}
  end

  test "parses alias boolean option as the alias key" do
    assert OptionParser.parse(["-d"], aliases: [d: :docs])
           == {[docs: true], [], []}
  end

  test "parses more than one boolean option" do
    assert OptionParser.parse(["--docs", "--compile"])
           == {[docs: true, compile: true], [], []}
  end

  test "parses more than one boolean options as the alias" do
    assert OptionParser.parse(["-d", "--compile"], aliases: [d: :docs])
           == {[docs: true, compile: true], [], []}
  end

  test "parses --key value option" do
    assert OptionParser.parse(["--source", "form_docs/"])
           == {[source: "form_docs/"], [], []}
  end

  test "parses --key=value option" do
    assert OptionParser.parse(["--source=form_docs/", "other"])
           == {[source: "form_docs/"], ["other"], []}
  end

  test "parses alias --key value option as the alias" do
    assert OptionParser.parse(["-s", "from_docs/"], aliases: [s: :source])
           == {[source: "from_docs/"], [], []}
  end

  test "parses alias --key=value option as the alias" do
    assert OptionParser.parse(["-s=from_docs/", "other"], aliases: [s: :source])
           == {[source: "from_docs/"], ["other"], []}
  end

  test "does not parse -- as an alias" do
    assert OptionParser.parse(["--s=from_docs/"], aliases: [s: :source])
           == {[s: "from_docs/"], [], []}
  end

  test "does not parse - as a switch" do
    assert OptionParser.parse(["-source=from_docs/"], aliases: [s: :source])
           == {[], [], [source: "from_docs/"]}
  end

  test "parses configured booleans" do
    assert OptionParser.parse(["--docs=false"], switches: [docs: :boolean])
           == {[docs: false], [], []}
    assert OptionParser.parse(["--docs=true"], switches: [docs: :boolean])
           == {[docs: true], [], []}
    assert OptionParser.parse(["--docs=other"], switches: [docs: :boolean])
           == {[], [], [docs: "other"]}
    assert OptionParser.parse(["--docs="], switches: [docs: :boolean])
           == {[], [], [docs: ""]}

    assert OptionParser.parse(["--docs", "foo"], switches: [docs: :boolean])
           == {[docs: true], ["foo"], []}
    assert OptionParser.parse(["--no-docs", "foo"], switches: [docs: :boolean])
           == {[docs: false], ["foo"], []}
    assert OptionParser.parse(["--no-docs=foo", "bar"], switches: [docs: :boolean])
           == {[], ["bar"], [docs: "foo"]}
    assert OptionParser.parse(["--no-docs=", "bar"], switches: [docs: :boolean])
           == {[], ["bar"], [docs: ""]}
  end

  test "does not set unparsed booleans" do
    assert OptionParser.parse(["foo"], switches: [docs: :boolean])
           == {[], ["foo"], []}
  end

  test "keeps options on configured keep" do
    args = ["--require", "foo", "--require", "bar", "baz"]
    assert OptionParser.parse(args, switches: [require: :keep])
           == {[require: "foo", require: "bar"], ["baz"], []}

    assert OptionParser.parse(["--require"], switches: [require: :keep])
           == {[], [], [require: true]}
  end

  test "parses configured strings" do
    assert OptionParser.parse(["--value", "1", "foo"], switches: [value: :string])
           == {[value: "1"], ["foo"], []}
    assert OptionParser.parse(["--value=1", "foo"], switches: [value: :string])
           == {[value: "1"], ["foo"], []}
    assert OptionParser.parse(["--value"], switches: [value: :string])
           == {[], [], [value: true]}
    assert OptionParser.parse(["--no-value"], switches: [value: :string])
           == {[], [], [value: false]}
  end

  test "parses configured integers" do
    assert OptionParser.parse(["--value", "1", "foo"], switches: [value: :integer])
           == {[value: 1], ["foo"], []}
    assert OptionParser.parse(["--value=1", "foo"], switches: [value: :integer])
           == {[value: 1], ["foo"], []}
    assert OptionParser.parse(["--value", "WAT", "foo"], switches: [value: :integer])
           == {[], ["foo"], [value: "WAT"]}
  end

  test "parses configured integers with keep" do
    args = ["--value", "1", "--value", "2", "foo"]
    assert OptionParser.parse(args, switches: [value: [:integer, :keep]])
           == {[value: 1, value: 2], ["foo"], []}

    args = ["--value=1", "foo", "--value=2", "bar"]
    assert OptionParser.parse(args, switches: [value: [:integer, :keep]])
           == {[value: 1, value: 2], ["foo", "bar"], []}
  end

  test "parses configured floats" do
    assert OptionParser.parse(["--value", "1.0", "foo"], switches: [value: :float])
           == {[value: 1.0], ["foo"], []}
    assert OptionParser.parse(["--value=1.0", "foo"], switches: [value: :float])
           == {[value: 1.0], ["foo"], []}
    assert OptionParser.parse(["--value", "WAT", "foo"], switches: [value: :float])
           == {[], ["foo"], [value: "WAT"]}
  end

  test "parses no switches as flags" do
    assert OptionParser.parse(["--no-docs", "foo"])
           == {[no_docs: true], ["foo"], []}
  end

  test "parses more than one key/value options" do
    assert OptionParser.parse(["--source", "from_docs/", "--docs", "show"])
           == {[source: "from_docs/", docs: "show"], [], []}
  end

  test "overrides options by default" do
    assert OptionParser.parse(["--require", "foo", "--require", "bar", "baz"])
           == {[require: "bar"], ["baz"], []}
  end

  test "parses mixed options" do
    args = ["--source", "from_docs/", "--compile", "-x"]
    assert OptionParser.parse(args, aliases: [x: :x])
           == {[source: "from_docs/", compile: true, x: true], [], []}
  end

  test "stops on first non option arguments" do
    args = ["--source", "from_docs/", "test/enum_test.exs", "--verbose"]
    assert OptionParser.parse_head(args)
           == {[source: "from_docs/"], ["test/enum_test.exs", "--verbose"], []}
  end

  test "stops on --" do
    options = OptionParser.parse(["--source", "from_docs/", "--", "1", "2", "3"])
    assert options == {[source: "from_docs/"], ["--", "1", "2", "3"], []}

    options = OptionParser.parse_head(["--source", "from_docs/", "--", "1", "2", "3"])
    assert options == {[source: "from_docs/"], ["--", "1", "2", "3"], []}

    options = OptionParser.parse(["--no-dash", "foo", "bar", "--", "-x"])
    assert options == {[no_dash: true], ["foo", "bar", "--", "-x"], []}
  end

  test "goes beyond the first non option arguments" do
    args = ["--source", "from_docs/", "test/enum_test.exs", "--verbose"]
    assert OptionParser.parse(args)
           == {[source: "from_docs/", verbose: true], ["test/enum_test.exs"], []}
  end

  test "collects multiple invalid options" do
    args = ["--bad", "opt", "foo", "-o", "bad", "bar"]
    assert OptionParser.parse(args, switches: [bad: :integer])
           == {[], ["foo", "bar"], [bad: "opt", o: "bad"]}
  end
end

defmodule OptionParserTest.Strict do
  use ExUnit.Case, async: true

  test "parses arguments" do
    assert OptionParser.parse(["a", "b", "c"], strict: true)
           == {:ok, [], ["a", "b", "c"]}
  end

  test "parses defined options" do
    config = [switches: [opt: :boolean], strict: true]
    assert OptionParser.parse(["a", "--opt", "b"], config)
           == {:ok, [opt: true], ["a", "b"]}

    config = [switches: [opt: :string], strict: true]
    assert OptionParser.parse(["a", "--opt", "b", "c"], config)
           == {:ok, [opt: "b"], ["a", "c"]}
  end

  test "parses aliases to defined options" do
    config = [switches: [opt: :boolean], aliases: [o: :opt], strict: true]
    assert OptionParser.parse(["a", "-o", "b"], config)
           == {:ok, [opt: true], ["a", "b"]}

    config = [switches: [opt: :string], aliases: [o: :opt], strict: true]
    assert OptionParser.parse(["a", "-o", "b", "c"], config)
           == {:ok, [opt: "b"], ["a", "c"]}
  end

  test "error on unknown option" do
    assert OptionParser.parse(["--docs"], strict: true)
           == {:error, {:unknown, :docs, nil}, {[], [], []}}
    assert OptionParser.parse(["--no-docs", "hello", "world"], strict: true)
           == {:error, {:unknown, :no_docs, nil}, {[], [], ["hello", "world"]}}
    assert OptionParser.parse(["hello", "--docs", "world"], strict: true)
           == {:error, {:unknown, :docs, nil}, {[], ["hello"], ["world"]}}
  end

  test "error on unknown alias" do
    assert OptionParser.parse(["-o"], strict: true)
           == {:error, {:unknown, :o, nil}, {[], [], []}}
    assert OptionParser.parse(["-o=hello"], strict: true)
           == {:error, {:unknown, :o, "hello"}, {[], [], []}}
    assert OptionParser.parse(["hello", "-o", "world"], strict: true)
           == {:error, {:unknown, :o, nil}, {[], ["hello"], ["world"]}}
  end

  test "error on alias to unknown option" do
    assert OptionParser.parse(["-d"], aliases: [d: :docs], strict: true)
           == {:error, {:unknown, :docs, nil}, {[], [], []}}
    assert OptionParser.parse(["hello", "-d", "world"], aliases: [d: :docs], strict: true)
           == {:error, {:unknown, :docs, nil}, {[], ["hello"], ["world"]}}
  end

  test "continues parsing" do
    config = [switches: [verbose: :boolean, opt: :string], aliases: [v: :verbose], strict: true]
    result = OptionParser.parse(["-v", "foo", "bar", "-d", "bar", "--opt", "baz", "quux"], config)
    assert result == {:error, {:unknown, :d, nil}, {[verbose: true], ["foo", "bar"], ["bar", "--opt", "baz", "quux"]}}

    {:error, _, {opts, args, rest}} = result
    cont = {[{:d, "ok"}|opts], args, rest}
    assert OptionParser.parse_cont(cont, config)
           == {:ok, [d: "ok", verbose: true, opt: "baz"], ["foo", "bar", "bar", "quux"]}
  end
end
