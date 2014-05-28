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

  test "does not interpret undefined options with value as boolean" do
    assert OptionParser.parse(["--no-bool"])
           == {[no_bool: true], [], []}
    assert OptionParser.parse(["--no-bool"], strict: [])
           == {[], [], [{"--no-bool", nil}]}
    assert OptionParser.parse(["--no-bool=...", "other"])
           == {[], ["other"], [{"--no-bool", "..."}]}
  end

  test "does not parse -- as an alias" do
    assert OptionParser.parse(["--s=from_docs/"], aliases: [s: :source])
           == {[s: "from_docs/"], [], []}
  end

  test "does not parse - as a switch" do
    assert OptionParser.parse(["-source=from_docs/"], aliases: [s: :source])
           == {[], [], [{"-source", "from_docs/"}]}
  end

  test "parses configured booleans" do
    assert OptionParser.parse(["--docs=false"], switches: [docs: :boolean])
           == {[docs: false], [], []}
    assert OptionParser.parse(["--docs=true"], switches: [docs: :boolean])
           == {[docs: true], [], []}
    assert OptionParser.parse(["--docs=other"], switches: [docs: :boolean])
           == {[], [], [{"--docs", "other"}]}
    assert OptionParser.parse(["--docs="], switches: [docs: :boolean])
           == {[], [], [{"--docs", ""}]}

    assert OptionParser.parse(["--docs", "foo"], switches: [docs: :boolean])
           == {[docs: true], ["foo"], []}
    assert OptionParser.parse(["--no-docs", "foo"], switches: [docs: :boolean])
           == {[docs: false], ["foo"], []}
    assert OptionParser.parse(["--no-docs=foo", "bar"], switches: [docs: :boolean])
           == {[], ["bar"], [{"--no-docs", "foo"}]}
    assert OptionParser.parse(["--no-docs=", "bar"], switches: [docs: :boolean])
           == {[], ["bar"], [{"--no-docs", ""}]}
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
           == {[], [], [{"--require", nil}]}
  end

  test "parses configured strings" do
    assert OptionParser.parse(["--value", "1", "foo"], switches: [value: :string])
           == {[value: "1"], ["foo"], []}
    assert OptionParser.parse(["--value=1", "foo"], switches: [value: :string])
           == {[value: "1"], ["foo"], []}
    assert OptionParser.parse(["--value"], switches: [value: :string])
           == {[], [], [{"--value", nil}]}
    assert OptionParser.parse(["--no-value"], switches: [value: :string])
           == {[], [], [{"--no-value", nil}]}
  end

  test "parses configured integers" do
    assert OptionParser.parse(["--value", "1", "foo"], switches: [value: :integer])
           == {[value: 1], ["foo"], []}
    assert OptionParser.parse(["--value=1", "foo"], switches: [value: :integer])
           == {[value: 1], ["foo"], []}
    assert OptionParser.parse(["--value", "WAT", "foo"], switches: [value: :integer])
           == {[], ["foo"], [{"--value", "WAT"}]}
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
           == {[], ["foo"], [{"--value", "WAT"}]}
  end

  test "parses no switches as flags" do
    assert OptionParser.parse(["--no-docs", "foo"])
           == {[no_docs: true], ["foo"], []}
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
    assert options == {[source: "from_docs/"], ["1", "2", "3"], []}

    options = OptionParser.parse_head(["--source", "from_docs/", "--", "1", "2", "3"])
    assert options == {[source: "from_docs/"], ["1", "2", "3"], []}

    options = OptionParser.parse(["--no-dash", "foo", "bar", "--", "-x"])
    assert options == {[no_dash: true], ["foo", "bar", "-x"], []}

    options = OptionParser.parse_head(["--no-dash", "foo", "bar", "--", "-x"])
    assert options == {[no_dash: true], ["foo", "bar", "--", "-x"], []}
  end

  test "goes beyond the first non option arguments" do
    args = ["--source", "from_docs/", "test/enum_test.exs", "--verbose"]
    assert OptionParser.parse(args)
           == {[source: "from_docs/", verbose: true], ["test/enum_test.exs"], []}
  end

  test "parses more than one key/value options" do
    assert OptionParser.parse(["--source", "from_docs/", "--docs", "show"])
           == {[source: "from_docs/", docs: "show"], [], []}
  end

  test "collects multiple invalid options" do
    args = ["--bad", "opt", "foo", "-o", "bad", "bar"]
    assert OptionParser.parse(args, switches: [bad: :integer])
           == {[], ["foo", "bar"], [{"--bad", "opt"}, {"-o", "bad"}]}
  end

  test "parses more than one key/value options using strict" do
    assert OptionParser.parse(["--source", "from_docs/", "--docs", "show"],
                              strict: [source: :string, docs: :string])
           == {[source: "from_docs/", docs: "show"], [], []}

    assert OptionParser.parse(["--source", "from_docs/", "--doc", "show"],
                              strict: [source: :string, docs: :string])
           == {[source: "from_docs/"], ["show"], [{"--doc", nil}]}

    assert OptionParser.parse(["--source", "from_docs/", "--doc=show"],
                              strict: [source: :string, docs: :string])
           == {[source: "from_docs/"], [], [{"--doc", nil}]}
  end

  test "parses - as argument" do
    assert OptionParser.parse(["-a", "-", "-", "-b", "-"], aliases: [b: :boo])
           == {[boo: "-"], ["-"], [{"-a", "-"}]}

    assert OptionParser.parse(["--foo", "-", "-b", "-"], strict: [foo: :boolean, boo: :string], aliases: [b: :boo])
           == {[foo: true, boo: "-"], ["-"], []}
  end

  test "next strict: good options" do
    config = [strict: [str: :string, int: :integer, bool: :boolean]]
    assert OptionParser.next(["--str", "hello", "..."], config)
           == {:ok, :str, "hello", ["..."]}
    assert OptionParser.next(["--int=13", "..."], config)
           == {:ok, :int, 13, ["..."]}
    assert OptionParser.next(["--bool=false", "..."], config)
           == {:ok, :bool, false, ["..."]}
    assert OptionParser.next(["--no-bool", "..."], config)
           == {:ok, :bool, false, ["..."]}
    assert OptionParser.next(["--bool", "..."], config)
           == {:ok, :bool, true, ["..."]}
    assert OptionParser.next(["..."], config)
           == {:error, ["..."]}
  end

  test "next strict: unknown options" do
    config = [strict: [bool: :boolean]]
    assert OptionParser.next(["--str", "13", "..."], config)
           == {:undefined, "--str", nil, ["13", "..."]}
    assert OptionParser.next(["--int=hello", "..."], config)
           == {:undefined, "--int", "hello", ["..."]}
    assert OptionParser.next(["-no-bool=other", "..."], config)
           == {:undefined, "-no-bool", "other", ["..."]}
  end

  test "next strict: bad type" do
    config = [strict: [str: :string, int: :integer, bool: :boolean]]
    assert OptionParser.next(["--str", "13", "..."], config)
           == {:ok, :str, "13", ["..."]}
    assert OptionParser.next(["--int=hello", "..."], config)
           == {:invalid, "--int", "hello", ["..."]}
    assert OptionParser.next(["--int", "hello", "..."], config)
           == {:invalid, "--int", "hello", ["..."]}
    assert OptionParser.next(["--bool=other", "..."], config)
           == {:invalid, "--bool", "other", ["..."]}
  end

  test "next strict: missing value" do
    config = [strict: [str: :string, int: :integer, bool: :boolean]]
    assert OptionParser.next(["--str"], config)
           == {:invalid, "--str", nil, []}
    assert OptionParser.next(["--int"], config)
           == {:invalid, "--int", nil, []}
    assert OptionParser.next(["--bool=", "..."], config)
           == {:invalid, "--bool", "", ["..."]}
    assert OptionParser.next(["--no-bool=", "..."], config)
           == {:undefined, "--no-bool", "", ["..."]}
  end
end
