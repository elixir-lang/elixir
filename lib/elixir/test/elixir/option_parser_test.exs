Code.require_file "test_helper.exs", __DIR__

defmodule OptionParserTest do
  use ExUnit.Case, async: true

  test "parses boolean option" do
    assert OptionParser.parse(["--docs"]) == { [docs: true], [] }
  end

  test "parses alias boolean option" do
    assert OptionParser.parse(["-d"]) == { [d: true], [] }
  end

  test "parses alias boolean option as the alias key" do
    assert OptionParser.parse(["-d"], aliases: [d: :docs]) == { [docs: true], [] }
  end

  test "parses more than one boolean option" do
    assert OptionParser.parse(["--docs", "--compile"]) == { [docs: true, compile: true], [] }
  end

  test "parses more than one boolean options as the alias" do
    assert OptionParser.parse(["--d", "--compile"], aliases: [d: :docs]) == { [docs: true, compile: true], [] }
  end

  test "parses key/value option" do
    assert OptionParser.parse(["--source", "form_docs/"]) == { [source: "form_docs/"], [] }
  end

  test "parses key=value option" do
    assert OptionParser.parse(["--source=form_docs/", "other"]) == { [source: "form_docs/"], ["other"] }
  end

  test "parses alias key/value option" do
    assert OptionParser.parse(["-s", "from_docs/"]) == { [s: "from_docs/"], [] }
  end

  test "parses alias key/value option as the alias" do
    assert OptionParser.parse(["-s", "from_docs/"], aliases: [s: :source]) == { [source: "from_docs/"], [] }
  end

  test "parses alias key=value option as the alias" do
    assert OptionParser.parse(["-s=from_docs/", "other"], aliases: [s: :source]) == { [source: "from_docs/"], ["other"] }
  end

  test "parses configured booleans" do
    assert OptionParser.parse(["--docs=false"], switches: [docs: :boolean]) == { [docs: false], [] }
    assert OptionParser.parse(["--docs=true"],  switches: [docs: :boolean]) == { [docs: true], [] }
    assert OptionParser.parse(["--docs=other"], switches: [docs: :boolean]) == { [docs: false], [] }

    assert OptionParser.parse(["--docs", "foo"],    switches: [docs: :boolean]) == { [docs: true], ["foo"] }
    assert OptionParser.parse(["--docs", "foo"],    switches: [docs: :boolean])  == { [docs: true], ["foo"] }
    assert OptionParser.parse(["--no-docs", "foo"], switches: [docs: :boolean]) == { [docs: false], ["foo"] }
  end

  test "does not set unparsed booleans" do
    assert OptionParser.parse(["foo"], switches: [docs: :boolean]) == { [], ["foo"] }
  end

  test "keeps options on configured keep" do
    assert OptionParser.parse(["--require", "foo", "--require", "bar", "baz"], switches: [require: :keep]) ==
      { [require: "foo", require: "bar"], ["baz"] }
  end

  test "parses configured integers" do
    assert OptionParser.parse(["--value", "1", "foo"], switches: [value: :integer])  == { [value: 1], ["foo"] }
    assert OptionParser.parse(["--value", "WAT", "foo"], switches: [value: :integer]) == { [], ["foo"] }
  end

  test "parses configured integers with keep" do
    assert OptionParser.parse(["--value", "1", "--value", "2", "foo"],
                              switches: [value: [:integer, :keep]]) == { [value: 1, value: 2], ["foo"] }
  end

  test "parses configured floats" do
    assert OptionParser.parse(["--value", "1.0", "foo"], switches: [value: :float])  == { [value: 1.0], ["foo"] }
    assert OptionParser.parse(["--value", "WAT", "foo"], switches: [value: :float]) == { [], ["foo"] }
  end

  test "parses no switches as flags" do
    assert OptionParser.parse(["--no-docs", "foo"])  == { [no_docs: true], ["foo"] }
  end

  test "parses more than one key/value options" do
    assert OptionParser.parse(["--source", "from_docs/", "--docs", "show"]) ==
      { [source: "from_docs/", docs: "show"], [] }
  end

  test "overrides options by default" do
    assert OptionParser.parse(["--require", "foo", "--require", "bar", "baz"]) ==
      { [require: "bar"], ["baz"] }
  end

  test "parses mixed options" do
    options = OptionParser.parse(["--source", "from_docs/", "--compile", "-x"])
    assert options == { [source: "from_docs/", compile: true, x: true], [] }
  end

  test "stops on first non option arguments" do
    options = OptionParser.parse_head(["--source", "from_docs/", "test/enum_test.exs", "--verbose"])
    assert options == { [source: "from_docs/"], ["test/enum_test.exs", "--verbose"] }
  end

  test "stops on --" do
    options = OptionParser.parse_head(["--source", "from_docs/", "--", "1", "2", "3"])
    assert options == { [source: "from_docs/"], ["--", "1", "2", "3"] }
  end

  test "goes beyond the first non option arguments" do
    options = OptionParser.parse(["--source", "from_docs/", "test/enum_test.exs", "--verbose"])
    assert options == { [source: "from_docs/", verbose: true], ["test/enum_test.exs"] }
  end
end
