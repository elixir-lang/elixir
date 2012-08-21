Code.require_file "../test_helper.exs", __FILE__

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

  test "parses key/value option when value is boolean" do
    assert OptionParser.parse(["--docs", "false"]) == { [docs: false], [] }
    assert OptionParser.parse(["--docs", "true"]) == { [docs: true], [] }
  end

  test "parses key=value option when value is boolean" do
    assert OptionParser.parse(["--docs=false"]) == { [docs: false], [] }
    assert OptionParser.parse(["--docs=true"]) == { [docs: true], [] }
  end

  test "parses flags" do
    assert OptionParser.parse(["--docs", "foo"], flags: [:docs]) == { [docs: true], ["foo"] }
  end

  test "flags default to false even when not given" do
    assert OptionParser.parse(["foo"], flags: [:docs]) == { [docs: false], ["foo"] }
  end

  test "parses flags with boolean" do
    assert OptionParser.parse(["--docs", "true", "foo"], flags: [:docs])  == { [docs: true], ["foo"] }
    assert OptionParser.parse(["--docs", "false", "foo"], flags: [:docs]) == { [docs: false], ["foo"] }
  end

  test "parses no switches as flags" do
    assert OptionParser.parse(["--no-docs", "foo"])  == { [no_docs: true], ["foo"] }
  end

  test "parses more than one key/value options" do
    options = OptionParser.parse(["--source", "from_docs/", "--docs", "false"])
    assert options == { [docs: false, source: "from_docs/"], [] }
  end

  test "parses mixed options" do
    options = OptionParser.parse(["--source", "from_docs/", "--docs", "false", "--compile", "-x"])
    assert options == { [docs: false, source: "from_docs/", compile: true, x: true], [] }
  end

  test "stops on first non option arguments" do
    options = OptionParser.parse_head(["--source", "from_docs/", "test/enum_test.exs", "--verbose"])
    assert options == { [source: "from_docs/"], ["test/enum_test.exs", "--verbose"] }
  end

  test "goes beyond the first non option arguments" do
    options = OptionParser.parse(["--source", "from_docs/", "test/enum_test.exs", "--verbose"])
    assert options == { [source: "from_docs/", verbose: true], ["test/enum_test.exs"] }
  end
end
