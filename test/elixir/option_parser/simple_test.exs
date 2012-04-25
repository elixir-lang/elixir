Code.require_file "../../test_helper", __FILE__

defmodule OptionParser.SimpleTest do
  use ExUnit.Case

  test "parses boolean option" do
    assert_equal { [docs: true], [] }, OptionParser.Simple.parse(["--docs"])
  end

  test "parses alias boolean option" do
    assert_equal { [d: true], [] }, OptionParser.Simple.parse(["-d"])
  end

  test "parses more than one boolean options" do
    assert_equal { [docs: true, compile: true], [] }, OptionParser.Simple.parse(["--docs", "--compile"])
  end

  test "parses key/value option" do
    assert_equal { [source: "form_docs/"], [] }, OptionParser.Simple.parse(["--source", "form_docs/"])
  end

  test "parses alias key/value option" do
    assert_equal { [s: "from_docs/"], [] }, OptionParser.Simple.parse(["-s", "from_docs/"])
  end

  test "parses key/value option when value is false" do
    assert_equal { [docs: false], [] }, OptionParser.Simple.parse(["--docs", "false"])
  end

  test "parses key/value option when value is true" do
    assert_equal { [docs: true], [] }, OptionParser.Simple.parse(["--docs", "true"])
  end

  test "parses more than one key/value options" do
    options = OptionParser.Simple.parse(["--source", "from_docs/", "--docs", "false"])
    assert_equal { [docs: false, source: "from_docs/"], [] }, options
  end

  test "parses mixed options" do
    options = OptionParser.Simple.parse(["--source", "from_docs/", "--docs", "false", "--compile", "-x"])
    assert_equal { [docs: false, source: "from_docs/", compile: true, x: true], [] }, options
  end

  test "ignores not option arguments" do
    options = OptionParser.Simple.parse(["--source", "from_docs/", "test/enum_test.exs"])
    assert_equal { [source: "from_docs/"], ["test/enum_test.exs"] }, options
  end
end
