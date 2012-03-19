Code.require_file "../test_helper", __FILE__

defmodule OptionParserTest do
  use ExUnit::Case

  test "parses boolean option" do
    assert_equal [docs: true], OptionParser.parse(['--docs'])
  end

  test "parses more than one boolean option" do
    assert_equal [docs: true, compile: true], OptionParser.parse(['--docs', '--compile'])
  end

  test "parses key/value option" do
    assert_equal [source: 'form_docs/'], OptionParser.parse(['--source', 'form_docs/'])
  end

  test "parses key/value option when value is false" do
    assert_equal [docs: false], OptionParser.parse(['--docs', 'false'])
  end

  test "parses key/value option when value is true" do
    assert_equal [docs: true], OptionParser.parse(['--docs', 'true'])
  end
end
