Code.require_file "../test_helper", __FILE__

defmodule OptionParserTest do
  use ExUnit::Case

  test "parses boolean option" do
    assert_equal [docs: true], OptionParser.parse(['--docs'])
  end

  test "parses more than one boolean option" do
    assert_equal [docs: true, compile: true], OptionParser.parse(['--docs', '--compile'])
  end
end
