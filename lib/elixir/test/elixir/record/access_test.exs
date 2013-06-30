Code.require_file "../test_helper.exs", __DIR__

defmodule Record.AccessTest do
  use ExUnit.Case, async: true

  defrecord Config, integer: 0, value: nil do
    def get_integer(__MODULE__[integer: integer]) do
      integer
    end
  end

  test :keywords do
    assert Config[] == { Config, 0, nil }
    assert Config[integer: 1] == { Config, 1, nil }
  end

  test :in_match_with_variable do
    assert get_var(Config.new) == 0
    assert get_var(Config.new(integer: 1)) == 1
  end

  test :in_match_with_record_match do
    assert is_config(Config.new) == true
    assert is_config({ Access.AtomTest, 1 }) == false
    assert is_config({ Config, 1, 2, 3 }) == false
  end

  test :in_match_with_field_match do
    assert is_zero(Config.new) == true
    assert is_zero(Config.new(integer: 1)) == false
  end

  test :match do
    assert Config[integer: 1] = Config.new(integer: 1)
    refute Config[integer: 1] = Config.new(integer: 0)
  end

  test :underscore_record_syntax do
    record = Config[_: 0]
    assert Config[integer: 0, value: 0] == record
    assert Config[_: _] = Config[_: "x"]
    refute match?(Config[_: 0], Config[_: 1])
  end

  test :access_protocol_on_being_defined_record do
    assert Config.new(integer: 13).get_integer == 13
  end

  defp get_var(Config[integer: integer]) do
    integer
  end

  defp is_zero(Config[integer: 0]), do: true
  defp is_zero(Config[integer: _]),  do: false

  defp is_config(Config[]), do: true
  defp is_config(_), do: false
end
