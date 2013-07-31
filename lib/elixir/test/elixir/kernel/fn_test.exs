Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.FnTest do
  use ExUnit.Case, async: true

  test "capture remote" do
    assert (&:erlang.atom_to_list/1).(:a) == 'a'
    assert (&Kernel.atom_to_list/1).(:a) == 'a'

    assert (&List.flatten/1).([[0]]) == [0]
    assert (&(List.flatten/1)).([[0]]) == [0]
  end

  test "capture local" do
    assert (&atl/1).(:a) == 'a'
    assert (&(atl/1)).(:a) == 'a'
  end

  test "capture imported" do
    assert (&atom_to_list/1).(:a) == 'a'
    assert (&(atom_to_list/1)).(:a) == 'a'
  end

  defp atl(arg) do
    :erlang.atom_to_list arg
  end
end