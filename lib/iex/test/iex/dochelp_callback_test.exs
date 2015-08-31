Code.require_file "../test_helper.exs", __DIR__

defmodule IEx.DocHelp.CallBackTest do
  use IEx.Case

  test "documentation/1 for Elixir module" do
    assert {:not_found, _} = IEx.DocHelp.CallBack.documentation(IEx.Case)
    assert {:found, _} = IEx.DocHelp.CallBack.documentation(Tuple)
  end

  test "documentation/1 for Erlang module" do
    assert {:unknown, _} = IEx.DocHelp.CallBack.documentation(:erlang)
  end

  test "documentation/2 for Elixir module function" do
    assert {:found, _} = IEx.DocHelp.CallBack.documentation(IEx.DocHelp, :documentation)
  end

  test "documentation/3 for Elixir module function arity" do
    assert {:not_found, _} = IEx.DocHelp.CallBack.documentation(IEx.DocHelp, :documentation, 4)
    assert {:found, _} = IEx.DocHelp.CallBack.documentation(IEx.DocHelp, :documentation, 2)
  end

end
