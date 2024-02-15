Code.require_file("../test_helper.exs", __DIR__)

defmodule Mix.SCMTest do
  use MixTest.Case

  setup do
    available = Mix.SCM.available()
    on_exit(fn -> Mix.State.put(:scm, available) end)
    :ok
  end

  test "prepends an SCM" do
    Mix.SCM.prepend(Hello)
    assert Enum.at(Mix.SCM.available(), 0) == Hello
    Mix.SCM.delete(Hello)
    assert Hello not in Mix.SCM.available()
  end

  test "appends an SCM" do
    Mix.SCM.append(Hello)
    assert Enum.at(Mix.SCM.available(), -1) == Hello
    Mix.SCM.delete(Hello)
    assert Hello not in Mix.SCM.available()
  end
end
