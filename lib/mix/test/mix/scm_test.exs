Code.require_file "../test_helper.exs", __DIR__

defmodule Mix.SCMTest do
  use MixTest.Case

  setup do
    {:ok, [scm: Mix.SCM.available]}
  end

  teardown context do
    :application.set_env(:mix, :scm, context[:scm])
    :ok
  end

  test "prepends a SCM" do
    Mix.SCM.prepend(Hello)
    assert Enum.at(Mix.SCM.available, 0) == Hello
  end

  test "appends a SCM" do
    Mix.SCM.append(Hello)
    assert Enum.at(Mix.SCM.available, -1) == Hello
  end
end
