Code.require_file "../test_helper", __FILE__

defmodule MixTest do
  use MixTest.Case

  test :push_and_pop_projects do
    Mix.push_project(MixTest)
    assert Mix.project == MixTest
  after
    Mix.pop_project
  end

  test :run do
    assert Mix.run(["hello"]) == "Hello, World!"
  end
end