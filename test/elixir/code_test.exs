Code.require_file "../test_helper", __FILE__

defmodule CodeTest do
  use ExUnit::Case

  contents = quote do
    defmodule CodeTest::Sample do
      def eval_quoted_info, do: { __MODULE__, __FILE__, __LINE__ }
    end
  end

  Code.eval_quoted contents, [], "sample.ex", 13

  test :eval_quoted do
    assert_equal { ::CodeTest::Sample, "sample.ex", 13 }, CodeTest::Sample.eval_quoted_info()
  end

  test :require do
    Code.require_file "../fixtures/code_sample", __FILE__

    expanded = File.expand_path("test/elixir/fixtures/code_sample.exs")
    assert Erlang.lists.member(expanded, Code.loaded_files)
  end
end
