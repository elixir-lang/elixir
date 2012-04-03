Code.require_file "../test_helper", __FILE__

defmodule CodeTest do
  use ExUnit.Case

  contents = quote do
    defmodule CodeTest.Sample do
      def eval_quoted_info, do: { __MODULE__, __FILE__, __LINE__ }
    end
  end

  Code.eval_quoted contents, [], file: "sample.ex", line: 13

  test :eval do
    assert_equal { 3, [] }, Code.eval("1 + 2")
    assert_match { 3, _ }, Code.eval("a + b", [a: 1, b: 2], file: __FILE__, line: __LINE__)
  end

  test :eval_quoted do
    assert_equal { 3, [] }, Code.eval_quoted(quote(do: 1 + 2))
    assert_equal { CodeTest.Sample, "sample.ex", 13 }, CodeTest.Sample.eval_quoted_info()
  end

  test :require do
    Code.require_file "../fixtures/code_sample", __FILE__

    expanded = File.expand_path("test/elixir/fixtures/code_sample.exs")
    assert_member expanded, Code.loaded_files
  end

  test :file do
    assert_equal __FILE__, :filename.absname(__FILE__)
  end

  test :compile_source do
    compile = __MODULE__.__info__(:compile)

    # Erlang has a bug that does not allow us to set the source
    # when compiling forms. In such cases, the source will be
    # under compile option. This is fixed and future Erlang
    # version will return the proper source always (source2).
    options = :proplists.get_value(:options, compile, [])
    source1 = :proplists.get_value(:source, options, nil)
    source2 = :proplists.get_value(:source, compile, nil)

    assert_equal binary_to_list(__FILE__), source1 || source2
  end
end
