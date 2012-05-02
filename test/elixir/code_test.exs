Code.require_file "../test_helper", __FILE__

defmodule CodeTest do
  use ExUnit.Case

  def one, do: 1

  contents = quote do
    defmodule CodeTest.Sample do
      def eval_quoted_info, do: { __MODULE__, __FILE__, __LINE__ }
    end
  end

  Code.eval_quoted contents, [], file: "sample.ex", line: 13

  test :eval do
    assert Code.eval("1 + 2") == { 3, [] }
    assert_match { 3, _ }, Code.eval("a + b", [a: 1, b: 2], file: __FILE__, line: __LINE__)
  end

  test :eval_with_scope do
    assert Code.eval("one", [], delegate_locals_to: __MODULE__) == { 1, [] }
  end

  test :eval_quoted do
    assert Code.eval_quoted(quote(do: 1 + 2)) == { 3, [] }
    assert CodeTest.Sample.eval_quoted_info() == { CodeTest.Sample, "sample.ex", 13 }
  end

  test :require do
    Code.require_file "../fixtures/code_sample", __FILE__

    expanded = File.expand_path("test/elixir/fixtures/code_sample.exs")
    assert_member expanded, Code.loaded_files
  end

  test :file do
    assert :filename.absname(__FILE__) == __FILE__
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

    assert (source1 || source2) == binary_to_list(__FILE__)
  end

  test :compile_info_returned_with_source_accessible_through_keyword_module do
    compile = __MODULE__.__info__(:compile)
    refute_nil Keyword.get(compile, :source)
  end
end
