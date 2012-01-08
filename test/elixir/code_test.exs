Code.require_file "../test_helper", __FILE__

defmodule CodeTest do
  use ExUnit::Case

  contents = quote do
    defmodule CodeTest::Sample do
      def eval_quoted_info, do: { __MODULE__, __FILE__, __LINE__ }
    end
  end

  Code.eval_quoted contents, [], __FILE__, __LINE__

  def test_eval_quoted do
    # We do not assert on the line here because macros
    # always ignore the line numbers. We need to revaluate
    # the situation on Erlang R15.
    { ::CodeTest::Sample, __FILE__, _ } = CodeTest::Sample.eval_quoted_info()
  end

  def test_require do
    Code.require_file "../fixtures/code_sample", __FILE__

    expanded = File.expand_path("test/elixir/fixtures/code_sample.exs")
    true = Erlang.lists.member(expanded, Code.loaded_files)
  end

  def test_require_on_failure do
    expanded = File.expand_path("code_sample.exs")

    try do
      Code.require_file "code_sample"
      error { :bad_assertion, "Expected code_sample to not be available" }
    catch: { :error, { :enoent, ^expanded }, _ }
    end
  end
end