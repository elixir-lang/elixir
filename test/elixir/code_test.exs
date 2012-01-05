Code.require_file "../test_helper", __FILE__

defmodule CodeTest do
  use ExUnit::Case

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