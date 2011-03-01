object CodeTest
  proto ExUnit::Case

  def paths_test
    Code.unshift_path "test/elixir/fixtures"
    true = Code.paths.include? File.expand_path("test/elixir/fixtures")
  after
    Code.delete_path "test/elixir/fixtures"
  end

  def require_test
    self.assert_raise { 'enoent, "code_sample" }, do
      Code.require "code_sample"
    end

    Code.unshift_path "test/elixir/fixtures"
    true  = Code.require "code_sample"
    false = Code.require "code_sample"
    Code.loaded.include? File.expand_path("test/elixir/fixtures/code_sample.ex")
  after
    Code.delete_path "test/elixir/fixtures"
  end
end