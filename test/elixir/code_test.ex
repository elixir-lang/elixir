object CodeTest
  proto ExUnit::Case

  def paths_test
    Code.unshift_path "test/fixtures"
    true = Code.paths.include? File.expand_path("test/fixtures")
  end
end