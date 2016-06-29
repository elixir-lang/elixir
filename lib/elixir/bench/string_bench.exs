defmodule StringBench do
  use Benchfella
  
  @contents File.read!("bench/string_bench_data.txt")
  @contents_small File.read!("bench/string_bench_data_small.txt")
  
  
  @is_lower_pat Regex.compile!(~S"\p{Ll}", "u")
  defp is_lower_regex(char) do
    Regex.match?(@is_lower_pat, char)
  end
  
  @is_digit_pat Regex.compile!(~S"\p{Nd}", "u")
  defp is_digit_regex(char) do
    Regex.match?(@is_digit_pat, char)
  end


  before_each_bench _ do
    {:ok, String.codepoints(@contents)}
  end

  bench "downcase original" do
    String.Casing.downcase_orig(@contents)
  end

  bench "downcase" do
    String.downcase(@contents)
  end
  
  bench "downcase original (small data)" do
    String.Casing.downcase_orig(@contents_small)
  end

  bench "downcase (small data)" do
    String.downcase(@contents_small)
  end


  bench "upcase original" do
    String.Casing.upcase_orig(@contents)
  end

  bench "upcase" do
    String.upcase(@contents)
  end


  bench "is_lower_regex" do
    Enum.map(bench_context, &is_lower_regex/1)
  end


  bench "is_lower" do
    Enum.map(bench_context, &String.is_lower/1)
  end


  bench "is_digit_regex" do
    Enum.map(bench_context, &is_digit_regex/1)
  end


  bench "is_digit" do
    Enum.map(bench_context, &String.is_digit/1)
  end

end