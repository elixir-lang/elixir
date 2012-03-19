defmodule OptionParser do
  def parse([h|t], dict // []) do
    dict = do_parse(h, dict)
    parse(t, dict)
  end

  def parse([], dict) do
    dict
  end

  defp do_parse('--' ++ option, dict) do
    Orddict.put dict, list_to_atom(option), true
  end
end
