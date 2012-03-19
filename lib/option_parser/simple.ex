defmodule OptionParser::Simple do
  def parse(['-' ++ option,h|t], dict // []) do
    option = normalize_option(option)

    case h do
    match: '-' ++ _
      dict = Orddict.put dict, option, true
      parse([h|t], dict)
    else:
      dict = key_value(option, h, dict)
      parse(t, dict)
    end
  end

  def parse(['-' ++ option], dict) do
    dict = Orddict.put dict, normalize_option(option), true
  end

  def parse([], dict) do
    dict
  end

  ## Helpers

  defp key_value(key, boolean, dict) when boolean == 'false' \
                                     when boolean == 'true' do
    Orddict.put dict, key, list_to_atom(boolean)
  end

  defp key_value(key, value, dict) do
    Orddict.put dict, key, value
  end

  defp normalize_option('-' ++ option) do
    list_to_atom(option)
  end

  defp normalize_option(option) do
    list_to_atom(option)
  end
end
