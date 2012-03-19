defmodule OptionParser::Simple do
  def parse(['--' ++ option,h|t], dict // []) do
    option = list_to_atom(option)

    case h do
    match: '-' ++ _
      dict = Orddict.put dict, option, true
      parse([h|t], dict)
    else:
      dict = key_value(option, h, dict)
      parse(t, dict)
    end
  end

  def parse(['--' ++ option], dict) do
    dict = Orddict.put dict, list_to_atom(option), true
  end

  def parse([], dict) do
    dict
  end

  defp key_value(key, boolean, dict) when boolean == 'false' \
                                     when boolean == 'true' do
    Orddict.put dict, key, list_to_atom(boolean)
  end

  defp key_value(key, value, dict) do
    Orddict.put dict, key, value
  end
end
