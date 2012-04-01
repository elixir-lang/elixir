defmodule OptionParser.Simple do

  @doc """
  Parses the argv and returns one Tuple with parsed options
  and the arguments.

  ## Example

      OptionParser.Simple.parse(['--debug'])
      #=> { [debug: true], [] }

      OptionParser.Simple.parse(['--source', 'lib'])
      #=> { [source: 'lib'], [] }

      OptionParser.Simple.parse(['--source', 'lib', 'test/enum_test.exs'])
      #=> { [source: 'lib'], ['test/enum_test.exs'] }

  """
  def parse(['-' ++ option,h|t], dict // [], args // []) do
    option = normalize_option(option)

    case h do
    match: '-' ++ _
      dict = Keyword.put dict, option, true
      parse([h|t], dict, args)
    else:
      dict = key_value(option, h, dict)
      parse(t, dict, args)
    end
  end

  def parse(['-' ++ option], dict, args) do
    dict = Keyword.put dict, normalize_option(option), true
    { dict, args }
  end

  def parse(value, dict, args) do
    { dict, List.concat args, value }
  end

  ## Helpers

  defp key_value(key, boolean, dict) when boolean == 'false' \
                                     when boolean == 'true' do
    Keyword.put dict, key, list_to_atom(boolean)
  end

  defp key_value(key, value, dict) do
    Keyword.put dict, key, value
  end

  defp normalize_option('-' ++ option) do
    list_to_atom(option)
  end

  defp normalize_option(option) do
    list_to_atom(option)
  end
end
