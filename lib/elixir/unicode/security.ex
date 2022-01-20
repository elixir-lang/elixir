defmodule String.Tokenizer.Security do
  @moduledoc false
  # UTS39 security checks that operate on all tokens in a file,
  # like Confusables. If we add whole-file mixed-script-confusables
  # checks we can add them to the list of lints here
  #
  def unicode_lint_warnings(tokens, file \\ "nofile") do
    for lint <- [String.Confusables], warning <- lint.check_tokens(tokens),
      do: format_warning(file, warning)
  end

  defp format_warning(file, {token, reason}) do
    {_, {line, col, _}, _} = token
    {{line, col}, file, to_charlist(reason)}
  end
end

defmodule String.Confusables do
  @moduledoc false

  # AAAA ;   BBBB CCCC DDDDD ;
  # ^ char   ^ prototypical char or sequence of chars it can be confused with
  confusables_path = "confusables.txt"

  lines =
    Path.join(__DIR__, confusables_path)
    |> File.read!()
    |> String.split(["\r\n", "\n"], trim: true)

  regex = ~r/^((?:[0-9A-F]+ )+);\t((?:[0-9A-F]+ )+);/u
  matches = Enum.map(lines, &Regex.run(regex, &1, capture: :all_but_first))

  confusable_prototype_lookup =
    for [one_char_str, prototype_chars_str] = ls <- matches, !is_nil(ls), reduce: %{} do
      acc ->
        confusable = String.to_integer(String.trim(one_char_str), 16)

        prototype =
          String.split(prototype_chars_str, " ", trim: true)
          |> Enum.map(&String.to_integer(&1, 16))

        Map.put_new(acc, confusable, prototype)
    end

  # don't consider ascii confusable: 0O, l1, etc
  defp confusable_prototype(c) when c in ?A..?Z or c in ?a..?z or c in ?0..?9, do: c
  defp confusable_prototype(?_), do: ?_

  for {confusable, prototype} <- confusable_prototype_lookup do
    defp confusable_prototype(unquote(confusable)) do
      unquote(prototype)
    end
  end

  defp confusable_prototype(other), do: other

  defp confusable_skeleton(s) do
    # "- Convert X to NFD format, as described in [UAX15].
    #  - Concatenate the prototypes for each character in X according to
    #    the specified data, producing a string of exemplar characters.
    #  - Reapply NFD." (UTS 39 section 4, skeleton definition)
    String.normalize(s, :nfd)
    |> to_charlist
    |> Enum.map(&confusable_prototype/1)
    |> to_string
    |> String.normalize(:nfd)
  end

  def check_tokens(tokens) do
    {_, warnings} =
      for token <- tokens, reduce: {%{}, []} do
        {skeletons, warnings} ->
          case check_token_for_confusability(token, skeletons) do
            {:ok, skeletons} -> {skeletons, warnings}
            {:warn, reason} -> {skeletons, [{token, reason} | warnings]}
          end
      end

    warnings
  end

  defp check_token_for_confusability({kind, _, name} = token, skeletons) when kind in [:identifier, :alias] do
    skeleton = name |> to_string |> confusable_skeleton

    case skeletons[skeleton] do
      {_, _, ^name} ->
        {:ok, skeletons}

      {_, {line, _, _}, name2} when name != name2 ->
        {:warn, "confusable identifier: '#{name}' looks like '#{name2}' on line #{line}"}

      _ ->
        {:ok, Map.put(skeletons, skeleton, token)}
    end
  end

  defp check_token_for_confusability(_token, skeletons), do: {:ok, skeletons}
end
