defmodule String.Tokenizer.Security do
  @moduledoc false

  # UTS39 security checks that operate on all tokens in a file,
  # like Confusables. If we add whole-file mixed-script-confusable-characters
  # checks we can add them to the list of lints here
  def unicode_lint_warnings(tokens) do
    for warning <- confusables(tokens),
        do: format_warning(warning)
  end

  defp format_warning({token, reason}) do
    {_, {line, col, _}, _} = token
    {{line, col}, to_charlist(reason)}
  end

  ## Confusables

  defp confusables(tokens) do
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

  @identifiers [
    :identifier,
    :op_identifier,
    :kw_identifier,
    :paren_identifier,
    :bracket_identifier,
    :alias,
    :atom
  ]

  defp check_token_for_confusability(
         {kind, {_line, _column, [_ | _] = name} = info, _},
         skeletons
       )
       when kind in @identifiers do
    skeleton = confusable_skeleton(name)

    case skeletons[skeleton] do
      {_, _, ^name} ->
        {:ok, skeletons}

      {line, _, previous_name} when name != previous_name ->
        {:warn,
         "confusable identifier: '#{name}' looks like '#{previous_name}' on line #{line}, " <>
           "but they are written using different characters"}

      _ ->
        {:ok, Map.put(skeletons, skeleton, info)}
    end
  end

  defp check_token_for_confusability(_token, skeletons), do: {:ok, skeletons}

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
    for [confusable_str, prototype_str] <- matches, reduce: %{} do
      acc ->
        confusable = String.to_integer(String.trim(confusable_str), 16)

        if Map.has_key?(acc, confusable) or
             confusable in ?A..?Z or confusable in ?a..?z or confusable in ?0..?9 do
          acc
        else
          prototype =
            prototype_str
            |> String.split(" ", trim: true)
            |> Enum.map(&String.to_integer(&1, 16))

          Map.put(acc, confusable, prototype)
        end
    end

  for {confusable, prototype} <- confusable_prototype_lookup do
    defp confusable_prototype(unquote(confusable)) do
      unquote(prototype)
    end
  end

  defp confusable_prototype(other), do: <<other::utf8>>

  def confusable_skeleton(s) do
    # "- Convert X to NFD format, as described in [UAX15].
    #  - Concatenate the prototypes for each character in X according to
    #    the specified data, producing a string of exemplar characters.
    #  - Reapply NFD." (UTS 39 section 4, skeleton definition)
    :unicode.characters_to_nfd_list(s)
    |> Enum.map(&confusable_prototype/1)
    |> :unicode.characters_to_nfd_list()
  end
end
