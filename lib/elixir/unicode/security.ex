# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

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
           "but they are written using different characters" <> dir_compare(name, previous_name)}

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
    |> bidi_skeleton()
    |> :unicode.characters_to_nfd_list()
  end

  # Unicode 15 adds bidiSkeleton because, w/RTL codepoints, idents that
  # aren't confusable LTR *are* confusable in most places human review
  # occurs (editors/browsers, thanks to bidi algo, UAX9).
  #
  # The solution is to detect spans with reversed visual direction,
  # and reverse those, so that the input we check for confusability
  # matches the perceived sequence instead of the byte sequence.
  #
  # (we need this regardless of script mixing, because direction-neutral
  # chars like _ or 0..9 can mix w/RTL chars).
  def bidi_skeleton(s) do
    # UTS39-28 4:
    #
    # Bidirectional confusability is costlier to check than
    # confusability, as [unicode bidi algo] must be applied.
    # [...] a fast path can be used: [...] if X has no characters
    # w/bidi classes R or AL, bidiSkeleton(X) = skeleton(X)
    if match?([_, _ | _], s) and any_rtl?(s) do
      unbidify(s) |> Enum.map(&confusable_prototype/1)
    else
      Enum.map(s, &confusable_prototype/1)
    end
  end

  defp any_rtl?(s), do: Enum.any?(s, &(:rtl == String.Tokenizer.dir(&1)))

  defp dir_compare(a, b) do
    """
    #{if any_rtl?(a), do: "\n\n" <> dir_breakdown(a)}
    #{if any_rtl?(b), do: dir_breakdown(b)}
    """
  end

  defp dir_breakdown(s) do
    init = "'#{s}' includes right-to-left characters:\n"

    init <>
      for codepoint <- s, into: "" do
        hex = :io_lib.format(~c"~4.16.0B", [codepoint])
        "  \\u#{hex} #{[codepoint]} #{String.Tokenizer.dir(codepoint)}\n"
      end
  end

  # make charlist match visual order by resolving the bidi levels
  # of each character and then reversing them according to uax9
  #
  #  UTS39-28 4: '[...] if the strings are known not to contain explicit
  #   directional formatting characters[...], the algorithm can
  #   be drastically simplified, [...], obviating the need for
  #   the [...] stack of the [unicode bidi algo]'
  #
  # Since identifiers are always in a ltr paragraph and cannot have
  # brackets nor formatting characters, the only levels are 0 (ltr),
  # 1 (rtl), and 2 (numbers within rtl).
  def unbidify(chars) when is_list(chars) do
    types = Enum.map(chars, &String.Tokenizer.dir/1)

    # https://www.unicode.org/reports/tr9/#W1
    {types, _} =
      Enum.map_reduce(types, :ltr, fn
        :nonspacing_mark, previous -> {previous, previous}
        type, _previous -> {type, type}
      end)

    # https://www.unicode.org/reports/tr9/#W7
    {types, _} =
      Enum.map_reduce(types, :ltr, fn
        :weak_number, :ltr -> {:ltr, :ltr}
        type, _strong when type in [:ltr, :rtl] -> {type, type}
        type, strong -> {type, strong}
      end)

    # https://www.unicode.org/reports/tr9/#I1
    levels =
      Enum.map(types, fn
        :ltr -> 0
        :rtl -> 1
        :neutral -> nil
        number when number in [:weak_number, :arabic_number] -> 2
      end)

    # https://www.unicode.org/reports/tr9/#N1 and https://www.unicode.org/reports/tr9/#N2
    #
    # Neutrals are rtl if surrounded by rtl or numbers, otherwise ltr.
    {nexts, _} =
      levels
      |> Enum.reverse()
      |> Enum.map_reduce(0, fn
        nil, next -> {next, next}
        level, _next -> {level, level}
      end)

    {levels, _} =
      levels
      |> Enum.zip(Enum.reverse(nexts))
      |> Enum.map_reduce(0, fn
        {nil, next}, previous -> {if(previous > 0 and next > 0, do: 1, else: 0), previous}
        {level, _next}, _previous -> {level, level}
      end)

    # https://www.unicode.org/reports/tr9/#L2
    chars
    |> Enum.zip(levels)
    |> reverse_levels(2)
    |> reverse_levels(1)
    |> Enum.map(&elem(&1, 0))
  end

  defp reverse_levels(pairs, level) do
    pairs
    |> Enum.chunk_by(fn {_char, char_level} -> char_level >= level end)
    |> Enum.flat_map(fn
      [{_char, char_level} | _] = chunk when char_level >= level -> Enum.reverse(chunk)
      chunk -> chunk
    end)
  end
end
