defmodule String.Unicode.ScriptSet do
  @moduledoc false

  # Used at compile-time and run-time
  # There are 160+ scripts, so a script set is an int of 1-3 64-bit words
  # Bitwise isn't loaded yet so we use :erlang
  def empty_script_set(), do: 0
  def is_empty(ss) when ss == 0, do: true
  def is_empty(_), do: false
  def intersection(left, right), do: :erlang.band(left, right)
  def union(left, right), do: :erlang.bor(left, right)
  def remove(left, right), do: :erlang.bxor(left, intersection(left, right))
  def set_index(idx), do: :erlang.bsl(1, idx)
end

defmodule String.Tokenizer.Security do
  @moduledoc false

  # UTS39 security checks that operate on all tokens in a file,
  # like Confusables. If we add whole-file mixed-script-confusable-characters
  # checks we can add them to the list of lints here
  def unicode_lint_warnings(tokens, file \\ "nofile") do
    for warning <- confusables(tokens),
        do: format_warning(file, warning)
  end

  defp format_warning(file, {token, reason}) do
    {_, {line, col, _}, _} = token
    {{line, col}, file, to_charlist(reason)}
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

  defp confusable_skeleton(s) do
    # "- Convert X to NFD format, as described in [UAX15].
    #  - Concatenate the prototypes for each character in X according to
    #    the specified data, producing a string of exemplar characters.
    #  - Reapply NFD." (UTS 39 section 4, skeleton definition)
    :unicode.characters_to_nfd_list(s)
    |> Enum.map(&confusable_prototype/1)
    |> :unicode.characters_to_nfd_list()
  end

  ## Mixed script

  # The following implements script resolution and mixed-script detection,
  #  for use in mixed-script confusable detection
  # UTS 39, Security - 5.1
  #  https://www.unicode.org/reports/tr39/#Mixed_Script_Detection)
  #
  # Assumes 3 text files from UAX24 (Scripts)
  # 1. Scripts.txt               codepoint => primary script (by full name)
  # 2. ScriptExtensions.txt      codepoint => N scripts, (by short names)
  # 3. PropertyValueAliases.txt  short names <=> long names mapping
  #

  # first we'll build a lookup of short <=> long names, starting with:
  initial_mapping = %{
    "Jpan" => "Japanese",
    "Kore" => "Korean",
    "Hanb" => "Han with Bopomofo",
    "Zzzz" => "Unknown"
  }

  script_aliases =
    Path.join(__DIR__, "PropertyValueAliases.txt")
    |> File.read!()
    |> String.split(["\r\n", "\n"], trim: true)
    |> Enum.map(&String.split(&1, [";", " "], trim: true))
    |> Enum.reduce(initial_mapping, fn
      ["sc", short, long | _], acc -> Map.put_new(acc, short, long)
      _, acc -> acc
    end)
    |> Enum.sort()

  # ordered lists of name => index
  script_shortnames_sorted = Enum.map(script_aliases, &elem(&1, 0))
  script_names_sorted = Enum.map(script_aliases, &elem(&1, 1))

  # this module uses ScriptSet, the alias data above, and the Script*.txt
  # files, to implement 'resolved script sets'. that's enough to tell
  # us if script mixing is occurring, and which scripts are involved.
  #
  # Since some cases of mixing are legitimate, like using acronyms from
  # another script, we'll use this in 'MixedScriptWithOnlyConfusableCharacters'
  # to try to highlight only that rare but potentially impactful case.
  #
  import String.Unicode.ScriptSet
  @empty empty_script_set()

  script_shortname_indices = script_shortnames_sorted |> Enum.with_index() |> Map.new()
  script_indices = script_names_sorted |> Enum.with_index() |> Map.new()

  # next we handle the 'augmented' part of 'augmented script sets' from UTS 39
  augmentation_rules = %{
    "Hani" => ~w(Hanb Jpan Kore),
    "Hira" => ~w(Jpan),
    "Kana" => ~w(Jpan),
    "Hang" => ~w(Kore),
    "Bopo" => ~w(Hanb)
  }

  # the above lookup, replaced with their indices in list of scripts
  #  for use in scriptsets
  augmented_indices =
    for {k, vs} <- augmentation_rules, into: %{} do
      for_script_with_this_index = script_shortname_indices[k]
      add_these_indices = Enum.map(vs, &script_shortname_indices[&1])
      {for_script_with_this_index, add_these_indices}
    end

  # when accumulating script indices into scriptsets,
  #  we check for those augmentated indices
  add_script_index_to_scriptset = fn scriptset, script_index ->
    scriptset = union(scriptset, set_index(script_index))

    case augmented_indices[script_index] do
      nil ->
        scriptset

      indices_to_add ->
        for idx <- indices_to_add, reduce: scriptset do
          ss -> union(ss, set_index(idx))
        end
    end
  end

  # build a single scriptset a list of script names (using
  #  the function defined above, so it's an 'augmented script set')
  scriptset = fn lookup, names ->
    names
    |> String.split()
    |> Enum.reduce(empty_script_set(), fn script_name, scripts_acc_bitmap ->
      index = Map.get(lookup, script_name)
      add_script_index_to_scriptset.(scripts_acc_bitmap, index)
    end)
  end

  # we can build list of codepoint-to-scriptset mappings
  # from a file and a scriptname lookup
  find_matches =
    &(Regex.run(&2, &1, capture: :all_but_first) || Regex.run(&3, &1, capture: :all_but_first))

  build_scriptsets = fn fname, lu ->
    single = ~r/^ *([0-9A-F]+) *; *([^#]+) *#/u
    range = ~r/^ *([0-9A-F]+)\.\.([0-9A-F]+) *; *([^#]+) *#/u
    matches =
      Path.join(__DIR__, fname)
      |> File.read!()
      |> String.split(["\r\n", "\n"], trim: true)
      |> Enum.map(&find_matches.(&1, range, single))

    for ls <- matches, ls != nil, reduce: [] do
      acc ->
        case ls do
          [a, b, scripts] ->
            entry = {String.to_integer(a, 16), String.to_integer(b, 16), scriptset.(lu, scripts)}
            [entry | acc]

          [a, scripts] ->
            [{String.to_integer(a, 16), scriptset.(lu, scripts)} | acc]
        end
    end
  end

  script_extensions_ss = build_scriptsets.("ScriptExtensions.txt", script_shortname_indices)
  scripts_ss = build_scriptsets.("Scripts.txt", script_indices)

  def scripts(c) when not is_integer(c), do: {:error, :non_integer}

  # precedence is ScriptExtensions.txt first, then Scripts.txt,
  # then fall back to 'Unknown', per headers of those text files.
  for entry <- script_extensions_ss ++ scripts_ss do
    case entry do
      {a, b, ss} -> def scripts(c) when c in unquote(a)..unquote(b), do: unquote(ss)
      {a, ss} -> def scripts(unquote(a)), do: unquote(ss)
    end
  end

  def scripts(_other) do
    unquote(scriptset.(script_indices, "Unknown"))
  end

  # 'resolved script set ignores chars w/Extensions {Common}, {Inherited} and augments
  #  characters with CJK scripts with their respective writing systems. [ALL] are
  #  ignored when testing for differences in script."
  common = scriptset.(script_indices, "Common")
  inh = scriptset.(script_indices, "Inherited")
  all_ss = union(inh, common)
  all_sets = [common, inh, all_ss]

  @compile {:inline, all?: 1}
  defp all?(ss), do: ss in unquote(all_sets)

  # 'define the 'resolved script set' for a string to be the intersection
  #  of the augmented script sets over all characters in the string'
  defp resolve([]), do: {@empty, @empty}
  defp resolve([c | chars]), do: continue(chars, {scripts(c), scripts(c)})
  defp continue([c | chars], sets), do: continue(chars, add(sets, scripts(c)))
  defp continue([], sets), do: sets

  # - 'ignore chars w/Extensions {Common}, {Inherited} when testing for diffs in script'
  # - 'don't resolve {ø} for all-common identifiers, like "_123"' (Table 1a)
  defp add({resolved, seen} = sets, ss) do
    cond do
      @empty == seen -> {ss, ss}
      all?(ss) -> sets
      not all?(ss) and (all?(seen) or seen == @empty) -> {ss, ss}
      true -> {intersection(resolved, ss), union(seen, ss)}
    end
  end

  # 'Highly Restrictive: The string qualifies as Single Script, or
  #  The string is covered by any of the following sets of scripts,
  #  according to the definition in Section 5.1:
  #   Latin + Han + Hiragana + Katakana; or equivalently: Latn + Jpan
  #   Latin + Han + Bopomofo; or equivalently: Latn + Hanb
  #   Latin + Han + Hangul; or equivalently: Latn + Kore'
  latn = scriptset.(script_shortname_indices, "Latn")

  @highly_restrictive [
    union(latn, scriptset.(script_shortname_indices, "Jpan")),
    union(latn, scriptset.(script_shortname_indices, "Hanb")),
    union(latn, scriptset.(script_shortname_indices, "Kore"))
  ]
  def highly_restrictive(chars) do
    # let's try making our ideal error msg and returning it in this case
    case resolve(chars) do
      {_resolved = @empty, _seen} ->
        # in the rare case of identifiers whose script doesn't resolve,
        # we check if they meet highly restrictive rules
        non_common_chars = Enum.filter(chars, &(not all?(scripts(&1))))

        if Enum.any?(@highly_restrictive, &cover?(non_common_chars, &1)) do
          :ok
        else
          {_resolved, seen} = resolve(chars)

          msg = '''
          Unsafe mixed-script identifier \'#{chars}\' using {#{inspect_scripts(seen)}} does not meet the requirements of Unicode TS39 Section 5.2, 'Highly Restrictive'.

          '''

          per_char_msg = per_char_scripts(chars)

          {:error, msg ++ per_char_msg ++ '\n'}
        end

      {_resolved, _seen} ->
        :ok
    end
  end

  defp per_char_scripts(chars) do
    for c <- chars, into: '' do
      hex = :io_lib.format(' \\u~4.16.0B', [c])
      ' #{hex} \'#{[c]}\' {#{c |> scripts |> inspect_scripts}}\n'
    end
  end

  # 'A set of scripts is defined to cover a string if the intersection of
  #  that set with the augmented script sets of all characters in the string
  #  is nonempty; in other words, if every character in the string shares at
  #  least one script with the cover set.'
  defp cover?([c | chars], set), do: cover?(chars, set, intersection(set, scripts(c)))
  defp cover?(_chars, _set, @empty), do: false
  defp cover?([], _set, _not_empty), do: true
  defp cover?([c | chars], set, _), do: cover?(chars, set, intersection(set, scripts(c)))

  defp inspect_scripts(@empty), do: ["∅"]

  defp inspect_scripts(scriptset, naming \\ :full) do
    lookup =
      case naming do
        :full -> unquote(Macro.escape(script_names_sorted))
        _ -> unquote(Macro.escape(script_shortnames_sorted))
      end

    scriptset
    |> Integer.to_string(2)
    |> String.split("", trim: true)
    |> Enum.reverse()
    |> Enum.with_index()
    |> Enum.reduce([], fn
      {"1", idx}, acc -> [Enum.at(lookup, idx) | acc]
      _, acc -> acc
    end)
    |> Enum.sort()
    |> Enum.join(", ")
  end
end
