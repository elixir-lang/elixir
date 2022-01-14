# semi-random selection of math chars for now added as PoC
# we use the same list in Tokenizer & UncommonCodepoints
unicode_whitelist = 'λ∆Ø∂δω∇Φϕσμπκxαθᵥᵢᵣᵤᵦᵧᵨᵩᵪᶠ∫∪∩∨∧∑∋∈'

defmodule String.Tokenizer do
  @moduledoc false

  data_path = Path.join(__DIR__, "UnicodeData.txt")

  {letter_uptitlecase, start, continue, _} =
    data_path
    |> File.read!()
    |> String.split(["\r\n", "\n"], trim: true)
    |> Enum.reduce({[], [], [], nil}, fn line, acc ->
      {letter_uptitlecase, start, continue, first} = acc
      [codepoint, line] = :binary.split(line, ";")
      [name, line] = :binary.split(line, ";")
      [category, _] = :binary.split(line, ";")

      {codepoints, first} =
        case name do
          "<" <> _ when is_integer(first) ->
            last = String.to_integer(codepoint, 16)
            {Enum.to_list(last..first), nil}

          "<" <> _ ->
            first = String.to_integer(codepoint, 16)
            {[first], first + 1}

          _ ->
            {[String.to_integer(codepoint, 16)], nil}
        end

      cond do
        category in ~w(Lu Lt) ->
          {codepoints ++ letter_uptitlecase, start, continue, first}

        category in ~w(Ll Lm Lo Nl) ->
          {letter_uptitlecase, codepoints ++ start, continue, first}

        category in ~w(Mn Mc Nd Pc) ->
          {letter_uptitlecase, start, codepoints ++ continue, first}

        true ->
          {letter_uptitlecase, start, continue, first}
      end
    end)

  prop_path = Path.join(__DIR__, "PropList.txt")

  {start, continue, patterns} =
    prop_path
    |> File.read!()
    |> String.split(["\r\n", "\n"], trim: true)
    |> Enum.reduce({start, continue, []}, fn line, acc ->
      [codepoints | category] = :binary.split(line, ";")

      pos =
        case category do
          [" Other_ID_Start" <> _] -> 0
          [" Other_ID_Continue" <> _] -> 1
          [" Pattern_White_Space" <> _] -> 2
          [" Pattern_Syntax" <> _] -> 2
          _ -> -1
        end

      if pos >= 0 do
        entries =
          case :binary.split(codepoints, "..") do
            [<<codepoint::4-binary, _::binary>>] ->
              [String.to_integer(codepoint, 16)]

            [first, <<last::4-binary, _::binary>>] ->
              Enum.to_list(String.to_integer(last, 16)..String.to_integer(first, 16))
          end

        put_elem(acc, pos, entries ++ elem(acc, pos))
      else
        acc
      end
    end)

  id_upper = letter_uptitlecase -- patterns
  id_start = start -- patterns
  id_continue = continue -- patterns

  unicode_upper = Enum.filter(id_upper, &(&1 > 127))
  unicode_start = Enum.filter(id_start, &(&1 > 127))
  unicode_continue = Enum.filter(id_continue, &(&1 > 127))

  rangify = fn [head | tail] ->
    {first, last, acc} =
      Enum.reduce(tail, {head, head, []}, fn
        number, {first, last, acc} when number == first - 1 ->
          {number, last, acc}

        number, {first, last, acc} ->
          {number, number, [{first, last} | acc]}
      end)

    [{first, last} | acc]
  end

  @compile {:inline, ascii_upper?: 1, ascii_start?: 1, ascii_continue?: 1}
  defp ascii_upper?(entry), do: entry in ?A..?Z

  defp ascii_start?(?_), do: true
  defp ascii_start?(entry), do: entry in ?a..?z

  defp ascii_continue?(entry), do: entry in ?0..?9

  range = rangify.(unicode_upper)

  for {first, last} <- range do
    if first == last do
      defp unicode_upper?(unquote(first)), do: true
    else
      defp unicode_upper?(entry) when entry in unquote(first)..unquote(last), do: true
    end
  end

  defp unicode_upper?(_), do: false

  range = rangify.(unicode_start)

  for {first, last} <- range do
    if first == last do
      defp unicode_start?(unquote(first)), do: true
    else
      defp unicode_start?(entry) when entry in unquote(first)..unquote(last), do: true
    end
  end

  defp unicode_start?(c) when c in unquote(unicode_whitelist), do: true
  defp unicode_start?(_), do: false

  unless {13312, 19903} in range do
    raise "CHECK: CJK Ideograph not in range. Make sure all properties are listed."
  end

  for {first, last} <- rangify.(unicode_continue) do
    if first == last do
      defp unicode_continue?(unquote(first)), do: true
    else
      defp unicode_continue?(entry) when entry in unquote(first)..unquote(last), do: true
    end
  end

  defp unicode_continue?(c) when c in unquote(unicode_whitelist), do: true
  defp unicode_continue?(_), do: false

  # Pattern is used as a performance check since most
  # atoms and variables end with an atom character.
  for {first, last} <- rangify.(patterns), last <= 127 do
    if first == last do
      defp ascii_pattern?(unquote(first)), do: true
    else
      defp ascii_pattern?(entry) when entry in unquote(first)..unquote(last), do: true
    end
  end

  defp ascii_pattern?(_), do: false

  def tokenize([head | tail]) do
    cond do
      ascii_upper?(head) ->
        validate(continue(tail, [head], 1, true, []), :alias)

      ascii_start?(head) ->
        validate(continue(tail, [head], 1, true, []), :identifier)

      unicode_upper?(head) ->
        validate(continue(tail, [head], 1, false, []), :atom)

      unicode_start?(head) ->
        validate(continue(tail, [head], 1, false, []), :identifier)

      true ->
        {:error, :empty}
    end
  end

  def tokenize([]) do
    {:error, :empty}
  end

  defp continue([?! | tail], acc, length, ascii_letters?, special) do
    {[?! | acc], tail, length + 1, ascii_letters?, [?! | special]}
  end

  defp continue([?? | tail], acc, length, ascii_letters?, special) do
    {[?? | acc], tail, length + 1, ascii_letters?, [?? | special]}
  end

  defp continue([?@ | tail], acc, length, ascii_letters?, special) do
    continue(tail, [?@ | acc], length + 1, ascii_letters?, [?@ | List.delete(special, ?@)])
  end

  defp continue([head | tail] = list, acc, length, ascii_letters?, special) do
    cond do
      ascii_start?(head) or ascii_upper?(head) or ascii_continue?(head) ->
        continue(tail, [head | acc], length + 1, ascii_letters?, special)

      not ascii_pattern?(head) and
          (unicode_start?(head) or unicode_upper?(head) or unicode_continue?(head)) ->
        continue(tail, [head | acc], length + 1, false, special)

      true ->
        {acc, list, length, ascii_letters?, special}
    end
  end

  defp continue([], acc, length, ascii_letters?, special) do
    {acc, [], length, ascii_letters?, special}
  end

  defp validate({acc, rest, length, ascii_letters?, special}, kind) do
    acc = :lists.reverse(acc)

    if ascii_letters? or :unicode.characters_to_nfc_list(acc) == acc do
      {kind, acc, rest, length, ascii_letters?, special}
    else
      {:error, {:not_nfc, acc}}
    end
  end

  def unicode_lint_warnings(tokens, file \\ "nofile") do
    alias String.Unicode.{Confusables, MixedScript, UncommonCodepoints}

    case UncommonCodepoints.check_tokens(tokens) do
      {_ascii_only = true, _} ->
        []

      {_, warnings} ->
        (warnings ++ Confusables.check_tokens(tokens) ++ MixedScript.check_tokens(tokens))
        |> Enum.map(&format_warning(file, &1))
    end
  end

  defp format_warning(file, {token, reason}) do
    {_, {line, col, _}, _} = token
    {{line, col}, file, to_charlist(reason)}
  end
end

# ---
# the rest of this file is the implementation of UTS 39 security
#
# 1. UncommonCodepoints
# 2. Confusables
# 3. MixedScript

get_lines = fn fname ->
  Path.join(__DIR__, fname)
  |> File.read!()
  |> String.split(["\r\n", "\n"], trim: true)
end

find_matches =
  &(Regex.run(&2, &1, capture: :all_but_first) || Regex.run(&3, &1, capture: :all_but_first))

defmodule String.Unicode.UncommonCodepoints do
  @moduledoc false

  single = ~r/^ *([0-9A-F]+) *;#/u
  range = ~r/^ *([0-9A-F]+)\.\.([0-9A-F]+) *;/u
  lines = get_lines.("IdentifierStatus.txt")

  allowed_codepoints =
    for ls <- Enum.map(lines, &find_matches.(&1, single, range)), !is_nil(ls), reduce: [] do
      acc -> [Enum.map(ls, &String.to_integer(&1, 16)) | acc]
    end
    |> Enum.reverse()

  def allowed_codepoint?(c) when c < 128, do: true

  for codepoints <- allowed_codepoints do
    case codepoints do
      [a, b] -> def allowed_codepoint?(c) when c in unquote(a)..unquote(b), do: true
      [a] -> def allowed_codepoint?(unquote(a)), do: true
    end
  end

  def allowed_codepoint?(c) when c in unquote(unicode_whitelist), do: true
  def allowed_codepoint?(_), do: false

  def check_tokens(tokens) do
    for token <- tokens, reduce: {true, []} do
      {was_ascii, warnings} ->
        case check_for_uncommon_codepoint(token) do
          {:ok, ascii} -> {was_ascii && ascii, warnings}
          {:warn, ascii, warning} -> {was_ascii && ascii, [warning | warnings]}
        end
    end
  end

  defp check_for_uncommon_codepoint({kind, _info, name} = token)
       when is_atom(name) and kind in [:identifier, :alias] do
    case all_allowed?(to_charlist(name)) do
      {:not_allowed, c, ascii} ->
        msg = "identifier '#{name}' has uncommon codepoint \\u#{Integer.to_string(c, 16)}"
        {:warn, ascii, {token, msg}}

      {:ok, ascii} ->
        {:ok, ascii}
    end
  end

  defp check_for_uncommon_codepoint(_token), do: {:ok, true}

  defp all_allowed?(chars, ascii \\ true)

  defp all_allowed?([c | chars], _is_ascii) when c >= 128 do
    case allowed_codepoint?(c) do
      true -> all_allowed?(chars, false)
      _ -> {:not_allowed, c, false}
    end
  end

  defp all_allowed?([_c | chars], is_ascii), do: all_allowed?(chars, is_ascii)
  defp all_allowed?([], is_ascii), do: {:ok, is_ascii}
end

defmodule String.Unicode.Confusables do
  @moduledoc false
  @skeletons %{}

  # AAAA ;\t;BBBB CCCC DDDDD;
  # ^ char   ^ prototypical chars it can be confused with
  lines = get_lines.("confusables.txt")
  regex = ~r/^((?:[0-9A-F]+ )+);\t((?:[0-9A-F]+ )+);/u
  matches = Enum.map(lines, &Regex.run(regex, &1, capture: :all_but_first))

  confusable_prototype_lookup =
    for [one_char_str, prototype_chars_str] = ls <- matches, !is_nil(ls), reduce: %{} do
      acc ->
        confusable = String.to_integer(String.trim(one_char_str), 16)

        prototype =
          String.split(prototype_chars_str, " ", trim: true)
          |> Enum.map(&String.to_integer(&1, 16))

        case String.Tokenizer.tokenize([confusable]) do
          {:error, _why} -> acc
          _ -> Map.put_new(acc, confusable, prototype)
        end
    end

  # don't consider ascii confusable: 0O, l1, etc
  def confusable_prototype(c) when c in ?A..?Z or c in ?a..?z or c in ?0..?9, do: c
  def confusable_prototype(?_), do: ?_

  for {confusable, prototype} <- confusable_prototype_lookup do
    def confusable_prototype(unquote(confusable)) do
      unquote(prototype)
    end
  end

  def confusable_prototype(other), do: other

  def confusable_skeleton(s) do
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
      for token <- tokens, reduce: {@skeletons, []} do
        {skeletons, warnings} ->
          case check_token(token, skeletons) do
            {:ok, skeletons} -> {skeletons, warnings}
            {:warn, reason} -> {skeletons, [{token, reason} | warnings]}
          end
      end

    warnings
  end

  defp check_token({kind, _, name} = token, skeletons) when kind in [:identifier, :alias] do
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

  defp check_token(_token, skeletons), do: {:ok, skeletons}
end

defmodule String.Unicode.ScriptSet do
  @moduledoc false

  # ScriptSetResolution uses this at compile-time and run-time
  # there are 160-some scripts, a set is an int of 1-3 64-bit words
  # Bitwise isn't loaded at Tokenizer load time so use :erlang
  def empty_script_set(), do: 0
  def is_empty(ss) when ss == 0, do: true
  def is_empty(_), do: false
  def intersection(left, right), do: :erlang.band(left, right)
  def union(left, right), do: :erlang.bor(left, right)
  def remove(left, right), do: :erlang.bxor(left, intersection(left, right))
  def set_index(idx), do: :erlang.bsl(1, idx)
end

# The following implements script resolution and mixed-script protection
# UTS 39, Security - 5.1, Mixed Script Detection
#   https://www.unicode.org/reports/tr39/#Mixed_Script_Detection)
#
# Assumes 3 text files from UAX24 (Scripts)
# 1. Scripts.txt              codepoint => primary script (by full name)
# 2. ScriptExtensions.txt     codepoint => N scripts, (by short names)
# 3. PropertyValueAliases.txt short names <=> long names mapping
# And 'confusables.txt' from above for Mixed-Script confusables protection.

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

# ordered lists of names => indices
script_shortnames_sorted = Enum.map(script_aliases, &elem(&1, 0))
script_names_sorted = Enum.map(script_aliases, &elem(&1, 1))

defmodule String.Unicode.ScriptSetResolution do
  # this module uses ScriptSets and data above to impl 'resolved script set';
  #  that's enough to tell us if script mixing is occurring, and what all of
  #  the scripts involved are. Since some cases of mixing are legitimate,
  #  like using acronyms from another language, MixedScript will build on
  #  that to check script mixing for confusables.
  @moduledoc false
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

  # build a scriptset from lists of script names
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
  build_scriptsets = fn fname, lu ->
    single = ~r/^ *([0-9A-F]+) *; *([^#]+) *#/u
    range = ~r/^ *([0-9A-F]+)\.\.([0-9A-F]+) *; *([^#]+) *#/u
    matches = Enum.map(get_lines.(fname), &find_matches.(&1, range, single))

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
  def resolve([]), do: {@empty, @empty}
  def resolve([c | chars]), do: continue(chars, {scripts(c), scripts(c)})
  def continue([c | chars], sets), do: continue(chars, add(sets, scripts(c)))
  def continue([], sets), do: sets

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
end

defmodule String.Unicode.MixedScript do
  alias String.Unicode.{ScriptSet, Confusables, ScriptSetResolution, UncommonCodepoints}
  @empty ScriptSet.empty_script_set()
  empty = ScriptSet.empty_script_set()

  # this module uses the modules defined above to filter
  # confusables.txt to only allowed chars, and within those,
  # to only those sets of mutually confusable chars that aren't
  # single-script -- that is, chars that could be confusable
  # with chars in a different writing system.

  lines = get_lines.("confusables.txt")
  regex = ~r/^((?:[0-9A-F]+ )+);\t((?:[0-9A-F]+ )+);/u
  matches = Enum.map(lines, &Regex.run(regex, &1, capture: :all_but_first))

  allowed_confusables_by_prototype =
    for [one_char_str, prototype_chars_str] = ls <- matches, !is_nil(ls), reduce: %{} do
      acc ->
        confusable = String.to_integer(String.trim(one_char_str), 16)

        prototype =
          String.split(prototype_chars_str, " ", trim: true)
          |> Enum.map(&String.to_integer(&1, 16))

        # only build this up from the allowed codepoints, first.
        if UncommonCodepoints.allowed_codepoint?(confusable) do
          case Map.has_key?(acc, prototype) do
            true -> %{acc | prototype => [confusable | acc[prototype]]}
            false -> Map.put_new(acc, prototype, [confusable])
          end
        else
          acc
        end
    end

  sort_by_confusable_prototype = fn a, b ->
    Confusables.confusable_prototype(a) <= Confusables.confusable_prototype(b)
  end

  mixed =
    for {proto, ls} <- allowed_confusables_by_prototype, reduce: [] do
      acc ->
        chars =
          if 1 == length(proto) and UncommonCodepoints.allowed_codepoint?(hd(proto)),
            do: [hd(proto) | ls],
            else: ls

        # filter out confusables that aren't mixed script, example:
        #  529B '力' {Han, Han with Bopomofo, Japanese, Korean}
        #  30AB 'カ' {Japanese, Katakana}
        # They're confusable, and only 1 is Katakana. But the 'resolved
        # script set' is {Japanese}, so while confusable w/each other, (see
        # Confusables check) they're not 'mixed script confusable characters.
        {resolved, _seen} = ScriptSetResolution.resolve(chars)

        case resolved do
          ^empty -> chars ++ acc
          _ -> acc
        end
    end
    |> Enum.uniq()
    |> Enum.sort(sort_by_confusable_prototype)

  def table() do
    unquote(mixed)
  end

  # about 800 items in 'mixed' right now
  def is_potential_mixed_script_confusable_char(c) when c in unquote(mixed), do: true
  def is_potential_mixed_script_confusable_char(_), do: false

  # -- security check!

  def check_tokens(tokens) do
    # First, check for mixed script.
    # UTS 39 section 5.1, a string is 'Mixed script' if:
    # > ...its 'resolved script set' is empty, and defined to be
    # > single-script if its resolved script set is nonempty.
    # > (Note that the term “single-script string” may be confusing.
    # > It means that there is at least one script in the resolved script
    # > set, not that there is only one).
    {mixings, uniq_chars} =
      for token <- tokens, reduce: {[], []} do
        state -> add_resolution_for_token(state, token)
      end

    # If there are some, check if mixed script usage will be confusable
    if [] == mixings,
      do: [],
      else: check_confusable_mixed_script(mixings, uniq_chars)
  end

  defp add_resolution_for_token(acc, {:identifier, _, name} = token) do
    {warnings, uniq_chars} = acc
    chars = Atom.to_charlist(name)
    uniq_chars = for(c <- chars, c not in uniq_chars, do: c) ++ uniq_chars

    case ScriptSetResolution.resolve(chars) do
      {_resolved = @empty, seen} -> {[{token, chars, seen} | warnings], uniq_chars}
      _ -> {warnings, uniq_chars}
    end
  end

  defp add_resolution_for_token(acc, _token), do: acc

  @latin_scriptset ScriptSetResolution.scripts(hd('L'))

  defp check_confusable_mixed_script(mixings, uniq_chars) do
    uniq_not_confusable =
      Enum.filter(uniq_chars, &(!is_potential_mixed_script_confusable_char(&1)))

    {_resolved, scripts_to_verify} = ScriptSetResolution.resolve(uniq_chars)
    {_resolved, verified} = ScriptSetResolution.resolve(uniq_not_confusable)

    unverified =
      scripts_to_verify
      |> ScriptSet.remove(verified)
      |> ScriptSet.remove(@latin_scriptset)

    case unverified do
      @empty -> []
      _ -> warning_per_problematic_script(mixings, unverified)
    end
  end

  defp warning_per_problematic_script(mixings, unverified_scripts) do
    suspicious =
      for {token, chars, ss} <- mixings do
        {token, chars, ScriptSet.intersection(unverified_scripts, ss)}
      end

    one_example_per_scriptset =
      for {token = {_, {line, _, _}, _}, chars, bad_ss} <- suspicious, reduce: %{} do
        acc = %{^bad_ss => _already_has_one_example} ->
          acc

        acc ->
          msg = """
          The only uses of #{inspect_scripts(bad_ss)} in this file \
          are mixed-script confusables, like '#{chars}' on line #{line}:
          #{debug_resolve(chars, bad_ss, " <- mixed-script confusable")}\
          """

          Map.put_new(acc, bad_ss, {token, msg})
      end

    Map.values(one_example_per_scriptset)
  end

  # The rest of these fns support script inspection for warning message detail
  defp script_names, do: unquote(Macro.escape(script_names_sorted))
  defp script_shortnames, do: unquote(Macro.escape(script_shortnames_sorted))

  defp inspect_scripts(@empty), do: ["∅"]

  defp inspect_scripts(scriptset, naming \\ :full) do
    lookup = %{full: script_names(), short: script_shortnames()}[naming]

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

  defp debug_resolve(chars, noted_scripts, note) do
    per_char_msg =
      for c <- chars do
        hex = Integer.to_string(c, 16) |> String.pad_leading(4, "0")
        scripts = ScriptSetResolution.scripts(c)
        write_note = @empty != ScriptSet.intersection(noted_scripts, scripts)
        note = if write_note, do: note, else: ""
        " \\u#{hex} '#{[c]}' {#{inspect_scripts(scripts)}}#{note}\n"
      end

    {resolved, _} = ScriptSetResolution.resolve(chars)
    "#{per_char_msg} Resolved script set (intersection): {#{inspect_scripts(resolved)}}"
  end
end
