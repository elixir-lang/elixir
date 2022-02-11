defmodule String.Tokenizer do
  @moduledoc false

  ##
  ## First let's load all characters that we will allow in identifiers
  ##

  {letter_uptitlecase, start, continue, _} =
    Path.join(__DIR__, "UnicodeData.txt")
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

  # Each character is classified accordingly

  {start, continue, patterns} =
    Path.join(__DIR__, "PropList.txt")
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

  # Also restrict characters for security purposes according to UTS 39

  restricted =
    Path.join(__DIR__, "IdentifierType.txt")
    |> File.read!()
    |> String.split(["\r\n", "\n"], trim: true)
    |> Enum.flat_map(fn line ->
      with [codepoints, type_with_comments] <- :binary.split(line, ";"),
           [types, _comments] <- :binary.split(type_with_comments, "#"),
           types = String.split(types, " ", trim: true),
           true <- "Inclusion" not in types and "Recommended" not in types do
        case :binary.split(String.trim(codepoints), "..") do
          [a] -> [String.to_integer(a, 16)]
          [a, b] -> Enum.to_list(String.to_integer(a, 16)..String.to_integer(b, 16))
        end
      else
        _ -> []
      end
    end)

  id_upper = (letter_uptitlecase -- patterns) -- restricted
  id_start = (start -- patterns) -- restricted
  id_continue = (continue -- patterns) -- restricted

  unicode_upper = Enum.filter(id_upper, &(&1 > 127))
  unicode_start = Enum.filter(id_start, &(&1 > 127))
  unicode_continue = Enum.filter(id_continue, &(&1 > 127))

  unicode_all = Map.from_keys(unicode_upper ++ unicode_start ++ unicode_continue, [])
  IO.puts(:stderr, "[Unicode] Tokenizing #{map_size(unicode_all)} non-ascii codepoints")

  ##
  ## Compute scriptsets for all characters above
  ##

  # 3 text files from UAX24 (Scripts):
  #
  # 1. Scripts.txt               codepoint => primary script (by full name)
  # 2. ScriptExtensions.txt      codepoint => N scripts, (by short names)
  # 3. PropertyValueAliases.txt  short names <=> long names mapping

  # First we'll build a lookup of short <=> long names, starting with
  # names that we will make part of the highly restricted set later.
  script_aliases =
    Path.join(__DIR__, "PropertyValueAliases.txt")
    |> File.read!()
    |> String.split(["\r\n", "\n"], trim: true)
    |> Enum.flat_map(fn line ->
      case String.split(line, [";", " "], trim: true) do
        ["sc", short, long | _] -> [{short, long}]
        _ -> []
      end
    end)
    |> Map.new()

  # Now we will compute all used scriptsets as well as
  # a mapping from codepoint to scriptsets.
  codepoints_to_scriptset = fn file, aliases ->
    Path.join(__DIR__, file)
    |> File.read!()
    |> String.split(["\r\n", "\n"], trim: true)
    |> Enum.flat_map(fn line ->
      with [codepoints, scripts_with_comments] <- :binary.split(line, ";"),
           [scripts, _comments] <- :binary.split(scripts_with_comments, "#"),
           scripts =
             scripts |> String.split(" ", trim: true) |> Enum.map(&Map.get(aliases, &1, &1)),
           true <- "Common" not in scripts and "Inherited" not in scripts do
        codepoints =
          case :binary.split(String.trim(codepoints), "..") do
            [a] -> [String.to_integer(a, 16)]
            [a, b] -> Enum.to_list(String.to_integer(a, 16)..String.to_integer(b, 16))
          end

        for codepoint <- codepoints,
            Map.has_key?(unicode_all, codepoint),
            do: {codepoint, scripts}
      else
        _ -> []
      end
    end)
  end

  scripts = codepoints_to_scriptset.("Scripts.txt", %{})
  script_extensions = codepoints_to_scriptset.("ScriptExtensions.txt", script_aliases)
  all_codepoints_to_scriptset = scripts ++ script_extensions

  all_scriptsets =
    all_codepoints_to_scriptset
    |> Enum.flat_map(&elem(&1, 1))
    |> Enum.uniq()
    |> then(&(["Han with Bopomofo", "Japanese", "Korean"] ++ &1))

  # We will represent scriptsets using a bitmap. So let's define
  # a separate module for said operations. We will also sort the
  # scriptsets and make Latin the first one for convenience.

  defmodule ScriptSet do
    @moduledoc false
    def from_index(idx), do: :erlang.bsl(1, idx)
    def lattices(size), do: {0, trunc(:math.pow(2, size)) - 1}
    def union(left, right), do: :erlang.bor(left, right)

    def to_indexes(set) do
      for {?1, index} <- set |> Integer.to_charlist(2) |> Enum.reverse() |> Enum.with_index() do
        index
      end
    end
  end

  sorted_scriptsets = ["Latin" | all_scriptsets |> List.delete("Latin") |> Enum.sort()]

  scriptset_masks =
    sorted_scriptsets
    |> Enum.with_index(fn scriptset, index ->
      {scriptset, ScriptSet.from_index(index)}
    end)
    |> Map.new()

  # Some scriptsets must be augmented according to the rules below
  augmentation_rules = %{
    "Han" => ["Han with Bopomofo", "Japanese", "Korean"],
    "Hiragana" => ["Japanese"],
    "Katakana" => ["Japanese"],
    "Hangul" => ["Korean"],
    "Bopomofo" => ["Han with Bopomofo"]
  }

  scriptset_masks =
    for {key, additions} <- augmentation_rules, reduce: scriptset_masks do
      acc ->
        Map.update!(acc, key, fn value ->
          additions
          |> Enum.map(&Map.fetch!(acc, &1))
          |> Enum.reduce(value, &ScriptSet.union/2)
        end)
    end

  {bottom, top} = ScriptSet.lattices(map_size(scriptset_masks))
  IO.puts(:stderr, "[Unicode] Tokenizing #{map_size(scriptset_masks)} scriptsets")

  codepoints_to_mask =
    for {codepoint, scriptsets} <- all_codepoints_to_scriptset, into: %{} do
      {codepoint,
       scriptsets
       |> Enum.map(&Map.fetch!(scriptset_masks, &1))
       |> Enum.reduce(bottom, &ScriptSet.union/2)}
    end

  ##
  ## Define functions and module attributes to access characters and their scriptsets
  ##

  @bottom bottom
  @latin 1
  @top top
  @indexed_scriptsets sorted_scriptsets |> Enum.with_index(&{&2, &1}) |> Map.new()

  latin = Map.fetch!(scriptset_masks, "Latin")

  @highly_restrictive [
    ScriptSet.union(latin, Map.fetch!(scriptset_masks, "Japanese")),
    ScriptSet.union(latin, Map.fetch!(scriptset_masks, "Han with Bopomofo")),
    ScriptSet.union(latin, Map.fetch!(scriptset_masks, "Korean"))
  ]

  # ScriptSet helpers. Inline instead of dispatching to ScriptSet for performance

  @compile {:inline, ss_latin: 1, ss_intersect: 2}
  defp ss_latin(ss), do: :erlang.band(ss, @latin)
  defp ss_intersect(left, right), do: :erlang.band(left, right)

  # Ascii helpers

  @compile {:inline, ascii_upper?: 1, ascii_lower?: 1, ascii_continue?: 1}
  defp ascii_upper?(entry), do: entry >= ?A and entry <= ?Z
  defp ascii_lower?(entry), do: entry >= ?a and entry <= ?z
  defp ascii_continue?(entry), do: entry >= ?0 and entry <= ?9

  # Pattern is used as a performance check to end sooner before traversing unicode
  for pattern <- ' \t\n\r!"#$%&\'()*+,-./:;<=>?@[]^`{|}~' do
    defp ascii_pattern?(unquote(pattern)), do: true
  end

  defp ascii_pattern?(_), do: false

  # Unicode helpers
  # We use ranges whenever possible to reduce bytecode size.

  unicode_upper = Enum.map(unicode_upper, &{&1, Map.get(codepoints_to_mask, &1, top)})
  unicode_start = Enum.map(unicode_start, &{&1, Map.get(codepoints_to_mask, &1, top)})
  unicode_continue = Enum.map(unicode_continue, &{&1, Map.get(codepoints_to_mask, &1, top)})

  rangify = fn [{head, scriptset} | tail] ->
    {first, last, scriptset, acc} =
      Enum.reduce(tail, {head, head, scriptset, []}, fn
        {number, scriptset}, {first, last, scriptset, acc} when number == first - 1 ->
          {number, last, scriptset, acc}

        {number, scriptset}, {first, last, range_scriptset, acc} ->
          {number, number, scriptset, [{first, last, range_scriptset} | acc]}
      end)

    [{first, last, scriptset} | acc]
  end

  for {first, last, scriptset} <- rangify.(unicode_upper) do
    if first == last do
      defp unicode_upper(unquote(first)), do: unquote(scriptset)
    else
      defp unicode_upper(entry) when entry in unquote(first)..unquote(last),
        do: unquote(scriptset)
    end
  end

  defp unicode_upper(_), do: @bottom

  for {first, last, scriptset} <- rangify.(unicode_start) do
    if first == last do
      defp unicode_start(unquote(first)), do: unquote(scriptset)
    else
      defp unicode_start(entry) when entry in unquote(first)..unquote(last),
        do: unquote(scriptset)
    end
  end

  defp unicode_start(_), do: @bottom

  for {first, last, scriptset} <- rangify.(unicode_continue) do
    if first == last do
      defp unicode_continue(unquote(first)), do: unquote(scriptset)
    else
      defp unicode_continue(entry) when entry in unquote(first)..unquote(last),
        do: unquote(scriptset)
    end
  end

  defp unicode_continue(_), do: @bottom

  ##
  ## Now we are ready to tokenize!
  ##

  def tokenize([head | tail]) do
    cond do
      ascii_upper?(head) ->
        validate(continue(tail, [head], 1, true, @latin, []), :alias)

      ascii_lower?(head) ->
        validate(continue(tail, [head], 1, true, @latin, []), :identifier)

      head == ?_ ->
        validate(continue(tail, [head], 1, true, @top, []), :identifier)

      true ->
        case unicode_upper(head) do
          @bottom ->
            case unicode_start(head) do
              @bottom -> {:error, :empty}
              scriptset -> validate(continue(tail, [head], 1, false, scriptset, []), :identifier)
            end

          scriptset ->
            validate(continue(tail, [head], 1, false, scriptset, []), :atom)
        end
    end
  end

  def tokenize([]) do
    {:error, :empty}
  end

  defp continue([?! | tail], acc, length, ascii_letters?, scriptset, special) do
    {[?! | acc], tail, length + 1, ascii_letters?, scriptset, [?! | special]}
  end

  defp continue([?? | tail], acc, length, ascii_letters?, scriptset, special) do
    {[?? | acc], tail, length + 1, ascii_letters?, scriptset, [?? | special]}
  end

  defp continue([?@ | tail], acc, length, ascii_letters?, scriptset, special) do
    special = [?@ | List.delete(special, ?@)]
    continue(tail, [?@ | acc], length + 1, ascii_letters?, scriptset, special)
  end

  defp continue([head | tail] = list, acc, length, ascii_letters?, scriptset, special) do
    cond do
      ascii_lower?(head) or ascii_upper?(head) ->
        continue(tail, [head | acc], length + 1, ascii_letters?, ss_latin(scriptset), special)

      head == ?_ or ascii_continue?(head) ->
        continue(tail, [head | acc], length + 1, ascii_letters?, scriptset, special)

      ascii_pattern?(head) ->
        {acc, list, length, ascii_letters?, scriptset, special}

      true ->
        with @bottom <- unicode_start(head),
             @bottom <- unicode_upper(head),
             @bottom <- unicode_continue(head) do
          {acc, list, length, ascii_letters?, scriptset, special}
        else
          ss ->
            continue(tail, [head | acc], length + 1, false, ss_intersect(scriptset, ss), special)
        end
    end
  end

  defp continue([], acc, length, ascii_letters?, scriptset, special) do
    {acc, [], length, ascii_letters?, scriptset, special}
  end

  defp validate({acc, rest, length, ascii_letters?, scriptset, special}, kind) do
    acc = :lists.reverse(acc)

    cond do
      ascii_letters? ->
        {kind, acc, rest, length, ascii_letters?, special}

      :unicode.characters_to_nfc_list(acc) != acc ->
        {:error, {:not_nfc, acc}}

      scriptset == @bottom and not highly_restrictive?(acc) ->
        breakdown =
          for codepoint <- acc do
            scriptsets =
              case codepoint_to_scriptset(codepoint) do
                @top ->
                  ""

                scriptset ->
                  scriptset
                  |> ScriptSet.to_indexes()
                  |> Enum.map(&Map.fetch!(@indexed_scriptsets, &1))
                  |> then(&(" {" <> Enum.join(&1, ",") <> "}"))
              end

            hex = :io_lib.format('~4.16.0B', [codepoint])
            "  \\u#{hex} #{[codepoint]}#{scriptsets}\n"
          end

        prefix = 'invalid mixed-script identifier found: '

        suffix = '''


        Mixed-script identifiers are not supported for security reasons. \
        '#{acc}' is made of the following scripts:\n
        #{breakdown}
        Make sure all characters in the identifier resolve to a single script or a highly
        restrictive script. See https://hexdocs.pm/elixir/unicode-syntax.html for more information.
        '''

        {:error, {:not_highly_restrictive, acc, {prefix, suffix}}}

      true ->
        {kind, acc, rest, length, ascii_letters?, special}
    end
  end

  defp highly_restrictive?(acc) do
    scriptsets = Enum.map(acc, &codepoint_to_scriptset/1)

    # 'A set of scripts is defined to cover a string if the intersection of
    #  that set with the augmented script sets of all characters in the string
    #  is nonempty; in other words, if every character in the string shares at
    #  least one script with the cover set.'
    Enum.any?(@highly_restrictive, fn restrictive ->
      Enum.all?(scriptsets, &(ss_intersect(&1, restrictive) != @bottom))
    end)
  end

  defp codepoint_to_scriptset(head) do
    cond do
      ascii_lower?(head) or ascii_upper?(head) ->
        @latin

      head == ?_ or ascii_continue?(head) ->
        @top

      true ->
        with @bottom <- unicode_start(head),
             @bottom <- unicode_upper(head),
             @bottom <- unicode_continue(head),
             do: @top
    end
  end
end
