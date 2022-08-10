defmodule String.Tokenizer do
  @moduledoc false

  ## Custom normalization definitions
  #
  # These codepoints will be normalized from => to, and their
  # scriptset will be the union of both. If one of the two
  # codepoints is Script='Common|Inherited', this means both
  # codepoints can be used anywhere without unsafe script mixing;
  # similarly, they are exempted from the Restricted list.
  #
  start_normalizations = %{
    # NFKC-based automatic normalizations
    # U+00B5 => U+03BC
    ?µ => ?μ
  }

  normalizations = start_normalizations

  ##
  ## First let's load all characters that we will allow in identifiers
  ##

  range_to_codepoints = fn range ->
    case :binary.split(String.trim(range), "..") do
      [a] -> [String.to_integer(a, 16)]
      [a, b] -> Enum.to_list(String.to_integer(a, 16)..String.to_integer(b, 16))
    end
  end

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
      [range | category] = :binary.split(line, ";")

      pos =
        case category do
          [" Other_ID_Start" <> _] -> 0
          [" Other_ID_Continue" <> _] -> 1
          [" Pattern_White_Space" <> _] -> 2
          [" Pattern_Syntax" <> _] -> 2
          _ -> -1
        end

      if pos >= 0 do
        put_elem(acc, pos, range_to_codepoints.(range) ++ elem(acc, pos))
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
      with [range, type_with_comments] <- :binary.split(line, ";"),
           [types, _comments] <- :binary.split(type_with_comments, "#"),
           types = String.split(types, " ", trim: true),
           false <- "Inclusion" in types or "Recommended" in types do
        range_to_codepoints.(range)
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
      with [range, scripts_with_comments] <- :binary.split(line, ";"),
           [scripts, _comments] <- :binary.split(scripts_with_comments, "#"),
           scripts =
             scripts |> String.split(" ", trim: true) |> Enum.map(&Map.get(aliases, &1, &1)) do
        for codepoint <- range_to_codepoints.(range),
            Map.has_key?(unicode_all, codepoint) and
              "Common" not in scripts and "Inherited" not in scripts,
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

  # Add our custom normalizations

  codepoints_to_mask =
    for {from, to} <- normalizations, reduce: codepoints_to_mask do
      acc ->
        ss = ScriptSet.union(Map.get(acc, from, top), Map.get(acc, to, top))
        Map.put(acc, to, ss)
    end

  ##
  ## Define functions and module attributes to access characters and their scriptsets
  ##

  # bottom of bitmap == all bits are 0, no scripts in the scriptset
  @bottom bottom
  @latin 1
  # top of bitmap (all bits are 1) is ALL in UTS39 ('Common', 'Inherited');
  # a scriptset that will intersect with other all non-empty scriptsets
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

  # Hardcoded normalizations. Also split by upper, start, continue.

  for {from, to} <- start_normalizations do
    mask = Map.fetch!(codepoints_to_mask, to)
    defp normalize_start(unquote(from)), do: {unquote(to), unquote(mask)}
  end

  defp normalize_start(_codepoint), do: @bottom

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
              @bottom ->
                case normalize_start(head) do
                  @bottom ->
                    {:error, :empty}

                  {head, scriptset} ->
                    validate(continue(tail, [head], 1, false, scriptset, [:nfkc]), :identifier)
                end

              scriptset ->
                validate(continue(tail, [head], 1, false, scriptset, []), :identifier)
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
    {[?! | acc], tail, length + 1, ascii_letters?, scriptset, [:punctuation | special]}
  end

  defp continue([?? | tail], acc, length, ascii_letters?, scriptset, special) do
    {[?? | acc], tail, length + 1, ascii_letters?, scriptset, [:punctuation | special]}
  end

  defp continue([?@ | tail], acc, length, ascii_letters?, scriptset, special) do
    special = [:at | List.delete(special, :at)]
    continue(tail, [?@ | acc], length + 1, ascii_letters?, scriptset, special)
  end

  defp continue([head | tail] = list, acc, length, ascii_letters?, scriptset, special) do
    cond do
      ascii_lower?(head) or ascii_upper?(head) ->
        continue(tail, [head | acc], length + 1, ascii_letters?, ss_latin(scriptset), special)

      head == ?_ or ascii_continue?(head) ->
        continue(tail, [head | acc], length + 1, ascii_letters?, scriptset, special)

      # Pattern is used for performance and to not mark ascii tokens as unicode
      # ' \\\t\n\r!"#$%&\'()*+,-./:;<=>?@[]^`{|}~'
      head <= 127 ->
        {acc, list, length, ascii_letters?, scriptset, special}

      true ->
        with @bottom <- unicode_start(head),
             @bottom <- unicode_upper(head),
             @bottom <- unicode_continue(head) do
          case normalize_start(head) do
            @bottom ->
              {:error, {:unexpected_token, :lists.reverse([head | acc])}}

            {head, ss} ->
              ss = ss_intersect(scriptset, ss)
              special = [:nfkc | List.delete(special, :nfkc)]
              continue(tail, [head | acc], length + 1, false, ss, special)
          end
        else
          ss ->
            ss = ss_intersect(scriptset, ss)
            continue(tail, [head | acc], length + 1, false, ss, special)
        end
    end
  end

  defp continue([], acc, length, ascii_letters?, scriptset, special) do
    {acc, [], length, ascii_letters?, scriptset, special}
  end

  defp validate({:error, _} = error, _kind) do
    error
  end

  defp validate({acc, rest, length, true, _scriptset, special}, kind) do
    {kind, :lists.reverse(acc), rest, length, true, special}
  end

  defp validate({original_acc, rest, length, false, scriptset, special}, kind) do
    original_acc = :lists.reverse(original_acc)
    acc = :unicode.characters_to_nfc_list(original_acc)

    special =
      if original_acc == acc do
        special
      else
        [:nfkc | List.delete(special, :nfkc)]
      end

    if scriptset != @bottom or highly_restrictive?(acc) do
      {kind, acc, rest, length, false, special}
    else
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

          hex = :io_lib.format(~c"~4.16.0B", [codepoint])
          "  \\u#{hex} #{[codepoint]}#{scriptsets}\n"
        end

      prefix = ~c"invalid mixed-script identifier found: "

      suffix = ~c"""


      Mixed-script identifiers are not supported for security reasons. \
      '#{acc}' is made of the following scripts:\n
      #{breakdown}
      All characters in the identifier should resolve to a single script, \
      or use a highly restrictive set of scripts.
      """

      {:error, {:not_highly_restrictive, acc, {prefix, suffix}}}
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
