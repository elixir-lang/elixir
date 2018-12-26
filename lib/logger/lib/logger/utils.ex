defmodule Logger.Utils do
  @moduledoc false

  @doc """
  Truncates a `chardata` into `n` bytes.

  There is a chance we truncate in the middle of a grapheme
  cluster but we never truncate in the middle of a binary
  codepoint. For this reason, truncation is not exact.
  """
  @spec truncate(IO.chardata(), non_neg_integer) :: IO.chardata()
  def truncate(chardata, :infinity) when is_binary(chardata) or is_list(chardata) do
    chardata
  end

  def truncate(chardata, n) when n >= 0 do
    {chardata, n} = truncate_n(chardata, n)
    if n >= 0, do: chardata, else: [chardata, " (truncated)"]
  end

  defp truncate_n(_, n) when n < 0 do
    {"", n}
  end

  defp truncate_n(binary, n) when is_binary(binary) do
    remaining = n - byte_size(binary)

    if remaining < 0 do
      # There is a chance we are cutting at the wrong
      # place so we need to fix the binary.
      {fix_binary(binary_part(binary, 0, n)), remaining}
    else
      {binary, remaining}
    end
  end

  defp truncate_n(int, n) when int in 0..127, do: {int, n - 1}
  defp truncate_n(int, n) when int in 127..0x07FF, do: {int, n - 2}
  defp truncate_n(int, n) when int in 0x800..0xFFFF, do: {int, n - 3}
  defp truncate_n(int, n) when int >= 0x10000 and is_integer(int), do: {int, n - 4}

  defp truncate_n(list, n) when is_list(list) do
    truncate_n_list(list, n, [])
  end

  defp truncate_n(other, _n) do
    raise ArgumentError,
          "cannot truncate chardata because it contains something that is not " <>
            "valid chardata: #{inspect(other)}"
  end

  defp truncate_n_list(_, n, acc) when n < 0 do
    {:lists.reverse(acc), n}
  end

  defp truncate_n_list([h | t], n, acc) do
    {h, n} = truncate_n(h, n)
    truncate_n_list(t, n, [h | acc])
  end

  defp truncate_n_list([], n, acc) do
    {:lists.reverse(acc), n}
  end

  defp truncate_n_list(t, n, acc) do
    {t, n} = truncate_n(t, n)
    {:lists.reverse(acc, t), n}
  end

  defp fix_binary(binary) do
    # Use a thirteen-bytes offset to look back in the binary.
    # This should allow at least two codepoints of 6 bytes.
    suffix_size = min(byte_size(binary), 13)
    prefix_size = byte_size(binary) - suffix_size
    <<prefix::binary-size(prefix_size), suffix::binary-size(suffix_size)>> = binary
    prefix <> fix_binary(suffix, "")
  end

  defp fix_binary(<<h::utf8, t::binary>>, acc) do
    acc <> <<h::utf8>> <> fix_binary(t, "")
  end

  defp fix_binary(<<h, t::binary>>, acc) do
    fix_binary(t, <<h, acc::binary>>)
  end

  defp fix_binary(<<>>, _acc) do
    <<>>
  end

  @doc """
  Receives a format string and arguments, scans them, and then replace `~p`,
  `~P`, `~w` and `~W` by its inspected variants.

  For information about format scanning and how to consume them,
  check `:io_lib.scan_format/2`
  """
  def scan_inspect(format, args, truncate, opts \\ %Inspect.Opts{})

  def scan_inspect(format, args, truncate, opts) when is_atom(format) do
    scan_inspect(Atom.to_charlist(format), args, truncate, opts)
  end

  def scan_inspect(format, args, truncate, opts) when is_binary(format) do
    scan_inspect(:binary.bin_to_list(format), args, truncate, opts)
  end

  def scan_inspect(format, [], _truncate, _opts) when is_list(format) do
    :io_lib.scan_format(format, [])
  end

  def scan_inspect(format, args, truncate, opts) when is_list(format) do
    # A pre-pass that removes binaries from
    # arguments according to the truncate limit.
    {args, _} =
      Enum.map_reduce(args, truncate, fn arg, acc ->
        if is_binary(arg) and acc != :infinity do
          truncate_n(arg, acc)
        else
          {arg, acc}
        end
      end)

    format
    |> :io_lib.scan_format(args)
    |> Enum.map(&handle_format_spec(&1, opts))
  end

  @inspected_format_spec %{
    adjust: :right,
    args: [],
    control_char: ?s,
    encoding: :unicode,
    pad_char: ?\s,
    precision: :none,
    strings: true,
    width: :none
  }

  defp handle_format_spec(%{control_char: char} = spec, opts) when char in 'wWpP' do
    %{args: args, width: width, strings: strings?} = spec

    opts = %{
      opts
      | charlists: inspect_charlists(strings?, opts),
        limit: inspect_limit(char, args, opts),
        width: inspect_width(char, width)
    }

    %{@inspected_format_spec | args: [inspect_data(args, opts)]}
  end

  defp handle_format_spec(spec, _opts), do: spec

  defp inspect_charlists(false, _), do: :as_lists
  defp inspect_charlists(_, opts), do: opts.charlists

  defp inspect_limit(char, [_, limit], _) when char in 'WP', do: limit
  defp inspect_limit(_, _, opts), do: opts.limit

  defp inspect_width(char, _) when char in 'wW', do: :infinity
  defp inspect_width(_, width), do: width

  defp inspect_data([data | _], opts) do
    data
    |> Inspect.Algebra.to_doc(opts)
    |> Inspect.Algebra.format(opts.width)
  end

  @doc """
  Returns a timestamp that includes milliseconds.
  """
  def timestamp(utc_log?) do
    {_, _, micro} = now = :os.timestamp()

    {date, {hours, minutes, seconds}} =
      case utc_log? do
        true -> :calendar.now_to_universal_time(now)
        false -> :calendar.now_to_local_time(now)
      end

    {date, {hours, minutes, seconds, div(micro, 1000)}}
  end
end
