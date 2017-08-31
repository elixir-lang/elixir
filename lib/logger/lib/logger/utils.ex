defmodule Logger.Utils do
  @moduledoc false

  @doc """
  Truncates a `chardata` into `n` bytes.

  There is a chance we truncate in the middle of a grapheme
  cluster but we never truncate in the middle of a binary
  codepoint. For this reason, truncation is not exact.
  """
  @spec truncate(IO.chardata, non_neg_integer) :: IO.chardata
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

  defp truncate_n(int, n) when int in 0..127,                      do: {int, n - 1}
  defp truncate_n(int, n) when int in 127..0x07FF,                 do: {int, n - 2}
  defp truncate_n(int, n) when int in 0x800..0xFFFF,               do: {int, n - 3}
  defp truncate_n(int, n) when int >= 0x10000 and is_integer(int), do: {int, n - 4}

  defp truncate_n(list, n) when is_list(list) do
    truncate_n_list(list, n, [])
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
    <<prefix :: binary-size(prefix_size), suffix :: binary-size(suffix_size)>> = binary
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
  Receives a format string and arguments and replace `~p`,
  `~P`, `~w` and `~W` by its inspected variants.
  """
  def inspect(format, args, truncate, opts \\ %Inspect.Opts{})

  def inspect(format, args, truncate, opts) when is_atom(format) do
    do_inspect(Atom.to_charlist(format), args, truncate, opts)
  end

  def inspect(format, args, truncate, opts) when is_binary(format) do
    do_inspect(:binary.bin_to_list(format), args, truncate, opts)
  end

  def inspect(format, args, truncate, opts) when is_list(format) do
    do_inspect(format, args, truncate, opts)
  end

  defp do_inspect(format, [], _truncate, _opts), do: {format, []}
  defp do_inspect(format, args, truncate, opts) do
    # A pre-pass that removes binaries from
    # arguments according to the truncate limit.
    {args, _} = Enum.map_reduce(args, truncate, fn arg, acc ->
      if is_binary(arg) do
        truncate_n(arg, acc)
      else
        {arg, acc}
      end
    end)

    format
    |> :io_lib.scan_format(args)
    |> do_inspect(opts, [])
    |> :io_lib.unscan_format()
  end

  defp do_inspect([], _opts, acc),
    do: :lists.reverse(acc)
  defp do_inspect([map | t], opts, acc) when is_map(map),
    do: do_inspect(t, opts, [handle_format_map(map, opts) | acc])
  defp do_inspect([h | t], opts, acc),
    do: do_inspect(t, opts, [h | acc])

  @inspected_format_map %{
    adjust: :right,
    args: [],
    control_char: ?s,
    encoding: :unicode,
    pad_char: ?\s,
    precision: :none,
    strings: true,
    width: :none
  }

  defp handle_format_map(%{control_char: char} = map, opts) when char in 'wWpP',
    do: %{@inspected_format_map | args: [inspect_data(map, opts)]}
  defp handle_format_map(map, _opts),
    do: map

  defp inspect_data(%{strings: false} = map, opts) do
    map
    |> Map.delete(:strings)
    |> inspect_data(%{opts | charlists: :as_lists})
  end
  defp inspect_data(%{width: width} = map, opts) do
    map
    |> Map.delete(:width)
    |> inspect_data(%{opts | width: width})
  end
  defp inspect_data(%{control_char: ?W, args: [data, limit]}, opts) do
    do_inspect_data(data, %{opts | limit: limit, width: :infinity})
  end
  defp inspect_data(%{control_char: ?w, args: [data]}, opts) do
    do_inspect_data(data, %{opts | width: :infinity})
  end
  defp inspect_data(%{control_char: ?P, args: [data, limit]}, opts) do
    do_inspect_data(data, %{opts | limit: limit})
  end
  defp inspect_data(%{control_char: ?p, args: [data]}, opts) do
    do_inspect_data(data, opts)
  end

  defp do_inspect_data(data, opts) do
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
        true  -> :calendar.now_to_universal_time(now)
        false -> :calendar.now_to_local_time(now)
      end
    {date, {hours, minutes, seconds, div(micro, 1000)}}
  end
end
