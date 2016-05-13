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

  defp truncate_n(int, n) when int in 0..127,                      do: {int, n-1}
  defp truncate_n(int, n) when int in 127..0x07FF,                 do: {int, n-2}
  defp truncate_n(int, n) when int in 0x800..0xFFFF,               do: {int, n-3}
  defp truncate_n(int, n) when int >= 0x10000 and is_integer(int), do: {int, n-4}

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
    do_inspect(format, args, [], [], opts)
  end

  defp do_inspect([?~ | t], args, used_format, used_args, opts) do
    {t, args, cc_format, cc_args} = collect_cc(:width, t, args, [?~], [], opts)
    do_inspect(t, args, cc_format ++ used_format, cc_args ++ used_args, opts)
  end

  defp do_inspect([h | t], args, used_format, used_args, opts),
    do: do_inspect(t, args, [h | used_format], used_args, opts)

  defp do_inspect([], [], used_format, used_args, _opts),
    do: {:lists.reverse(used_format), :lists.reverse(used_args)}

  ## width

  defp collect_cc(:width, [?- | t], args, used_format, used_args, opts),
    do: collect_value(:width, t, args, [?- | used_format], used_args, opts, :precision)

  defp collect_cc(:width, t, args, used_format, used_args, opts),
    do: collect_value(:width, t, args, used_format, used_args, opts, :precision)

  ## precision

  defp collect_cc(:precision, [?. | t], args, used_format, used_args, opts),
    do: collect_value(:precision, t, args, [?. | used_format], used_args, opts, :pad_char)

  defp collect_cc(:precision, t, args, used_format, used_args, opts),
    do: collect_cc(:pad_char, t, args, used_format, used_args, opts)

  ## pad char

  defp collect_cc(:pad_char, [?., ?* | t], [arg | args], used_format, used_args, opts),
    do: collect_cc(:encoding, t, args, [?*, ?. | used_format], [arg | used_args], opts)

  defp collect_cc(:pad_char, [?., p | t], args, used_format, used_args, opts),
    do: collect_cc(:encoding, t, args, [p, ?. | used_format], used_args, opts)

  defp collect_cc(:pad_char, t, args, used_format, used_args, opts),
    do: collect_cc(:encoding, t, args, used_format, used_args, opts)

  ## encoding

  defp collect_cc(:encoding, [?l | t], args, used_format, used_args, opts),
    do: collect_cc(:done, t, args, [?l | used_format], used_args, %{opts | charlists: :as_lists})

  defp collect_cc(:encoding, [?t | t], args, used_format, used_args, opts),
    do: collect_cc(:done, t, args, [?t | used_format], used_args, opts)

  defp collect_cc(:encoding, t, args, used_format, used_args, opts),
    do: collect_cc(:done, t, args, used_format, used_args, opts)

  ## done

  defp collect_cc(:done, [?W | t], [data, limit | args], _used_format, _used_args, opts),
    do: collect_inspect(t, args, data, %{opts | limit: limit, width: :infinity})

  defp collect_cc(:done, [?w | t], [data | args], _used_format, _used_args, opts),
    do: collect_inspect(t, args, data, %{opts | width: :infinity})

  defp collect_cc(:done, [?P | t], [data, limit | args], _used_format, _used_args, opts),
    do: collect_inspect(t, args, data, %{opts | limit: limit})

  defp collect_cc(:done, [?p | t], [data | args], _used_format, _used_args, opts),
    do: collect_inspect(t, args, data, opts)

  defp collect_cc(:done, [h | t], args, used_format, used_args, _opts) do
    {args, used_args} = collect_cc(h, args, used_args)
    {t, args, [h | used_format], used_args}
  end

  defp collect_cc(?x, [a, prefix | args], used), do: {args, [prefix, a | used]}
  defp collect_cc(?X, [a, prefix | args], used), do: {args, [prefix, a | used]}
  defp collect_cc(?s, [a | args], used), do: {args, [a | used]}
  defp collect_cc(?e, [a | args], used), do: {args, [a | used]}
  defp collect_cc(?f, [a | args], used), do: {args, [a | used]}
  defp collect_cc(?g, [a | args], used), do: {args, [a | used]}
  defp collect_cc(?b, [a | args], used), do: {args, [a | used]}
  defp collect_cc(?B, [a | args], used), do: {args, [a | used]}
  defp collect_cc(?+, [a | args], used), do: {args, [a | used]}
  defp collect_cc(?#, [a | args], used), do: {args, [a | used]}
  defp collect_cc(?c, [a | args], used), do: {args, [a | used]}
  defp collect_cc(?i, [a | args], used), do: {args, [a | used]}
  defp collect_cc(?~, args, used), do: {args, used}
  defp collect_cc(?n, args, used), do: {args, used}

  defp collect_inspect(t, args, data, opts) do
    data =
      data
      |> Inspect.Algebra.to_doc(opts)
      |> Inspect.Algebra.format(opts.width)
    {t, args, 'st~', [data]}
  end

  defp collect_value(current, [?* | t], [arg | args], used_format, used_args, opts, next)
      when is_integer(arg) do
    collect_cc(next, t, args, [?* | used_format], [arg | used_args],
               put_value(opts, current, arg))
  end

  defp collect_value(current, [c | t], args, used_format, used_args, opts, next)
      when is_integer(c) and c >= ?0 and c <= ?9 do
    {t, c} = collect_value([c | t], [])
    collect_cc(next, t, args, c ++ used_format, used_args,
               put_value(opts, current, c |> :lists.reverse |> List.to_integer))
  end

  defp collect_value(_current, t, args, used_format, used_args, opts, next),
    do: collect_cc(next, t, args, used_format, used_args, opts)

  defp collect_value([c | t], buffer)
    when is_integer(c) and c >= ?0 and c <= ?9,
    do: collect_value(t, [c | buffer])

  defp collect_value(other, buffer),
    do: {other, buffer}

  defp put_value(opts, key, value) do
    if Map.has_key?(opts, key) do
      Map.put(opts, key, value)
    else
      opts
    end
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

  @doc """
  Formats time as chardata.
  """
  def format_time({hh, mi, ss, ms}) do
    [pad2(hh), ?:, pad2(mi), ?:, pad2(ss), ?., pad3(ms)]
  end

  @doc """
  Formats date as chardata.
  """
  def format_date({yy, mm, dd}) do
    [Integer.to_string(yy), ?-, pad2(mm), ?-, pad2(dd)]
  end

  defp pad3(int) when int < 10,  do: [?0, ?0, Integer.to_string(int)]
  defp pad3(int) when int < 100, do: [?0, Integer.to_string(int)]
  defp pad3(int), do: Integer.to_string(int)

  defp pad2(int) when int < 10, do: [?0, Integer.to_string(int)]
  defp pad2(int), do: Integer.to_string(int)
end
