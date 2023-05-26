defmodule Logger.Utils do
  @moduledoc false

  @doc """
  A filter for default translation and handling of reports.
  """
  def translator(%{domain: [:elixir | _]}, %{otp: false}), do: :stop
  def translator(%{meta: %{domain: [:otp, :sasl | _]}}, %{sasl: false}), do: :stop
  def translator(%{meta: %{domain: [:supervisor_report | _]}}, %{sasl: false}), do: :stop
  def translator(%{msg: {:string, _}}, _config), do: :ignore

  def translator(%{msg: msg, level: level} = event, %{translators: translators}) do
    %{level: min_level} = :logger.get_primary_config()

    try do
      case msg do
        {:report, %{label: label, report: report} = complete}
        when map_size(complete) == 2 ->
          translate(translators, min_level, level, :report, {label, report})

        {:report, %{label: {:error_logger, _}, format: format, args: args}} ->
          translate(translators, min_level, level, :format, {format, args})

        {:report, report} ->
          translate(translators, min_level, level, :report, {:logger, report})

        {format, args} ->
          translate(translators, min_level, level, :format, {format, args})
      end
    rescue
      e ->
        chardata = [
          "Failure while translating Erlang's logger event\n",
          Exception.format(:error, e, __STACKTRACE__)
        ]

        %{event | msg: {:string, chardata}}
    else
      :none -> :ignore
      :skip -> :stop
      {:ok, chardata} -> %{event | msg: {:string, chardata}}
      {:ok, char, meta} -> %{event | msg: {:string, char}, meta: Enum.into(meta, event.meta)}
    end
  end

  defp translate([{mod, fun} | t], min_level, level, kind, data) do
    with :none <- apply(mod, fun, [min_level, level, kind, data]) do
      translate(t, min_level, level, kind, data)
    end
  end

  defp translate([], _min_level, _level, _kind, _data) do
    :none
  end

  @doc """
  A filter for removing logs if current process opted out of certain levels.
  """
  def process_level(%{level: level}, _extra) do
    process_level = Logger.get_process_level(self())

    if process_level != nil and :logger.compare_levels(level, process_level) == :lt do
      :stop
    else
      :ignore
    end
  end

  @doc """
  A filter for logger exits which then removes itself.
  """
  def silence_logger_exit(
        %{
          msg:
            {:report,
             %{
               label: {:application_controller, :exit},
               report: [application: :logger, exited: :stopped] ++ _
             }}
        },
        _extra
      ) do
    :logger.remove_primary_filter(:silence_logger_exit)
    :stop
  end

  def silence_logger_exit(_message, _extra) do
    :ignore
  end

  @doc """
  Receives a format string and arguments, scans them, and then replace `~p`,
  `~P`, `~w` and `~W` by its inspected variants.

  For information about format scanning and how to consume them,
  check `:io_lib.scan_format/2`.
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
    width: :none,
    maps_order: :undefined
  }

  defp handle_format_spec(%{control_char: char} = spec, opts) when char in ~c"wWpP" do
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

  defp inspect_limit(char, [_, limit], _) when char in ~c"WP", do: limit
  defp inspect_limit(_, _, opts), do: opts.limit

  defp inspect_width(char, _) when char in ~c"wW", do: :infinity
  defp inspect_width(_, width), do: width

  defp inspect_data([data | _], opts) do
    width = if opts.width == :none, do: 80, else: opts.width

    data
    |> Inspect.Algebra.to_doc(opts)
    |> Inspect.Algebra.format(width)
  end

  @doc """
  Truncates `n` elements from chartdata.
  """
  def truncate_n(_, n) when n < 0 do
    {"", n}
  end

  def truncate_n(binary, n) when is_binary(binary) do
    remaining = n - byte_size(binary)

    if remaining < 0 do
      # There is a chance we are cutting at the wrong
      # place so we need to fix the binary.
      {fix_binary(binary_part(binary, 0, n)), remaining}
    else
      {binary, remaining}
    end
  end

  def truncate_n(int, n) when int in 0..127, do: {int, n - 1}
  def truncate_n(int, n) when int in 127..0x07FF, do: {int, n - 2}
  def truncate_n(int, n) when int in 0x800..0xFFFF, do: {int, n - 3}
  def truncate_n(int, n) when int >= 0x10000 and is_integer(int), do: {int, n - 4}

  def truncate_n(list, n) when is_list(list) do
    truncate_n_list(list, n, [])
  end

  def truncate_n(other, _n) do
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
    # This should allow at least two code points of 6 bytes.
    suffix_size = min(byte_size(binary), 13)
    prefix_size = byte_size(binary) - suffix_size
    <<prefix::binary-size(^prefix_size), suffix::binary-size(^suffix_size)>> = binary
    prefix <> fix_binary(suffix, "")
  end

  defp fix_binary(<<h::utf8, t::binary>>, acc) do
    acc <> <<h::utf8>> <> fix_binary(t, "")
  end

  defp fix_binary(<<h, t::binary>>, acc) do
    fix_binary(t, <<acc::binary, h>>)
  end

  defp fix_binary(<<>>, _acc) do
    <<>>
  end
end
