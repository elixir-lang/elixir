defmodule File.Stream do
  @moduledoc """
  Defines a `File.Stream` struct returned by `File.stream!/3`.

  The following fields are public:

    * `path`          - the file path
    * `modes`         - the file modes
    * `raw`           - a boolean indicating if bin functions should be used
    * `line_or_bytes` - if reading should read lines or a given number of bytes
    * `node`          - the node the file belongs to

  """

  defstruct path: nil, modes: [], line_or_bytes: :line, raw: true, node: nil

  @type t :: %__MODULE__{}

  @doc false
  def __build__(path, line_or_bytes, modes) do
    with {:read_offset, offset} <- :lists.keyfind(:read_offset, 1, modes),
         false <- is_integer(offset) and offset >= 0 do
      raise ArgumentError,
            "expected :read_offset to be a non-negative integer, got: #{inspect(offset)}"
    end

    raw = :lists.keyfind(:encoding, 1, modes) == false

    modes =
      case raw do
        true ->
          case :lists.keyfind(:read_ahead, 1, modes) do
            {:read_ahead, false} -> [:raw | :lists.keydelete(:read_ahead, 1, modes)]
            {:read_ahead, _} -> [:raw | modes]
            false -> [:raw, :read_ahead | modes]
          end

        false ->
          modes
      end

    %File.Stream{path: path, modes: modes, raw: raw, line_or_bytes: line_or_bytes, node: node()}
  end

  @doc false
  def __open__(%File.Stream{path: path, node: node}, modes) when node == node() do
    :file.open(path, modes)
  end

  @doc false
  def __open__(%File.Stream{path: path, node: node}, modes) do
    :erpc.call(node, :file_io_server, :start, [self(), path, List.delete(modes, :raw)])
  end

  defimpl Collectable do
    def into(%{modes: modes, raw: raw} = stream) do
      modes = for mode <- modes, mode not in [:read], do: mode

      case File.Stream.__open__(stream, [:write | modes]) do
        {:ok, device} ->
          {:ok, into(device, stream, raw)}

        {:error, reason} ->
          raise File.Error, reason: reason, action: "stream", path: stream.path
      end
    end

    defp into(device, stream, raw) do
      fn
        :ok, {:cont, x} ->
          case raw do
            true -> IO.binwrite(device, x)
            false -> IO.write(device, x)
          end

        :ok, :done ->
          # If delayed_write option is used and the last write failed will
          # MatchError here as {:error, _} is returned.
          :ok = :file.close(device)
          stream

        :ok, :halt ->
          # If delayed_write option is used and the last write failed will
          # MatchError here as {:error, _} is returned.
          :ok = :file.close(device)
      end
    end
  end

  defimpl Enumerable do
    @read_ahead_size 64 * 1024

    def reduce(%{modes: modes, line_or_bytes: line_or_bytes, raw: raw} = stream, acc, fun) do
      start_fun = fn ->
        case File.Stream.__open__(stream, read_modes(modes)) do
          {:ok, device} ->
            skip_bom_and_offset(device, raw, modes)

          {:error, reason} ->
            raise File.Error, reason: reason, action: "stream", path: stream.path
        end
      end

      next_fun =
        case raw do
          true -> &IO.each_binstream(&1, line_or_bytes)
          false -> &IO.each_stream(&1, line_or_bytes)
        end

      Stream.resource(start_fun, next_fun, &:file.close/1).(acc, fun)
    end

    def count(%{modes: modes, line_or_bytes: :line, path: path, raw: raw} = stream) do
      pattern = :binary.compile_pattern("\n")

      counter = fn device ->
        device = skip_bom_and_offset(device, raw, modes)
        count_lines(device, path, pattern, read_function(stream), 0)
      end

      {:ok, open!(stream, modes, counter)}
    end

    def count(%{path: path, line_or_bytes: bytes, raw: true, modes: modes, node: node} = stream) do
      case :erpc.call(node, File, :stat, [path]) do
        {:ok, %{size: 0}} ->
          {:error, __MODULE__}

        {:ok, %{size: size}} ->
          bom_offset = count_raw_bom(stream, modes)
          offset = get_read_offset(modes)
          size = max(size - bom_offset - offset, 0)
          remainder = if rem(size, bytes) == 0, do: 0, else: 1
          {:ok, div(size, bytes) + remainder}

        {:error, reason} ->
          raise File.Error, reason: reason, action: "stream", path: path
      end
    end

    def count(_stream) do
      {:error, __MODULE__}
    end

    def member?(_stream, _term) do
      {:error, __MODULE__}
    end

    def slice(_stream) do
      {:error, __MODULE__}
    end

    defp open!(stream, modes, fun) do
      case File.Stream.__open__(stream, read_modes(modes)) do
        {:ok, device} ->
          try do
            fun.(device)
          after
            :file.close(device)
          end

        {:error, reason} ->
          raise File.Error, reason: reason, action: "stream", path: stream.path
      end
    end

    defp count_raw_bom(stream, modes) do
      if :trim_bom in modes do
        open!(stream, read_modes(modes), &(&1 |> trim_bom(true) |> elem(1)))
      else
        0
      end
    end

    defp skip_bom_and_offset(device, raw, modes) do
      device =
        if :trim_bom in modes do
          device |> trim_bom(raw) |> elem(0)
        else
          device
        end

      offset = get_read_offset(modes)

      if offset > 0 do
        {:ok, _} = :file.position(device, {:cur, offset})
      end

      device
    end

    defp trim_bom(device, true) do
      bom_length = device |> IO.binread(4) |> bom_length()
      {:ok, new_pos} = :file.position(device, bom_length)
      {device, new_pos}
    end

    defp trim_bom(device, false) do
      # Or we read the bom in the correct amount or it isn't there
      case bom_length(IO.read(device, 1)) do
        0 ->
          {:ok, _} = :file.position(device, 0)
          {device, 0}

        _ ->
          {device, 1}
      end
    end

    defp bom_length(<<239, 187, 191, _rest::binary>>), do: 3
    defp bom_length(<<254, 255, _rest::binary>>), do: 2
    defp bom_length(<<255, 254, _rest::binary>>), do: 2
    defp bom_length(<<0, 0, 254, 255, _rest::binary>>), do: 4
    defp bom_length(<<254, 255, 0, 0, _rest::binary>>), do: 4
    defp bom_length(_binary), do: 0

    def get_read_offset(modes) do
      case :lists.keyfind(:read_offset, 1, modes) do
        {:read_offset, offset} -> offset
        false -> 0
      end
    end

    defp read_modes(modes) do
      for mode <- modes, mode not in [:write, :append, :trim_bom], do: mode
    end

    defp count_lines(device, path, pattern, read, count) do
      case read.(device) do
        data when is_binary(data) ->
          count_lines(device, path, pattern, read, count + count_lines(data, pattern))

        :eof ->
          count

        {:error, reason} ->
          raise File.Error, reason: reason, action: "stream", path: path
      end
    end

    defp count_lines(data, pattern), do: length(:binary.matches(data, pattern))

    defp read_function(%{raw: true}), do: &IO.binread(&1, @read_ahead_size)
    defp read_function(%{raw: false}), do: &IO.read(&1, @read_ahead_size)
  end
end
