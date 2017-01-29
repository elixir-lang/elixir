defmodule File.Stream do
  @moduledoc """
  Defines a `File.Stream` struct returned by `File.stream!/3`.

  The following fields are public:

    * `path`          - the file path
    * `modes`         - the file modes
    * `raw`           - a boolean indicating if bin functions should be used
    * `line_or_bytes` - if reading should read lines or a given amount of bytes

  """

  defstruct path: nil, modes: [], line_or_bytes: :line, raw: true

  @type t :: %__MODULE__{}

  @doc false
  def __build__(path, modes, line_or_bytes) do
    raw = :lists.keyfind(:encoding, 1, modes) == false

    modes =
      case raw do
        true ->
          if :lists.keyfind(:read_ahead, 1, modes) == {:read_ahead, false} do
            [:raw | modes]
          else
            [:raw, :read_ahead | modes]
          end
        false ->
          modes
      end

    %File.Stream{path: path, modes: modes, raw: raw, line_or_bytes: line_or_bytes}
  end

  defimpl Collectable do
    def into(%{path: path, modes: modes, raw: raw} = stream) do
      modes = for mode <- modes, mode not in [:read], do: mode

      case :file.open(path, [:write | modes]) do
        {:ok, device} ->
          {:ok, into(device, stream, raw)}
        {:error, reason} ->
          raise File.Error, reason: reason, action: "stream", path: path
      end
    end

    defp into(device, stream, raw) do
      fn
        :ok, {:cont, x} ->
          case raw do
            true  -> IO.binwrite(device, x)
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

    def reduce(%{path: path, modes: modes, line_or_bytes: line_or_bytes, raw: raw}, acc, fun) do
      start_fun =
        fn ->
          case :file.open(path, read_modes(modes)) do
            {:ok, device} ->
              if :trim_bom in modes, do: trim_bom(device), else: device
            {:error, reason} ->
              raise File.Error, reason: reason, action: "stream", path: path
          end
        end

      next_fun =
        case raw do
          true  -> &IO.each_binstream(&1, line_or_bytes)
          false -> &IO.each_stream(&1, line_or_bytes)
        end

      Stream.resource(start_fun, next_fun, &:file.close/1).(acc, fun)
    end

    def count(%{path: path, modes: modes, line_or_bytes: :line} = stream) do
      pattern = :binary.compile_pattern("\n")
      counter = &count_lines(&1, path, pattern, read_function(stream), 0)

      case File.open(path, read_modes(modes), counter) do
        {:ok, count} ->
          {:ok, count}
        {:error, reason} ->
          raise File.Error, reason: reason, action: "stream", path: path
      end
    end

    def count(%{path: path, line_or_bytes: bytes}) do
      case File.stat(path) do
        {:ok, %{size: 0}} ->
          {:error, __MODULE__}
        {:ok, %{size: size}} ->
          {:ok, div(size, bytes) + if(rem(size, bytes) == 0, do: 0, else: 1)}
        {:error, reason} ->
          raise File.Error, reason: reason, action: "stream", path: path
      end
    end

    def member?(_stream, _term) do
      {:error, __MODULE__}
    end

    defp trim_bom(device) do
      header = IO.binread(device, 4)
      {:ok, _new_pos} = :file.position(device, bom_length(header))
      device
    end

    defp bom_length(<<239, 187, 191, _rest::binary>>),
      do: 3
    defp bom_length(<<254, 255, _rest::binary>>),
      do: 2
    defp bom_length(<<255, 254, _rest::binary>>),
      do: 2
    defp bom_length(<<0, 0, 254, 255, _rest::binary>>),
      do: 4
    defp bom_length(<<254, 255, 0, 0, _rest::binary>>),
      do: 4
    defp bom_length(_binary),
      do: 0

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
