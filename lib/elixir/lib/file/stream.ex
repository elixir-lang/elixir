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
      modes = for mode <- modes, not mode in [:read], do: mode

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
    def reduce(%{path: path, modes: modes, line_or_bytes: line_or_bytes, raw: raw}, acc, fun) do
      modes = for mode <- modes, not mode in [:write, :append], do: mode

      start_fun =
        fn ->
          case :file.open(path, modes) do
            {:ok, device}    -> device
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

    def count(_stream) do
      {:error, __MODULE__}
    end

    def member?(_stream, _term) do
      {:error, __MODULE__}
    end
  end
end
