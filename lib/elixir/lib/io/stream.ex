defmodule IO.StreamError do
  defexception [:reason]

  @impl true
  def message(%{reason: reason}) do
    "error during streaming: #{inspect(reason)}"
  end
end

defmodule IO.Stream do
  @moduledoc """
  Defines an `IO.Stream` struct returned by `IO.stream/2` and `IO.binstream/2`.

  The following fields are public:

    * `device`        - the IO device
    * `raw`           - a boolean indicating if bin functions should be used
    * `line_or_bytes` - if reading should read lines or a given number of bytes

  It is worth noting that an IO stream has side effects and every time you go
  over the stream you may get different results.

  """

  defstruct device: nil, raw: true, line_or_bytes: :line

  @type t :: %__MODULE__{
          device: IO.device(),
          raw: boolean(),
          line_or_bytes: :line | non_neg_integer()
        }

  @doc false
  def __build__(device, raw, line_or_bytes) do
    %IO.Stream{device: device, raw: raw, line_or_bytes: line_or_bytes}
  end

  defimpl Collectable do
    def into(%{device: device, raw: raw} = stream) do
      {:ok, into(stream, device, raw)}
    end

    defp into(stream, device, raw) do
      fn
        :ok, {:cont, x} ->
          case raw do
            true -> IO.binwrite(device, x)
            false -> IO.write(device, x)
          end

        :ok, _ ->
          stream
      end
    end
  end

  defimpl Enumerable do
    def reduce(%{device: device, raw: raw, line_or_bytes: line_or_bytes}, acc, fun) do
      next_fun =
        case raw do
          true -> &IO.each_binstream(&1, line_or_bytes)
          false -> &IO.each_stream(&1, line_or_bytes)
        end

      Stream.resource(fn -> device end, next_fun, & &1).(acc, fun)
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
  end
end
