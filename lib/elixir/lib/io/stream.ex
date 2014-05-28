defmodule IO.StreamError do
  defexception [:reason, :message]

  def exception(opts) do
    reason    = opts[:reason]
    formatted = IO.iodata_to_binary(:file.format_error(reason))
    %IO.StreamError{message: "error during streaming: #{formatted}", reason: reason}
  end
end

defmodule IO.Stream do
  @moduledoc """
  Defines a `IO.Stream` struct returned by `IO.stream/2` and `IO.binstream/2`.

  The following fields are public:

  * `device` - the IO device
  * `raw` - a boolean indicating if bin functions should be used
  * `line_or_bytes` - if reading should read lines or a given amount of bytes

  """

  defstruct device: nil, raw: true, line_or_bytes: :line

  defimpl Collectable do
    def empty(stream) do
      stream
    end

    def into(%{device: device, raw: raw} = stream) do
      {:ok, into(stream, device, raw)}
    end

    defp into(stream, device, raw) do
      fn
        :ok, {:cont, x} ->
          case raw do
            true  -> IO.binwrite(device, x)
            false -> IO.write(device, x)
          end
        :ok, _ -> stream
      end
    end
  end

  defimpl Enumerable do
    def reduce(%{device: device, raw: raw, line_or_bytes: line_or_bytes}, acc, fun) do
      next_fun =
        case raw do
          true  -> &IO.each_binstream(&1, line_or_bytes)
          false -> &IO.each_stream(&1, line_or_bytes)
        end
      Stream.unfold(device, next_fun).(acc, fun)
    end

    def count(_stream) do
      {:error, __MODULE__}
    end

    def member?(_stream, _term) do
      {:error, __MODULE__}
    end
  end
end
