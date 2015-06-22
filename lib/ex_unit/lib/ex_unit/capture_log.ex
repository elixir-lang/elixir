defmodule ExUnit.CaptureLog do
  @moduledoc ~S"""
  Functionality to capture logs for testing.

  ## Examples

      defmodule AssertionTest do
        use ExUnit.Case

        import ExUnit.CaptureLog

        test "example" do
          assert capture_log(fn ->
            Logger.error "log msg"
          end) =~ "log msg"
        end

        test "check multiple captures concurrently" do
          fun = fn ->
            for msg <- ["hello", "hi"] do
              assert capture_log(fn -> Logger.error msg end) =~ msg
            end
            Logger.debug "testing"
          end
          assert capture_log(fun) =~ "hello"
          assert capture_log(fun) =~ "testing"
        end
      end

  """

  @doc """
  Captures Logger messages generated when evaluating `fun`.

  Returns the binary which is the captured output.

  This function mutes the `:console` backend
  and captures any log messages sent to Logger.

  Note that when the `async` is set to `true`,
  the messages from another test might be captured.

  It is possible to configure the level to capture with `:level`,
  which will set the capturing level for the duration of the
  capture, for instance, if the log level is set to :error
  any message with the lower level will be ignored.
  The default level is `nil`, which will capture all messages.
  The behaviour is undetermined if async tests change Logger level.

  The format, metadata and colors can be configured with `:format`,
  `:metadata` and `:colors` respectively. These three options
  defaults to the `:console` backend configuration parameters.
  """
  @spec capture_log(Keyword.t, (() -> any)) :: String.t
  def capture_log(opts \\ [], fun) do
    opts = Keyword.put_new(opts, :level, nil)
    {:ok, string_io} = StringIO.open("")
    try do
      case ExUnit.Server.log_capture_on(self(), {string_io, opts}) do
        {:ok, ref} ->
          try do
            _ = fun.()
          after
            :ok = Logger.flush()
            case ExUnit.Server.log_capture_off(ref) do
              {:error, reason} ->
                exit({reason, {ExUnit.Server, :log_capture_off, [ref]}})
              :ok -> :ok
            end
          end
        {:error, :no_logger} -> exit(:noproc)
      end
      :ok
    catch
      kind, reason ->
        stack = System.stacktrace()
        _ = StringIO.close(string_io)
        :erlang.raise(kind, reason, stack)
    else
      :ok ->
        {:ok, content} = StringIO.close(string_io)
        elem(content, 1)
    end
  end
end
