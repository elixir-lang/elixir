# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule ExUnit.CaptureLog do
  @moduledoc ~S"""
  Functionality for capturing logs in tests.

  ## Examples

      defmodule AssertionTest do
        use ExUnit.Case, async: true

        import ExUnit.CaptureLog
        require Logger

        test "capturing function result with `with_log`" do
          {result, log} = with_log(fn ->
              Logger.error("log msg")
              2 + 2
            end)

          assert result == 4
          assert log =~ "log msg"
        end

        test "concurrent processes' logs are captured" do
          parent = self()

          fun = fn ->
            for msg <- ["hello", "hi"] do
              spawn fn ->
                Logger.error(msg)
                send parent, :done
              end

              receive do
                :done -> :ok
              end
            end

            Logger.debug("parent")
          end

          log = capture_log(fun)

          assert log =~ "hello"
          assert log =~ "hi"
          assert log =~ "parent"
        end
      end

  """

  @compile {:no_warn_undefined, Logger}

  @type capture_log_opts :: [
          {:level, Logger.level() | nil}
          | {:formatter, {module(), term()}}
        ]

  @doc """
  Captures Logger messages generated when evaluating `fun`.

  Returns the binary which is the captured output. The captured log
  messages will be formatted using `Logger.default_formatter/1` by
  default. Any option, besides `:level` or `formatter`, will be
  forwarded as an override to the default formatter.

  This function mutes the default logger handler and captures any log
  messages sent to Logger from all logging processes. It is possible
  to ensure explicit log messages from other processes are captured
  by waiting for their exit or monitor signal.

  Note that when `async` is set to `true` on `use ExUnit.Case`,
  messages from other tests might be captured. For this reason,
  the `=~/2` operator is recommended for use when asserting against
  captured logs.

  To get the result of the evaluation along with the captured log,
  use `with_log/2`.

  ## Options

    * `:level` - set the capturing level for the duration of the capture.
      for instance, if set to `:warn`, then any message lower than `:warn`
      (`:notice`, `:info`, and `:debug`) will be ignored.
      Defaults to `nil`, meaning all messages are captured.
      Note this setting does not override the overall `Logger.level/0` value.
      Therefore, if `Logger.level/0` is set to a higher level than the one
      configured in this function, no message will be captured.
      The behaviour is undetermined if async tests change Logger level.

    * `:formatter` - use an alternative log formatter. Must be provided as `{module, config}`
      By default `Logger.default_formatter/1` is used.
  """
  @spec capture_log(capture_log_opts, (-> any)) :: String.t()
  def capture_log(opts \\ [], fun) do
    {_, log} = with_log(opts, fun)
    log
  end

  @doc """
  Invokes the given `fun` and returns the result and captured log.

  It accepts the same arguments and options as `capture_log/2`.

  ## Examples

      {result, log} =
        with_log(fn ->
          Logger.error("log msg")
          2 + 2
        end)

      assert result == 4
      assert log =~ "log msg"

  """
  @doc since: "1.13.0"
  @spec with_log(capture_log_opts, (-> result)) :: {result, log :: String.t()} when result: any
  def with_log(opts \\ [], fun) when is_list(opts) do
    opts =
      if opts[:level] == :warn do
        IO.warn("level: :warn is deprecated, please use :warning instead")
        Keyword.put(opts, :level, :warning)
      else
        opts
      end

    {:ok, string_io} = StringIO.open("")

    try do
      ref = ExUnit.CaptureServer.log_capture_on(self(), string_io, opts)

      try do
        fun.()
      after
        :ok = Logger.flush()
        :ok = ExUnit.CaptureServer.log_capture_off(ref)
      end
    catch
      kind, reason ->
        _ = StringIO.close(string_io)
        :erlang.raise(kind, reason, __STACKTRACE__)
    else
      result ->
        {:ok, {_input, output}} = StringIO.close(string_io)
        {result, output}
    end
  end
end
