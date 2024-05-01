defmodule Logger.Backends.HandlerTest do
  use Logger.Case
  @moduletag :logger

  defmodule CustomTranslator do
    def t(:debug, _level, :format, {~c"hello: ~p", [:ok]}) do
      :skip
    end

    def t(:debug, _level, :format, {~c"world: ~p", [:ok]}) do
      {:ok, "rewritten"}
    end

    def t(:debug, :info, :report, {:logger, %{hello: :ok}}) do
      :skip
    end

    def t(:debug, :info, :report, {:logger, %{world: :ok}}) do
      {:ok, "rewritten"}
    end

    def t(:debug, :info, :report, {:logger, %{error: error}}) do
      raise(error)
    end

    def t(_, _, _, _) do
      :none
    end
  end

  setup_all do
    Application.put_env(:logger, :default_handler, false)
    Logger.App.stop()

    # Explicitly use add to make sure that root is dynamically started
    Application.start(:logger)
    Logger.Backends.Internal.add(Logger.Backends.Console)

    on_exit(fn ->
      Application.delete_env(:logger, :default_handler)
      Logger.App.stop()
      Application.start(:logger)
    end)
  end

  test "add_translator/1 and remove_translator/1 for error_logger" do
    assert Logger.add_translator({CustomTranslator, :t})

    assert capture_log(fn ->
             :error_logger.info_msg(~c"hello: ~p", [:ok])
           end) == ""

    assert capture_log(fn ->
             :error_logger.info_msg(~c"world: ~p", [:ok])
           end) =~ "[notice] rewritten"
  after
    assert Logger.remove_translator({CustomTranslator, :t})
  end

  test "add_translator/1 and remove_translator/1 for logger formats" do
    refute {CustomTranslator, :t} in Application.fetch_env!(:logger, :translators)
    assert Logger.add_translator({CustomTranslator, :t})
    assert {CustomTranslator, :t} in Application.fetch_env!(:logger, :translators)

    assert capture_log(fn ->
             :logger.info(~c"hello: ~p", [:ok])
           end) == ""

    assert capture_log(fn ->
             :logger.info(~c"world: ~p", [:ok])
           end) =~ "[info] rewritten"

    assert capture_log(fn ->
             :logger.info(%{hello: :ok})
           end) == ""

    assert capture_log(fn ->
             :logger.info(%{world: :ok})
           end) =~ "[info] rewritten"
  after
    assert Logger.remove_translator({CustomTranslator, :t})
  end

  test "handles translation error" do
    assert Logger.add_translator({CustomTranslator, :t})

    message = capture_log(fn -> :logger.info(%{error: "oops"}) end)
    assert message =~ "[info] Failure while translating Erlang's logger event\n"
    assert message =~ "** (RuntimeError) oops\n"
  after
    assert Logger.remove_translator({CustomTranslator, :t})
  end

  test "converts Erlang metadata" do
    Logger.configure_backend(Logger.Backends.Console,
      metadata: [:file, :line, :module, :function]
    )

    message =
      capture_log(fn ->
        :logger.info("ok", %{file: ~c"file.erl", line: 13, mfa: {Foo, :bar, 3}})
      end)

    assert message =~ "module=Foo"
    assert message =~ "function=bar/3"
    assert message =~ "file=file.erl"
    assert message =~ "line=13"
  after
    Logger.configure_backend(Logger.Backends.Console, metadata: [])
  end

  test "uses reporting callback with Elixir inspection" do
    assert capture_log(fn ->
             callback = fn %{hello: :world} -> {"~p~n", [:formatted]} end
             :logger.info(%{hello: :world}, %{report_cb: callback})
           end) =~ "[info] :formatted"
  end

  test "uses Erlang log levels" do
    assert capture_log(fn -> :logger.emergency(~c"ok") end) =~ "[emergency] ok"
    assert capture_log(fn -> :logger.alert(~c"ok") end) =~ "[alert] ok"
    assert capture_log(fn -> :logger.critical(~c"ok") end) =~ "[critical] ok"
    assert capture_log(fn -> :logger.error(~c"ok") end) =~ "[error] ok"
    assert capture_log(fn -> :logger.warning(~c"ok") end) =~ "[warning] ok"
    assert capture_log(fn -> :logger.notice(~c"ok") end) =~ "[notice] ok"
    assert capture_log(fn -> :logger.info(~c"ok") end) =~ "[info] ok"
    assert capture_log(fn -> :logger.debug(~c"ok") end) =~ "[debug] ok"
  end

  test "include Erlang severity level information" do
    Logger.configure_backend(Logger.Backends.Console, metadata: [:erl_level])

    assert capture_log(fn -> :logger.emergency(~c"ok") end) =~ "erl_level=emergency"
    assert capture_log(fn -> :logger.alert(~c"ok") end) =~ "erl_level=alert"
    assert capture_log(fn -> :logger.critical(~c"ok") end) =~ "erl_level=critical"
    assert capture_log(fn -> :logger.error(~c"ok") end) =~ "erl_level=error"
    assert capture_log(fn -> :logger.warning(~c"ok") end) =~ "erl_level=warning"
    assert capture_log(fn -> :logger.info(~c"ok") end) =~ "erl_level=info"
    assert capture_log(fn -> :logger.debug(~c"ok") end) =~ "erl_level=debug"

    [:emergency, :alert, :critical, :error, :warning, :notice, :info, :debug]
    |> Enum.each(fn level ->
      assert capture_log(fn -> :logger.log(level, ~c"ok") end) =~ "erl_level=#{level}"
    end)
  after
    Logger.configure_backend(Logger.Backends.Console, metadata: [])
  end

  test "respects translator_inspect_opts for reports" do
    Application.put_env(:logger, :translator_inspect_opts, printable_limit: 1)

    assert capture_log(fn -> :logger.error(%{foo: "bar"}) end) =~
             ~S([error] [foo: "b" <> ...])
  after
    Application.put_env(:logger, :translator_inspect_opts, [])
  end

  test "calls report_cb/1 when supplied" do
    report = %{foo: "bar"}

    assert capture_log(fn -> :logger.error(report, %{report_cb: &format_report/1}) end) =~
             ~S([error] %{foo: "bar"})

    assert_received {:format, ^report}
  end

  test "calls report_cb/2 when supplied" do
    report = %{foo: "bar"}

    assert capture_log(fn -> :logger.error(report, %{report_cb: &format_report/2}) end) =~
             ~S([error] %{foo: "bar"})

    assert_received {:format, ^report, opts} when is_map(opts)
    assert %{chars_limit: 8096, depth: :unlimited, single_line: false} = opts
  end

  test "calls report_cb/2 when passed %{label: term(), report: term()}" do
    report = %{label: :foo, report: %{bar: 1, baz: 2}}

    assert capture_log(fn -> :logger.error(report, %{report_cb: &format_report/1}) end)
    assert_received {:format, ^report}

    assert capture_log(fn -> :logger.error(report, %{report_cb: &format_report/2}) end)
    assert_received {:format, ^report, _opts}
  end

  defp format_report(report) do
    send(self(), {:format, report})

    {~c"~p", [report]}
  end

  defp format_report(report, opts) do
    send(self(), {:format, report, opts})

    inspect(report)
  end
end
