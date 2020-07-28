defmodule Logger.HandlerTest do
  use Logger.Case
  @moduletag :logger

  defmodule CustomTranslator do
    def t(:debug, :info, :format, {'hello: ~p', [:ok]}) do
      :skip
    end

    def t(:debug, :info, :format, {'world: ~p', [:ok]}) do
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

  test "add_translator/1 and remove_translator/1 for error_logger" do
    assert Logger.add_translator({CustomTranslator, :t})

    assert capture_log(fn ->
             :error_logger.info_msg('hello: ~p', [:ok])
           end) == ""

    assert capture_log(fn ->
             :error_logger.info_msg('world: ~p', [:ok])
           end) =~ "[info]  rewritten"
  after
    assert Logger.remove_translator({CustomTranslator, :t})
  end

  test "add_translator/1 and remove_translator/1 for logger formats" do
    assert Logger.add_translator({CustomTranslator, :t})

    assert capture_log(fn ->
             :logger.info('hello: ~p', [:ok])
           end) == ""

    assert capture_log(fn ->
             :logger.info('world: ~p', [:ok])
           end) =~ "[info]  rewritten"

    assert capture_log(fn ->
             :logger.info(%{hello: :ok})
           end) == ""

    assert capture_log(fn ->
             :logger.info(%{world: :ok})
           end) =~ "[info]  rewritten"
  after
    assert Logger.remove_translator({CustomTranslator, :t})
  end

  test "handles translation error" do
    assert Logger.add_translator({CustomTranslator, :t})

    message = capture_log(fn -> :logger.info(%{error: "oops"}) end)
    assert message =~ "[info]  Failure while translating Erlang's logger event\n"
    assert message =~ "** (RuntimeError) oops\n"
  after
    assert Logger.remove_translator({CustomTranslator, :t})
  end

  test "converts Erlang metadata" do
    Logger.configure_backend(:console, metadata: [:file, :line, :module, :function])

    message =
      capture_log(fn ->
        :logger.info("ok", %{file: 'file.erl', line: 13, mfa: {Foo, :bar, 3}})
      end)

    assert message =~ "module=Foo"
    assert message =~ "function=bar/3"
    assert message =~ "file=file.erl"
    assert message =~ "line=13"
  after
    Logger.configure_backend(:console, metadata: [])
  end

  test "uses reporting callback with Elixir inspection" do
    assert capture_log(fn ->
             callback = fn %{hello: :world} -> {"~p~n", [:formatted]} end
             :logger.info(%{hello: :world}, %{report_cb: callback})
           end) =~ "[info]  :formatted"
  end

  test "converts log levels" do
    assert capture_log(fn -> :logger.emergency('ok') end) =~ "[error] ok"
    assert capture_log(fn -> :logger.alert('ok') end) =~ "[error] ok"
    assert capture_log(fn -> :logger.critical('ok') end) =~ "[error] ok"
    assert capture_log(fn -> :logger.error('ok') end) =~ "[error] ok"
    assert capture_log(fn -> :logger.warning('ok') end) =~ "[warn]  ok"
    assert capture_log(fn -> :logger.info('ok') end) =~ "[info]  ok"
    assert capture_log(fn -> :logger.debug('ok') end) =~ "[debug] ok"
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
    assert %{chars_limit: _} = opts
    assert %{depth: _} = opts
    assert %{single_line: false} = opts

    Application.put_env(:logger, :translator_inspect_opts, limit: 10, printable_limit: 1000)

    assert capture_log(fn -> :logger.error(report, %{report_cb: &format_report/2}) end) =~
             ~S([error] %{foo: "bar"})

    assert_received {:format, ^report, opts} when is_map(opts)
    assert %{chars_limit: 1000} = opts
    assert %{depth: 10} = opts
    assert %{single_line: false} = opts
  end

  test "calls report_cb when passed %{label: term(), report: term()}" do
    report = %{label: :foo, report: %{bar: 1, baz: 2}}

    assert capture_log(fn -> :logger.error(report, %{report_cb: &format_report/1}) end)
    assert_received {:format, ^report}

    assert capture_log(fn -> :logger.error(report, %{report_cb: &format_report/2}) end)
    assert_received {:format, ^report, _opts}
  end

  defp format_report(report) do
    send(self(), {:format, report})

    {'~p', [report]}
  end

  defp format_report(report, opts) do
    send(self(), {:format, report, opts})

    inspect(report)
  end
end
