# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.JSONFormatterTest do
  use ExUnit.Case, async: true

  defp decode(iodata) do
    :json.decode(IO.iodata_to_binary(iodata))
  end

  defp passing_test(opts \\ []) do
    tags = %{
      file: opts[:file] || "test/my_test.exs",
      line: opts[:line] || 10,
      module: MyTest,
      test: :"test passes",
      test_type: :test,
      describe: nil,
      describe_line: nil,
      async: false,
      registered: %{}
    }

    tags = Map.merge(tags, Map.new(opts[:extra_tags] || []))

    %ExUnit.Test{
      name: opts[:name] || :"test passes",
      module: opts[:module] || MyTest,
      state: nil,
      time: opts[:time] || 1234,
      tags: tags,
      logs: ""
    }
  end

  defp failed_test(opts \\ []) do
    error = %ExUnit.AssertionError{
      message: "Assertion with == failed",
      expr: quote(do: assert(1 == 2)),
      left: 1,
      right: 2
    }

    stacktrace = [
      {MyTest, :"test fails", 1, [file: ~c"test/my_test.exs", line: 16]}
    ]

    tags = %{
      file: "test/my_test.exs",
      line: opts[:line] || 15,
      module: MyTest,
      test: :"test fails",
      test_type: :test,
      describe: nil,
      describe_line: nil,
      async: false,
      registered: %{}
    }

    %ExUnit.Test{
      name: :"test fails",
      module: MyTest,
      state: {:failed, [{:error, error, stacktrace}]},
      time: 2345,
      tags: tags,
      logs: ""
    }
  end

  defp skipped_test do
    tags = %{
      file: "test/my_test.exs",
      line: 20,
      module: MyTest,
      test: :"test skipped",
      test_type: :test,
      describe: nil,
      describe_line: nil,
      async: false,
      registered: %{}
    }

    %ExUnit.Test{
      name: :"test skipped",
      module: MyTest,
      state: {:skipped, "not implemented"},
      time: 0,
      tags: tags,
      logs: ""
    }
  end

  defp excluded_test do
    tags = %{
      file: "test/my_test.exs",
      line: 25,
      module: MyTest,
      test: :"test excluded",
      test_type: :test,
      describe: nil,
      describe_line: nil,
      async: false,
      registered: %{}
    }

    %ExUnit.Test{
      name: :"test excluded",
      module: MyTest,
      state: {:excluded, "slow test"},
      time: 0,
      tags: tags,
      logs: ""
    }
  end

  defp invalid_test do
    failed_module = %ExUnit.TestModule{
      name: FailingModule,
      file: "test/failing_test.exs",
      state: {:failed, [{:error, %RuntimeError{message: "setup_all failed"}, []}]},
      tests: []
    }

    tags = %{
      file: "test/failing_test.exs",
      line: 5,
      module: FailingModule,
      test: :"test invalidated",
      test_type: :test,
      describe: nil,
      describe_line: nil,
      async: false,
      registered: %{}
    }

    %ExUnit.Test{
      name: :"test invalidated",
      module: FailingModule,
      state: {:invalid, failed_module},
      time: 0,
      tags: tags,
      logs: ""
    }
  end

  describe "encode_state" do
    test "maps nil to passed" do
      test = passing_test()
      {:ok, config} = ExUnit.JSONFormatter.init(seed: 42)
      {:noreply, config} = ExUnit.JSONFormatter.handle_cast({:test_finished, test}, config)

      output = capture_json(config, %{run: 10_000})
      [result] = output["tests"]
      assert result["state"] == "passed"
    end

    test "maps failed state" do
      test = failed_test()
      {:ok, config} = ExUnit.JSONFormatter.init(seed: 42)
      {:noreply, config} = ExUnit.JSONFormatter.handle_cast({:test_finished, test}, config)

      output = capture_json(config, %{run: 10_000})
      [result] = output["tests"]
      assert result["state"] == "failed"
    end

    test "maps skipped state" do
      test = skipped_test()
      {:ok, config} = ExUnit.JSONFormatter.init(seed: 42)
      {:noreply, config} = ExUnit.JSONFormatter.handle_cast({:test_finished, test}, config)

      output = capture_json(config, %{run: 10_000})
      [result] = output["tests"]
      assert result["state"] == "skipped"
    end

    test "maps excluded state" do
      test = excluded_test()
      {:ok, config} = ExUnit.JSONFormatter.init(seed: 42)
      {:noreply, config} = ExUnit.JSONFormatter.handle_cast({:test_finished, test}, config)

      output = capture_json(config, %{run: 10_000})
      [result] = output["tests"]
      assert result["state"] == "excluded"
    end

    test "maps invalid state" do
      test = invalid_test()
      {:ok, config} = ExUnit.JSONFormatter.init(seed: 42)
      {:noreply, config} = ExUnit.JSONFormatter.handle_cast({:test_finished, test}, config)

      output = capture_json(config, %{run: 10_000})
      [result] = output["tests"]
      assert result["state"] == "invalid"
    end
  end

  describe "passing test encoding" do
    test "encodes basic fields" do
      test = passing_test()
      {:ok, config} = ExUnit.JSONFormatter.init(seed: 42)
      {:noreply, config} = ExUnit.JSONFormatter.handle_cast({:test_finished, test}, config)

      output = capture_json(config, %{run: 10_000})
      [result] = output["tests"]

      assert result["module"] == "MyTest"
      assert result["name"] == "test passes"
      assert result["line"] == 10
      assert result["time_us"] == 1234
      assert result["failures"] == []
    end

    test "file path is relative" do
      test = passing_test()
      {:ok, config} = ExUnit.JSONFormatter.init(seed: 42)
      {:noreply, config} = ExUnit.JSONFormatter.handle_cast({:test_finished, test}, config)

      output = capture_json(config, %{run: 10_000})
      [result] = output["tests"]
      refute String.starts_with?(result["file"], "/")
    end
  end

  describe "failed test encoding" do
    test "includes failure details" do
      test = failed_test()
      {:ok, config} = ExUnit.JSONFormatter.init(seed: 42)
      {:noreply, config} = ExUnit.JSONFormatter.handle_cast({:test_finished, test}, config)

      output = capture_json(config, %{run: 10_000})
      [result] = output["tests"]
      [failure] = result["failures"]

      assert failure["kind"] == "assertion"
      assert failure["message"] =~ "Assertion with == failed"
      assert is_list(failure["stacktrace"])
    end

    test "includes structured assertion details" do
      test = failed_test()
      {:ok, config} = ExUnit.JSONFormatter.init(seed: 42)
      {:noreply, config} = ExUnit.JSONFormatter.handle_cast({:test_finished, test}, config)

      output = capture_json(config, %{run: 10_000})
      [result] = output["tests"]
      [failure] = result["failures"]

      assert failure["assertion"]["left"] == "1"
      assert failure["assertion"]["right"] == "2"
      assert failure["assertion"]["expr"] =~ "assert"
    end

    test "encodes stacktrace frames" do
      test = failed_test()
      {:ok, config} = ExUnit.JSONFormatter.init(seed: 42)
      {:noreply, config} = ExUnit.JSONFormatter.handle_cast({:test_finished, test}, config)

      output = capture_json(config, %{run: 10_000})
      [result] = output["tests"]
      [failure] = result["failures"]
      [frame] = failure["stacktrace"]

      assert frame["module"] == "MyTest"
      assert frame["function"] == "test fails"
      assert frame["arity"] == 1
      assert frame["line"] == 16
    end
  end

  describe "tag filtering" do
    test "filters internal ExUnit tags" do
      test = passing_test()
      {:ok, config} = ExUnit.JSONFormatter.init(seed: 42)
      {:noreply, config} = ExUnit.JSONFormatter.handle_cast({:test_finished, test}, config)

      output = capture_json(config, %{run: 10_000})
      [result] = output["tests"]
      tags = result["tags"]

      refute Map.has_key?(tags, "file")
      refute Map.has_key?(tags, "line")
      refute Map.has_key?(tags, "module")
      refute Map.has_key?(tags, "test")
      refute Map.has_key?(tags, "test_type")
      refute Map.has_key?(tags, "describe")
      refute Map.has_key?(tags, "describe_line")
      refute Map.has_key?(tags, "async")
      refute Map.has_key?(tags, "registered")
    end

    test "preserves user-defined tags" do
      test = passing_test(extra_tags: [priority: :high, timeout: 5000])
      {:ok, config} = ExUnit.JSONFormatter.init(seed: 42)
      {:noreply, config} = ExUnit.JSONFormatter.handle_cast({:test_finished, test}, config)

      output = capture_json(config, %{run: 10_000})
      [result] = output["tests"]
      tags = result["tags"]

      assert tags["priority"] == "high"
      assert tags["timeout"] == 5000
    end

    test "serializes tag value types correctly" do
      test =
        passing_test(
          extra_tags: [
            bool_tag: true,
            atom_tag: :fast,
            string_tag: "hello",
            number_tag: 42,
            list_tag: [1, :two, "three"],
            map_tag: %{nested: "value", count: 3}
          ]
        )

      {:ok, config} = ExUnit.JSONFormatter.init(seed: 42)
      {:noreply, config} = ExUnit.JSONFormatter.handle_cast({:test_finished, test}, config)

      output = capture_json(config, %{run: 10_000})
      [result] = output["tests"]
      tags = result["tags"]

      assert tags["bool_tag"] == true
      assert tags["atom_tag"] == "fast"
      assert tags["string_tag"] == "hello"
      assert tags["number_tag"] == 42
      assert tags["list_tag"] == [1, "two", "three"]
      assert tags["map_tag"] == %{"nested" => "value", "count" => 3}
    end
  end

  describe "deterministic sort order" do
    test "sorts by file, line, name" do
      test_a = passing_test(name: :"test z_last", file: "test/a_test.exs", line: 10)
      test_b = passing_test(name: :"test a_first", file: "test/b_test.exs", line: 5)
      test_c = passing_test(name: :"test middle", file: "test/a_test.exs", line: 20)

      {:ok, config} = ExUnit.JSONFormatter.init(seed: 42)
      {:noreply, config} = ExUnit.JSONFormatter.handle_cast({:test_finished, test_b}, config)
      {:noreply, config} = ExUnit.JSONFormatter.handle_cast({:test_finished, test_c}, config)
      {:noreply, config} = ExUnit.JSONFormatter.handle_cast({:test_finished, test_a}, config)

      output = capture_json(config, %{run: 10_000})
      names = Enum.map(output["tests"], & &1["name"])

      assert names == ["test z_last", "test middle", "test a_first"]
    end
  end

  describe "summary" do
    test "includes correct counts" do
      {:ok, config} = ExUnit.JSONFormatter.init(seed: 42)

      {:noreply, config} =
        ExUnit.JSONFormatter.handle_cast({:test_finished, passing_test()}, config)

      {:noreply, config} =
        ExUnit.JSONFormatter.handle_cast(
          {:test_finished, passing_test(name: :"test passes 2", line: 11)},
          config
        )

      {:noreply, config} =
        ExUnit.JSONFormatter.handle_cast({:test_finished, failed_test()}, config)

      {:noreply, config} =
        ExUnit.JSONFormatter.handle_cast({:test_finished, skipped_test()}, config)

      {:noreply, config} =
        ExUnit.JSONFormatter.handle_cast({:test_finished, excluded_test()}, config)

      {:noreply, config} =
        ExUnit.JSONFormatter.handle_cast({:test_finished, invalid_test()}, config)

      output = capture_json(config, %{run: 50_000})
      summary = output["summary"]

      assert summary["total"] == 6
      assert summary["passed"] == 2
      assert summary["failed"] == 1
      assert summary["skipped"] == 1
      assert summary["excluded"] == 1
      assert summary["invalid"] == 1
      assert summary["duration_us"] == 50_000
    end

    test "includes seed" do
      {:ok, config} = ExUnit.JSONFormatter.init(seed: 12345)
      output = capture_json(config, %{run: 1000})

      assert output["seed"] == 12345
    end
  end

  describe "module failures" do
    test "tracks setup_all failures" do
      module = %ExUnit.TestModule{
        name: FailingModule,
        file: "test/failing_test.exs",
        state: {:failed, [{:error, %RuntimeError{message: "setup_all failed"}, []}]},
        tests: []
      }

      {:ok, config} = ExUnit.JSONFormatter.init(seed: 42)
      {:noreply, config} = ExUnit.JSONFormatter.handle_cast({:module_finished, module}, config)

      output = capture_json(config, %{run: 10_000})
      [mod_failure] = output["module_failures"]

      assert mod_failure["module"] == "FailingModule"
      [failure] = mod_failure["failures"]
      assert failure["kind"] == "error"
      assert failure["message"] == "setup_all failed"
    end

    test "ignores passing modules" do
      module = %ExUnit.TestModule{
        name: PassingModule,
        file: "test/passing_test.exs",
        state: nil,
        tests: []
      }

      {:ok, config} = ExUnit.JSONFormatter.init(seed: 42)
      {:noreply, config} = ExUnit.JSONFormatter.handle_cast({:module_finished, module}, config)

      output = capture_json(config, %{run: 10_000})
      assert output["module_failures"] == []
    end
  end

  describe "sigquit" do
    test "outputs partial results with aborted flag" do
      test = passing_test()
      {:ok, config} = ExUnit.JSONFormatter.init(seed: 42)
      {:noreply, config} = ExUnit.JSONFormatter.handle_cast({:test_finished, test}, config)

      output =
        ExUnit.CaptureIO.capture_io(fn ->
          ExUnit.JSONFormatter.handle_cast({:sigquit, []}, config)
        end)

      result = decode(output)
      assert result["summary"]["aborted"] == true
      assert length(result["tests"]) == 1
    end
  end

  describe "full GenServer lifecycle" do
    test "produces valid JSON through complete lifecycle" do
      {:ok, pid} = GenServer.start_link(ExUnit.JSONFormatter, seed: 42)

      GenServer.cast(pid, {:suite_started, [seed: 42]})
      GenServer.cast(pid, {:test_finished, passing_test()})
      GenServer.cast(pid, {:test_finished, failed_test()})

      output =
        ExUnit.CaptureIO.capture_io(fn ->
          # Set the GenServer's group leader to the captured IO process
          # so that its IO.write output is captured
          Process.group_leader(pid, Process.group_leader())
          GenServer.cast(pid, {:suite_finished, %{run: 50_000, async: nil, load: nil}})
          # Wait for the cast to be processed
          :sys.get_state(pid)
        end)

      result = decode(output)

      assert is_integer(result["seed"])
      assert is_map(result["summary"])
      assert is_list(result["tests"])
      assert is_list(result["module_failures"])
      assert length(result["tests"]) == 2

      GenServer.stop(pid)
    end
  end

  describe "non-assertion failures" do
    test "encodes runtime errors" do
      stacktrace = [{MyTest, :"test crashes", 1, [file: ~c"test/my_test.exs", line: 30]}]

      tags = %{
        file: "test/my_test.exs",
        line: 30,
        module: MyTest,
        test: :"test crashes",
        test_type: :test,
        describe: nil,
        describe_line: nil,
        async: false,
        registered: %{}
      }

      test = %ExUnit.Test{
        name: :"test crashes",
        module: MyTest,
        state: {:failed, [{:error, %RuntimeError{message: "boom"}, stacktrace}]},
        time: 500,
        tags: tags,
        logs: ""
      }

      {:ok, config} = ExUnit.JSONFormatter.init(seed: 42)
      {:noreply, config} = ExUnit.JSONFormatter.handle_cast({:test_finished, test}, config)

      output = capture_json(config, %{run: 10_000})
      [result] = output["tests"]
      [failure] = result["failures"]

      assert failure["kind"] == "error"
      assert failure["message"] == "boom"
      refute Map.has_key?(failure, "assertion")
    end

    test "encodes exit failures" do
      tags = %{
        file: "test/my_test.exs",
        line: 35,
        module: MyTest,
        test: :"test exits",
        test_type: :test,
        describe: nil,
        describe_line: nil,
        async: false,
        registered: %{}
      }

      test = %ExUnit.Test{
        name: :"test exits",
        module: MyTest,
        state: {:failed, [{:exit, :shutdown, []}]},
        time: 100,
        tags: tags,
        logs: ""
      }

      {:ok, config} = ExUnit.JSONFormatter.init(seed: 42)
      {:noreply, config} = ExUnit.JSONFormatter.handle_cast({:test_finished, test}, config)

      output = capture_json(config, %{run: 10_000})
      [result] = output["tests"]
      [failure] = result["failures"]

      assert failure["kind"] == "exit"
    end

    test "encodes throw failures" do
      tags = %{
        file: "test/my_test.exs",
        line: 40,
        module: MyTest,
        test: :"test throws",
        test_type: :test,
        describe: nil,
        describe_line: nil,
        async: false,
        registered: %{}
      }

      test = %ExUnit.Test{
        name: :"test throws",
        module: MyTest,
        state: {:failed, [{:throw, "nope", []}]},
        time: 100,
        tags: tags,
        logs: ""
      }

      {:ok, config} = ExUnit.JSONFormatter.init(seed: 42)
      {:noreply, config} = ExUnit.JSONFormatter.handle_cast({:test_finished, test}, config)

      output = capture_json(config, %{run: 10_000})
      [result] = output["tests"]
      [failure] = result["failures"]

      assert failure["kind"] == "throw"
    end
  end

  describe "ignores unknown events" do
    test "handles unknown cast events gracefully" do
      {:ok, config} = ExUnit.JSONFormatter.init(seed: 42)
      {:noreply, ^config} = ExUnit.JSONFormatter.handle_cast({:test_started, %{}}, config)
      {:noreply, ^config} = ExUnit.JSONFormatter.handle_cast(:max_failures_reached, config)
      {:noreply, ^config} = ExUnit.JSONFormatter.handle_cast({:module_started, %{}}, config)
    end
  end

  # Helper to drive suite_finished and capture JSON output
  defp capture_json(config, times_us) do
    output =
      ExUnit.CaptureIO.capture_io(fn ->
        ExUnit.JSONFormatter.handle_cast({:suite_finished, times_us}, config)
      end)

    decode(output)
  end
end
