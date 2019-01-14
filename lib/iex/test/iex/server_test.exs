Code.require_file("../test_helper.exs", __DIR__)

defmodule IEx.ServerTest do
  use IEx.Case

  require IEx

  describe "options" do
    test "prefix" do
      assert capture_io(fn ->
               IEx.Server.run(prefix: "pry")
             end) =~ "pry(1)> "
    end

    test "env" do
      assert capture_iex("__ENV__.file", [], env: __ENV__) =~ "server_test.exs"
    end
  end

  describe "pry" do
    test "no sessions" do
      assert capture_io(fn ->
               assert IEx.pry() == {:error, :no_iex}
             end) =~ "Is an IEx shell running?"
    end

    test "inside evaluator itself" do
      assert capture_iex("require IEx; IEx.pry") =~ "Break reached"
    end

    test "outside of the evaluator with acceptance", config do
      Process.register(self(), config.test)

      {server, evaluator} = pry_session(config.test, "Y\niex_context")
      client = pry_request([server])
      send(evaluator, :run)

      assert Task.await(server) =~ ":inside_pry"
      assert Task.await(client) == :ok
    end

    test "outside of the evaluator with refusal", config do
      Process.register(self(), config.test)

      {server, evaluator} = pry_session(config.test, "N\niex_context")
      client = pry_request([server])
      send(evaluator, :run)

      assert Task.await(client) == {:error, :refused}
      assert Task.await(server) =~ "undefined function iex_context"
    end

    test "outside of the evaluator with crash", config do
      Process.register(self(), config.test)

      {server, _evaluator} = pry_session(config.test, "iex_context")
      client = pry_request([server])

      _ = Task.shutdown(server, :brutal_kill)
      assert Task.await(client) == {:error, :refused}
    end

    test "outside of the evaluator with double acceptance", config do
      Process.register(self(), config.test)

      {server1, evaluator1} = pry_session(config.test, "Y\niex_context")
      {server2, evaluator2} = pry_session(config.test, "Y\niex_context")
      client = pry_request([server1, server2])

      send(evaluator1, :run)
      send(evaluator2, :run)
      reply1 = Task.await(server1)
      reply2 = Task.await(server2)

      {accepted, refused} =
        if reply1 =~ ":inside_pry", do: {reply1, reply2}, else: {reply2, reply1}

      assert accepted =~ ":inside_pry"
      assert refused =~ "** session was already accepted elsewhere"
      assert refused =~ "undefined function iex_context"

      assert Task.await(client) == :ok
    end

    test "outside of the evaluator with double refusal", config do
      Process.register(self(), config.test)

      {server1, evaluator1} = pry_session(config.test, "N\niex_context")
      {server2, evaluator2} = pry_session(config.test, "N\niex_context")
      client = pry_request([server1, server2])

      send(evaluator1, :run)
      send(evaluator2, :run)
      reply1 = Task.await(server1)
      reply2 = Task.await(server2)

      assert reply1 =~ "undefined function iex_context"
      assert reply2 =~ "undefined function iex_context"

      assert Task.await(client) == {:error, :refused}
    end

    test "outside of the evaluator with acceptance and then refusal", config do
      Process.register(self(), config.test)

      {server1, evaluator1} = pry_session(config.test, "Y\niex_context")
      {server2, evaluator2} = pry_session(config.test, "N\niex_context")
      client = pry_request([server1, server2])

      send(evaluator1, :run)
      send(evaluator2, :run)
      assert Task.await(server1) =~ ":inside_pry"
      assert Task.await(server2) =~ "undefined function iex_context"

      assert Task.await(client) == :ok
    end

    test "outside of the evaluator with refusal and then acceptance", config do
      Process.register(self(), config.test)

      {server1, evaluator1} = pry_session(config.test, "N\niex_context")
      {server2, evaluator2} = pry_session(config.test, "Y\niex_context")
      client = pry_request([server1, server2])

      send(evaluator1, :run)
      send(evaluator2, :run)
      assert Task.await(server1) =~ "undefined function iex_context"
      assert Task.await(server2) =~ ":inside_pry"

      assert Task.await(client) == :ok
    end
  end

  describe ".iex.exs" do
    test "loads .iex.exs" do
      :ok = File.write!("/tmp/valid_dot_iex.exs", valid_dot_iex())

      assert capture_io(fn ->
               IEx.Server.run(dot_iex_path: "/tmp/valid_dot_iex.exs")
             end) =~ "valid .iex.exs"
    end

    test "syntax error in .iex.exs won't exit" do
      :ok = File.write!("/tmp/syntax_error_dot_iex.exs", syntax_error_dot_iex())

      assert capture_io(fn ->
               IEx.Server.run(dot_iex_path: "/tmp/syntax_error_dot_iex.exs")
             end) =~ "(TokenMissingError)"
    end

    test "runtime exception in .iex.exs won't exit" do
      :ok = File.write!("/tmp/runtime_exception_dot_iex.exs", runtime_exception_dot_iex())

      assert capture_io(fn ->
               IEx.Server.run(dot_iex_path: "/tmp/runtime_exception_dot_iex.exs")
             end) =~ "(RuntimeError) this should not cause Elixir to halt"
    end

    defp valid_dot_iex do
      """
      IO.puts("valid .iex.exs")
      """
    end

    defp syntax_error_dot_iex do
      """
      uh oh i'm new at this
      """
    end

    defp runtime_exception_dot_iex do
      """
      raise("this should not cause Elixir to halt")
      """
    end
  end

  # Helpers

  defp pry_session(name, session) do
    task =
      Task.async(fn ->
        capture_iex("""
        send(#{inspect(name)}, {:running, self()})
        receive do: (:run -> :ok)
        #{session}
        """)
      end)

    assert_receive {:running, evaluator}
    {task, evaluator}
  end

  defp pry_request(sessions) do
    :erlang.trace(Process.whereis(IEx.Broker), true, [:receive, tracer: self()])
    patterns = for %{pid: pid} <- sessions, do: {[:_, pid, :_], [], []}
    :erlang.trace_pattern(:receive, patterns, [])

    task =
      Task.async(fn ->
        iex_context = :inside_pry
        IEx.pry()
      end)

    for _ <- sessions do
      assert_receive {:trace, _, :receive, {_, _, call}} when elem(call, 0) in [:accept, :refuse]
    end

    task
  after
    :erlang.trace(Process.whereis(IEx.Broker), false, [:receive, tracer: self()])
  end
end
