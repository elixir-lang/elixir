Code.require_file("../test_helper.exs", __DIR__)

defmodule IEx.ServerTest do
  use IEx.Case

  require IEx

  describe "options" do
    test "prefix" do
      assert capture_io(fn ->
               IEx.Server.run(prefix: "custom", dot_iex: "")
             end) =~ "custom(1)> "
    end

    test "env" do
      assert capture_iex("__ENV__.file", [], env: __ENV__) =~ "server_test.exs"
    end
  end

  describe "pry" do
    test "inside evaluator itself" do
      assert capture_iex("require IEx; IEx.pry()") =~ "Break reached"
    end

    test "outside of the evaluator with acceptance", config do
      Process.register(self(), config.test)

      {server, evaluator} = pry_session(config.test, "Y\niex_context")
      client = pry_request([server])
      send(evaluator, :run)

      assert Task.await(server) =~ ":inside_pry"
      assert Task.await(client) == {:ok, false}
    end

    test "outside of the evaluator with refusal", config do
      Process.register(self(), config.test)

      assert capture_io(:stderr, fn ->
               {server, evaluator} = pry_session(config.test, "N\niex_context")
               client = pry_request([server])
               send(evaluator, :run)

               assert Task.await(client) == {:error, :refused}
               assert Task.await(server) =~ "cannot compile code"
             end) =~ "undefined variable \"iex_context\""
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

      assert capture_io(:stderr, fn ->
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

               assert Task.await(client) == {:ok, false}
             end) =~ "undefined variable \"iex_context\""
    end

    test "outside of the evaluator with double refusal", config do
      Process.register(self(), config.test)

      assert capture_io(:stderr, fn ->
               {server1, evaluator1} = pry_session(config.test, "N\niex_context")
               {server2, evaluator2} = pry_session(config.test, "N\niex_context")
               client = pry_request([server1, server2])

               send(evaluator1, :run)
               send(evaluator2, :run)
               reply1 = Task.await(server1)
               reply2 = Task.await(server2)

               assert reply1 =~ "cannot compile code"
               assert reply2 =~ "cannot compile code"

               assert Task.await(client) == {:error, :refused}
             end) =~ "undefined variable \"iex_context\""
    end

    test "outside of the evaluator with acceptance and then refusal", config do
      Process.register(self(), config.test)

      assert capture_io(:stderr, fn ->
               {server1, evaluator1} = pry_session(config.test, "Y\niex_context")
               {server2, evaluator2} = pry_session(config.test, "N\niex_context")
               client = pry_request([server1, server2])

               send(evaluator1, :run)
               send(evaluator2, :run)
               assert Task.await(server1) =~ ":inside_pry"
               assert Task.await(server2) =~ "cannot compile code"

               assert Task.await(client) == {:ok, false}
             end) =~ "undefined variable \"iex_context\""
    end

    test "outside of the evaluator with refusal and then acceptance", config do
      Process.register(self(), config.test)

      assert capture_io(:stderr, fn ->
               {server1, evaluator1} = pry_session(config.test, "N\niex_context")
               {server2, evaluator2} = pry_session(config.test, "Y\niex_context")
               client = pry_request([server1, server2])

               send(evaluator1, :run)
               send(evaluator2, :run)
               assert Task.await(server1) =~ "cannot compile code"
               assert Task.await(server2) =~ ":inside_pry"

               assert Task.await(client) == {:ok, false}
             end) =~ "undefined variable \"iex_context\""
    end

    @tag :tmp_dir
    test "outside evaluator with .iex", %{tmp_dir: tmp_dir} = config do
      Process.register(self(), config.test)
      path = Path.join(tmp_dir, "dot-iex")
      File.write!(path, "my_variable = 144")

      assert capture_io(:stderr, fn ->
               {server, evaluator} =
                 pry_session(config.test, "Y\nmy_variable", dot_iex: path)

               client = pry_request([server])
               send(evaluator, :run)

               assert Task.await(server) =~ "** (CompileError)"

               assert Task.await(client) == {:ok, false}
             end) =~ "undefined variable \"my_variable\""
    end

    @tag :tmp_dir
    test "after pry outside evaluator .iex", %{tmp_dir: tmp_dir} = config do
      Process.register(self(), config.test)
      path = Path.join(tmp_dir, "dot-iex")
      File.write!(path, "my_variable = 144")

      {server, evaluator} =
        pry_session(config.test, "Y\ncontinue\nmy_variable", dot_iex: path)

      client = pry_request([server])
      send(evaluator, :run)

      assert Task.await(server) =~ "144"
      assert Task.await(client) == {:ok, false}

      {server, evaluator} = pry_session(config.test, "Y\nnext\nmy_variable", dot_iex: path)

      client = pry_request([server])
      send(evaluator, :run)

      assert Task.await(server) =~ "144"
      assert Task.await(client) == {:ok, true}
    end
  end

  # Helpers

  defp pry_session(name, session, server_options \\ []) do
    task =
      Task.async(fn ->
        capture_iex(
          """
          send(#{inspect(name)}, {:running, self()}) && receive do: (:run -> :ok)
          #{session}
          """,
          [],
          server_options
        )
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
