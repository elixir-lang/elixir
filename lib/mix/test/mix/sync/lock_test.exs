Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Sync.LockTest do
  use ExUnit.Case, async: true

  alias Mix.Sync.Lock

  @lock_key Atom.to_string(__MODULE__)

  test "executes functions" do
    assert Lock.with_lock(@lock_key, fn -> :it_works! end) == :it_works!
    assert Lock.with_lock(@lock_key, fn -> :still_works! end) == :still_works!
  end

  test "releases lock on error" do
    assert_raise RuntimeError, fn ->
      Lock.with_lock(@lock_key, fn -> raise "oops" end)
    end

    assert Lock.with_lock(@lock_key, fn -> :still_works! end) == :still_works!
  end

  test "releases lock on exit" do
    {_pid, ref} =
      spawn_monitor(fn ->
        Lock.with_lock(@lock_key, fn -> Process.exit(self(), :kill) end)
      end)

    assert_receive {:DOWN, ^ref, _, _, _}
    assert Lock.with_lock(@lock_key, fn -> :still_works! end) == :still_works!
  end

  test "blocks until released" do
    parent = self()

    task =
      Task.async(fn ->
        Lock.with_lock(@lock_key, fn ->
          send(parent, :locked)
          assert_receive :will_lock
          :it_works!
        end)
      end)

    assert_receive :locked
    send(task.pid, :will_lock)
    assert Lock.with_lock(@lock_key, fn -> :still_works! end) == :still_works!
    assert Task.await(task) == :it_works!
  end

  @tag :capture_log
  test "blocks until released on error" do
    parent = self()

    {pid, ref} =
      spawn_monitor(fn ->
        Lock.with_lock(@lock_key, fn ->
          send(parent, :locked)
          assert_receive :will_lock
          raise "oops"
        end)
      end)

    assert_receive :locked
    send(pid, :will_lock)
    assert Lock.with_lock(@lock_key, fn -> :still_works! end) == :still_works!
    assert_receive {:DOWN, ^ref, _, _, _}
  end

  test "blocks until released on exit" do
    parent = self()

    {pid, ref} =
      spawn_monitor(fn ->
        Lock.with_lock(@lock_key, fn ->
          send(parent, :locked)
          assert_receive :will_not_lock
        end)
      end)

    assert_receive :locked
    Process.exit(pid, :kill)
    assert Lock.with_lock(@lock_key, fn -> :still_works! end) == :still_works!
    assert_receive {:DOWN, ^ref, _, _, _}
  end

  test "schedules and releases on exit" do
    assert Lock.with_lock(@lock_key, fn ->
             {pid, ref} =
               spawn_monitor(fn ->
                 Lock.with_lock(@lock_key, fn ->
                   raise "this will never be invoked"
                 end)
               end)

             Process.exit(pid, :kill)
             assert_receive {:DOWN, ^ref, _, _, :killed}
             :it_works!
           end) == :it_works!

    assert Lock.with_lock(@lock_key, fn -> :still_works! end) == :still_works!
  end

  @tag :tmp_dir
  test "property test with file access", %{tmp_dir: tmp_dir} do
    # Spawn N concurrent processes incrementing number in a file
    n = 10
    number_path = Path.join(tmp_dir, "number.txt")

    File.write!(number_path, "0")

    refs =
      for _ <- 1..n do
        spawn_monitor(fn ->
          Lock.with_lock(@lock_key, fn ->
            number = number_path |> File.read!() |> String.to_integer()
            new_number = number + 1
            File.write!(number_path, Integer.to_string(new_number))

            assert File.read!(number_path) == Integer.to_string(new_number)

            # Terminate without unlocking in random cases
            case Enum.random(1..2) do
              1 -> Process.exit(self(), :kill)
              2 -> :ok
            end
          end)
        end)
        |> elem(1)
      end

    await_monitors(refs)

    assert File.read!(number_path) == Integer.to_string(n)
  end

  test "lock can be acquired multiple times by the same process" do
    {_pid, ref} =
      spawn_monitor(fn ->
        Lock.with_lock(@lock_key, fn ->
          Lock.with_lock(@lock_key, fn ->
            Process.exit(self(), :kill)
          end)
        end)
      end)

    assert_receive {:DOWN, ^ref, _, _, _}

    assert Lock.with_lock(@lock_key, fn ->
             Lock.with_lock(@lock_key, fn ->
               :still_works!
             end)
           end) == :still_works!
  end

  test "calls :on_taken when the lock is held by a different process" do
    parent = self()

    {pid, ref} =
      spawn_monitor(fn ->
        Lock.with_lock(@lock_key, fn ->
          send(parent, :locked)
          assert_receive :will_lock
        end)
      end)

    assert_receive :locked

    on_taken = fn os_pid ->
      send(pid, :will_lock)
      send(self(), {:on_taken_called, os_pid})
    end

    assert Lock.with_lock(@lock_key, fn -> :it_works! end, on_taken: on_taken) == :it_works!

    os_pid = System.pid()
    assert_receive {:on_taken_called, ^os_pid}

    assert_receive {:DOWN, ^ref, _, _, _}
  end

  defp await_monitors([]), do: :ok

  defp await_monitors(refs) do
    receive do
      {:DOWN, ref, _, _, _} -> await_monitors(refs -- [ref])
    end
  end
end
