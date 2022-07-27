Code.require_file("../test_helper.exs", __DIR__)

defmodule Mix.StateTest do
  use ExUnit.Case, async: true

  describe "lock" do
    test "executes functions" do
      assert Mix.State.lock(:key, fn -> :it_works! end) == :it_works!
      assert Mix.State.lock(:key, fn -> :still_works! end) == :still_works!
    end

    test "releases lock on error" do
      assert_raise RuntimeError, fn ->
        Mix.State.lock(:key, fn -> raise "oops" end)
      end

      assert Mix.State.lock(:key, fn -> :still_works! end) == :still_works!
    end

    test "releases lock on exit" do
      {_pid, ref} =
        spawn_monitor(fn ->
          Mix.State.lock(:key, fn -> Process.exit(self(), :kill) end)
        end)

      assert_receive {:DOWN, ^ref, _, _, _}
      assert Mix.State.lock(:key, fn -> :still_works! end) == :still_works!
    end

    test "blocks until released" do
      parent = self()

      task =
        Task.async(fn ->
          Mix.State.lock(:key, fn ->
            send(parent, :locked)
            assert_receive :will_lock
            :it_works!
          end)
        end)

      assert_receive :locked
      send(task.pid, :will_lock)
      assert Mix.State.lock(:key, fn -> :still_works! end) == :still_works!
      assert Task.await(task) == :it_works!
    end

    test "blocks until released on error" do
      parent = self()

      {pid, ref} =
        spawn_monitor(fn ->
          Mix.State.lock(:key, fn ->
            send(parent, :locked)
            assert_receive :will_lock
            raise "oops"
          end)
        end)

      assert_receive :locked
      send(pid, :will_lock)
      assert Mix.State.lock(:key, fn -> :still_works! end) == :still_works!
      assert_receive {:DOWN, ^ref, _, _, _}
    end

    test "blocks until released on exit" do
      parent = self()

      {pid, ref} =
        spawn_monitor(fn ->
          Mix.State.lock(:key, fn ->
            send(parent, :locked)
            assert_receive :will_not_lock
          end)
        end)

      assert_receive :locked
      Process.exit(pid, :kill)
      assert Mix.State.lock(:key, fn -> :still_works! end) == :still_works!
      assert_receive {:DOWN, ^ref, _, _, _}
    end

    test "scheduls and releases on exit" do
      assert Mix.State.lock(:key, fn ->
               {pid, ref} =
                 spawn_monitor(fn ->
                   Mix.State.lock(:key, fn ->
                     raise "this will never be invoked"
                   end)
                 end)

               Process.exit(pid, :kill)
               assert_receive {:DOWN, ^ref, _, _, :killed}
               :it_works!
             end) == :it_works!

      assert Mix.State.lock(:key, fn -> :still_works! end) == :still_works!
    end
  end
end
