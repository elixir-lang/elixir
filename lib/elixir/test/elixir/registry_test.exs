Code.require_file("test_helper.exs", __DIR__)

defmodule RegistryTest do
  use ExUnit.Case, async: true
  doctest Registry, except: [:moduledoc]

  setup config do
    keys = config[:keys] || :unique
    partitions = config[:partitions] || 1
    listeners = List.wrap(config[:listener])
    opts = [keys: keys, name: config.test, partitions: partitions, listeners: listeners]
    {:ok, _} = start_supervised({Registry, opts})
    {:ok, %{registry: config.test, partitions: partitions}}
  end

  for {describe, partitions} <- ["with 1 partition": 1, "with 8 partitions": 8] do
    describe "unique #{describe}" do
      @describetag keys: :unique, partitions: partitions

      test "starts configured number of partitions", %{registry: registry, partitions: partitions} do
        assert length(Supervisor.which_children(registry)) == partitions
      end

      test "counts 0 keys in an empty registry", %{registry: registry} do
        assert 0 == Registry.count(registry)
      end

      test "counts the number of keys in a registry", %{registry: registry} do
        {:ok, _} = Registry.register(registry, "hello", :value)
        {:ok, _} = Registry.register(registry, "world", :value)

        assert 2 == Registry.count(registry)
      end

      test "has unique registrations", %{registry: registry} do
        {:ok, pid} = Registry.register(registry, "hello", :value)
        assert is_pid(pid)
        assert Registry.keys(registry, self()) == ["hello"]

        assert {:error, {:already_registered, pid}} = Registry.register(registry, "hello", :value)
        assert pid == self()
        assert Registry.keys(registry, self()) == ["hello"]

        {:ok, pid} = Registry.register(registry, "world", :value)
        assert is_pid(pid)
        assert Registry.keys(registry, self()) |> Enum.sort() == ["hello", "world"]
      end

      test "has unique registrations across processes", %{registry: registry} do
        {_, task} = register_task(registry, "hello", :value)
        Process.link(Process.whereis(registry))

        assert {:error, {:already_registered, ^task}} =
                 Registry.register(registry, "hello", :recent)

        assert Registry.keys(registry, self()) == []
        {:links, links} = Process.info(self(), :links)
        assert Process.whereis(registry) in links
      end

      test "has unique registrations even if partition is delayed", %{registry: registry} do
        {owner, task} = register_task(registry, "hello", :value)

        assert Registry.register(registry, "hello", :other) ==
                 {:error, {:already_registered, task}}

        :sys.suspend(owner)
        kill_and_assert_down(task)
        Registry.register(registry, "hello", :other)
        assert Registry.lookup(registry, "hello") == [{self(), :other}]
      end

      test "supports match patterns", %{registry: registry} do
        value = {1, :atom, 1}
        {:ok, _} = Registry.register(registry, "hello", value)
        assert Registry.match(registry, "hello", {1, :_, :_}) == [{self(), value}]
        assert Registry.match(registry, "hello", {1.0, :_, :_}) == []
        assert Registry.match(registry, "hello", {:_, :atom, :_}) == [{self(), value}]
        assert Registry.match(registry, "hello", {:"$1", :_, :"$1"}) == [{self(), value}]
        assert Registry.match(registry, "hello", :_) == [{self(), value}]
        assert Registry.match(registry, :_, :_) == []

        value2 = %{a: "a", b: "b"}
        {:ok, _} = Registry.register(registry, "world", value2)
        assert Registry.match(registry, "world", %{b: "b"}) == [{self(), value2}]
      end

      test "supports guard conditions", %{registry: registry} do
        value = {1, :atom, 2}
        {:ok, _} = Registry.register(registry, "hello", value)

        assert Registry.match(registry, "hello", {:_, :_, :"$1"}, [{:>, :"$1", 1}]) ==
                 [{self(), value}]

        assert Registry.match(registry, "hello", {:_, :_, :"$1"}, [{:>, :"$1", 2}]) == []

        assert Registry.match(registry, "hello", {:_, :"$1", :_}, [{:is_atom, :"$1"}]) ==
                 [{self(), value}]
      end

      test "count_match supports match patterns", %{registry: registry} do
        value = {1, :atom, 1}
        {:ok, _} = Registry.register(registry, "hello", value)
        assert 1 == Registry.count_match(registry, "hello", {1, :_, :_})
        assert 0 == Registry.count_match(registry, "hello", {1.0, :_, :_})
        assert 1 == Registry.count_match(registry, "hello", {:_, :atom, :_})
        assert 1 == Registry.count_match(registry, "hello", {:"$1", :_, :"$1"})
        assert 1 == Registry.count_match(registry, "hello", :_)
        assert 0 == Registry.count_match(registry, :_, :_)

        value2 = %{a: "a", b: "b"}
        {:ok, _} = Registry.register(registry, "world", value2)
        assert 1 == Registry.count_match(registry, "world", %{b: "b"})
      end

      test "count_match supports guard conditions", %{registry: registry} do
        value = {1, :atom, 2}
        {:ok, _} = Registry.register(registry, "hello", value)

        assert 1 == Registry.count_match(registry, "hello", {:_, :_, :"$1"}, [{:>, :"$1", 1}])
        assert 0 == Registry.count_match(registry, "hello", {:_, :_, :"$1"}, [{:>, :"$1", 2}])
        assert 1 == Registry.count_match(registry, "hello", {:_, :"$1", :_}, [{:is_atom, :"$1"}])
      end

      test "unregister_match supports patterns", %{registry: registry} do
        value = {1, :atom, 1}
        {:ok, _} = Registry.register(registry, "hello", value)

        Registry.unregister_match(registry, "hello", {2, :_, :_})
        assert Registry.lookup(registry, "hello") == [{self(), value}]
        Registry.unregister_match(registry, "hello", {1.0, :_, :_})
        assert Registry.lookup(registry, "hello") == [{self(), value}]
        Registry.unregister_match(registry, "hello", {:_, :atom, :_})
        assert Registry.lookup(registry, "hello") == []
      end

      test "unregister_match supports guards", %{registry: registry} do
        value = {1, :atom, 1}
        {:ok, _} = Registry.register(registry, "hello", value)

        Registry.unregister_match(registry, "hello", {:"$1", :_, :_}, [{:<, :"$1", 2}])
        assert Registry.lookup(registry, "hello") == []
      end

      test "unregister_match supports tricky keys", %{registry: registry} do
        {:ok, _} = Registry.register(registry, :_, :foo)
        {:ok, _} = Registry.register(registry, "hello", "b")

        Registry.unregister_match(registry, :_, :foo)
        assert Registry.lookup(registry, :_) == []

        assert Registry.keys(registry, self()) |> Enum.sort() == ["hello"]
      end

      test "compares using ===", %{registry: registry} do
        {:ok, _} = Registry.register(registry, 1.0, :value)
        {:ok, _} = Registry.register(registry, 1, :value)
        assert Registry.keys(registry, self()) |> Enum.sort() == [1, 1.0]
      end

      test "updates current process value", %{registry: registry} do
        assert Registry.update_value(registry, "hello", &raise/1) == :error
        register_task(registry, "hello", :value)
        assert Registry.update_value(registry, "hello", &raise/1) == :error

        Registry.register(registry, "world", 1)
        assert Registry.lookup(registry, "world") == [{self(), 1}]
        assert Registry.update_value(registry, "world", &(&1 + 1)) == {2, 1}
        assert Registry.lookup(registry, "world") == [{self(), 2}]
      end

      test "dispatches to a single key", %{registry: registry} do
        fun = fn _ -> raise "will never be invoked" end
        assert Registry.dispatch(registry, "hello", fun) == :ok

        {:ok, _} = Registry.register(registry, "hello", :value)

        fun = fn [{pid, value}] -> send(pid, {:dispatch, value}) end
        assert Registry.dispatch(registry, "hello", fun)

        assert_received {:dispatch, :value}
      end

      test "allows process unregistering", %{registry: registry} do
        :ok = Registry.unregister(registry, "hello")

        {:ok, _} = Registry.register(registry, "hello", :value)
        {:ok, _} = Registry.register(registry, "world", :value)
        assert Registry.keys(registry, self()) |> Enum.sort() == ["hello", "world"]

        :ok = Registry.unregister(registry, "hello")
        assert Registry.keys(registry, self()) == ["world"]

        :ok = Registry.unregister(registry, "world")
        assert Registry.keys(registry, self()) == []
      end

      test "allows unregistering with no entries", %{registry: registry} do
        assert Registry.unregister(registry, "hello") == :ok
      end

      @tag listener: :"unique_listener_#{partitions}"
      test "allows listeners", %{registry: registry, listener: listener} do
        Process.register(self(), listener)
        {_, task} = register_task(registry, "hello", :world)
        assert_received {:register, ^registry, "hello", ^task, :world}

        self = self()
        {:ok, _} = Registry.register(registry, "world", :value)
        assert_received {:register, ^registry, "world", ^self, :value}

        :ok = Registry.unregister(registry, "world")
        assert_received {:unregister, ^registry, "world", ^self}
      end

      test "links and unlinks on register/unregister", %{registry: registry} do
        {:ok, pid} = Registry.register(registry, "hello", :value)
        {:links, links} = Process.info(self(), :links)
        assert pid in links

        {:ok, pid} = Registry.register(registry, "world", :value)
        {:links, links} = Process.info(self(), :links)
        assert pid in links

        :ok = Registry.unregister(registry, "hello")
        {:links, links} = Process.info(self(), :links)
        assert pid in links

        :ok = Registry.unregister(registry, "world")
        {:links, links} = Process.info(self(), :links)
        refute pid in links
      end

      test "raises on unknown registry name" do
        assert_raise ArgumentError, ~r/unknown registry/, fn ->
          Registry.register(:unknown, "hello", :value)
        end
      end

      test "via callbacks", %{registry: registry} do
        name = {:via, Registry, {registry, "hello"}}

        # register_name
        {:ok, pid} = Agent.start_link(fn -> 0 end, name: name)

        # send
        assert Agent.update(name, &(&1 + 1)) == :ok

        # whereis_name
        assert Agent.get(name, & &1) == 1

        # unregister_name
        assert {:error, _} = Agent.start(fn -> raise "oops" end)

        # errors
        assert {:error, {:already_started, ^pid}} = Agent.start(fn -> 0 end, name: name)
      end

      test "uses value provided in via", %{registry: registry} do
        name = {:via, Registry, {registry, "hello", :value}}
        {:ok, pid} = Agent.start_link(fn -> 0 end, name: name)
        assert Registry.lookup(registry, "hello") == [{pid, :value}]
      end

      test "empty list for empty registry", %{registry: registry} do
        assert Registry.select(registry, [{{:_, :_, :_}, [], [:"$_"]}]) == []
      end

      test "select all", %{registry: registry} do
        name = {:via, Registry, {registry, "hello"}}
        {:ok, pid} = Agent.start_link(fn -> 0 end, name: name)
        {:ok, _} = Registry.register(registry, "world", :value)

        assert Registry.select(registry, [{{:"$1", :"$2", :"$3"}, [], [{{:"$1", :"$2", :"$3"}}]}])
               |> Enum.sort() ==
                 [{"hello", pid, nil}, {"world", self(), :value}]
      end

      test "select supports full match specs", %{registry: registry} do
        value = {1, :atom, 1}
        {:ok, _} = Registry.register(registry, "hello", value)

        assert [{"hello", self(), value}] ==
                 Registry.select(registry, [
                   {{"hello", :"$2", :"$3"}, [], [{{"hello", :"$2", :"$3"}}]}
                 ])

        assert [{"hello", self(), value}] ==
                 Registry.select(registry, [
                   {{:"$1", self(), :"$3"}, [], [{{:"$1", self(), :"$3"}}]}
                 ])

        assert [{"hello", self(), value}] ==
                 Registry.select(registry, [
                   {{:"$1", :"$2", value}, [], [{{:"$1", :"$2", {value}}}]}
                 ])

        assert [] ==
                 Registry.select(registry, [
                   {{"world", :"$2", :"$3"}, [], [{{"world", :"$2", :"$3"}}]}
                 ])

        assert [] == Registry.select(registry, [{{:"$1", :"$2", {1.0, :_, :_}}, [], [:"$_"]}])

        assert [{"hello", self(), value}] ==
                 Registry.select(registry, [
                   {{:"$1", :"$2", {:"$3", :atom, :"$4"}}, [],
                    [{{:"$1", :"$2", {{:"$3", :atom, :"$4"}}}}]}
                 ])

        assert [{"hello", self(), {1, :atom, 1}}] ==
                 Registry.select(registry, [
                   {{:"$1", :"$2", {:"$3", :"$4", :"$3"}}, [],
                    [{{:"$1", :"$2", {{:"$3", :"$4", :"$3"}}}}]}
                 ])

        value2 = %{a: "a", b: "b"}
        {:ok, _} = Registry.register(registry, "world", value2)

        assert [:match] ==
                 Registry.select(registry, [{{"world", self(), %{b: "b"}}, [], [:match]}])

        assert ["hello", "world"] ==
                 Registry.select(registry, [{{:"$1", :_, :_}, [], [:"$1"]}]) |> Enum.sort()
      end

      test "select supports guard conditions", %{registry: registry} do
        value = {1, :atom, 2}
        {:ok, _} = Registry.register(registry, "hello", value)

        assert [{"hello", self(), {1, :atom, 2}}] ==
                 Registry.select(registry, [
                   {{:"$1", :"$2", {:"$3", :"$4", :"$5"}}, [{:>, :"$5", 1}],
                    [{{:"$1", :"$2", {{:"$3", :"$4", :"$5"}}}}]}
                 ])

        assert [] ==
                 Registry.select(registry, [
                   {{:_, :_, {:_, :_, :"$1"}}, [{:>, :"$1", 2}], [:"$_"]}
                 ])

        assert ["hello"] ==
                 Registry.select(registry, [
                   {{:"$1", :_, {:_, :"$2", :_}}, [{:is_atom, :"$2"}], [:"$1"]}
                 ])
      end

      test "select allows multiple specs", %{registry: registry} do
        {:ok, _} = Registry.register(registry, "hello", :value)
        {:ok, _} = Registry.register(registry, "world", :value)

        assert ["hello", "world"] ==
                 Registry.select(registry, [
                   {{"hello", :_, :_}, [], [{:element, 1, :"$_"}]},
                   {{"world", :_, :_}, [], [{:element, 1, :"$_"}]}
                 ])
                 |> Enum.sort()
      end

      test "raises on incorrect shape of match spec", %{registry: registry} do
        assert_raise ArgumentError, fn ->
          Registry.select(registry, [{:_, [], []}])
        end
      end
    end
  end

  for {describe, partitions} <- ["with 1 partition": 1, "with 8 partitions": 8] do
    describe "duplicate #{describe}" do
      @describetag keys: :duplicate, partitions: partitions

      test "starts configured number of partitions", %{registry: registry, partitions: partitions} do
        assert length(Supervisor.which_children(registry)) == partitions
      end

      test "counts 0 keys in an empty registry", %{registry: registry} do
        assert 0 == Registry.count(registry)
      end

      test "counts the number of keys in a registry", %{registry: registry} do
        {:ok, _} = Registry.register(registry, "hello", :value)
        {:ok, _} = Registry.register(registry, "hello", :value)

        assert 2 == Registry.count(registry)
      end

      test "has duplicate registrations", %{registry: registry} do
        {:ok, pid} = Registry.register(registry, "hello", :value)
        assert is_pid(pid)
        assert Registry.keys(registry, self()) == ["hello"]

        assert {:ok, pid} = Registry.register(registry, "hello", :value)
        assert is_pid(pid)
        assert Registry.keys(registry, self()) == ["hello", "hello"]

        {:ok, pid} = Registry.register(registry, "world", :value)
        assert is_pid(pid)
        assert Registry.keys(registry, self()) |> Enum.sort() == ["hello", "hello", "world"]
      end

      test "compares using matches", %{registry: registry} do
        {:ok, _} = Registry.register(registry, 1.0, :value)
        {:ok, _} = Registry.register(registry, 1, :value)
        assert Registry.keys(registry, self()) |> Enum.sort() == [1, 1.0]
      end

      test "dispatches to multiple keys in serial", %{registry: registry} do
        Process.flag(:trap_exit, true)
        parent = self()

        fun = fn _ -> raise "will never be invoked" end
        assert Registry.dispatch(registry, "hello", fun, parallel: false) == :ok

        {:ok, _} = Registry.register(registry, "hello", :value1)
        {:ok, _} = Registry.register(registry, "hello", :value2)
        {:ok, _} = Registry.register(registry, "world", :value3)

        fun = fn entries ->
          assert parent == self()
          for {pid, value} <- entries, do: send(pid, {:dispatch, value})
        end

        assert Registry.dispatch(registry, "hello", fun, parallel: false)

        assert_received {:dispatch, :value1}
        assert_received {:dispatch, :value2}
        refute_received {:dispatch, :value3}

        fun = fn entries ->
          assert parent == self()
          for {pid, value} <- entries, do: send(pid, {:dispatch, value})
        end

        assert Registry.dispatch(registry, "world", fun, parallel: false)

        refute_received {:dispatch, :value1}
        refute_received {:dispatch, :value2}
        assert_received {:dispatch, :value3}

        refute_received {:EXIT, _, _}
      end

      test "dispatches to multiple keys in parallel", context do
        %{registry: registry, partitions: partitions} = context
        Process.flag(:trap_exit, true)
        parent = self()

        fun = fn _ -> raise "will never be invoked" end
        assert Registry.dispatch(registry, "hello", fun, parallel: true) == :ok

        {:ok, _} = Registry.register(registry, "hello", :value1)
        {:ok, _} = Registry.register(registry, "hello", :value2)
        {:ok, _} = Registry.register(registry, "world", :value3)

        fun = fn entries ->
          if partitions == 8 do
            assert parent != self()
          else
            assert parent == self()
          end

          for {pid, value} <- entries, do: send(pid, {:dispatch, value})
        end

        assert Registry.dispatch(registry, "hello", fun, parallel: true)

        assert_received {:dispatch, :value1}
        assert_received {:dispatch, :value2}
        refute_received {:dispatch, :value3}

        fun = fn entries ->
          if partitions == 8 do
            assert parent != self()
          else
            assert parent == self()
          end

          for {pid, value} <- entries, do: send(pid, {:dispatch, value})
        end

        assert Registry.dispatch(registry, "world", fun, parallel: true)

        refute_received {:dispatch, :value1}
        refute_received {:dispatch, :value2}
        assert_received {:dispatch, :value3}

        refute_received {:EXIT, _, _}
      end

      test "allows process unregistering", %{registry: registry} do
        {:ok, _} = Registry.register(registry, "hello", :value)
        {:ok, _} = Registry.register(registry, "hello", :value)
        {:ok, _} = Registry.register(registry, "world", :value)
        assert Registry.keys(registry, self()) |> Enum.sort() == ["hello", "hello", "world"]

        :ok = Registry.unregister(registry, "hello")
        assert Registry.keys(registry, self()) == ["world"]

        :ok = Registry.unregister(registry, "world")
        assert Registry.keys(registry, self()) == []
      end

      test "allows unregistering with no entries", %{registry: registry} do
        assert Registry.unregister(registry, "hello") == :ok
      end

      test "supports match patterns", %{registry: registry} do
        value1 = {1, :atom, 1}
        value2 = {2, :atom, 2}

        {:ok, _} = Registry.register(registry, "hello", value1)
        {:ok, _} = Registry.register(registry, "hello", value2)

        assert Registry.match(registry, "hello", {1, :_, :_}) == [{self(), value1}]
        assert Registry.match(registry, "hello", {1.0, :_, :_}) == []

        assert Registry.match(registry, "hello", {:_, :atom, :_}) |> Enum.sort() ==
                 [{self(), value1}, {self(), value2}]

        assert Registry.match(registry, "hello", {:"$1", :_, :"$1"}) |> Enum.sort() ==
                 [{self(), value1}, {self(), value2}]

        assert Registry.match(registry, "hello", {2, :_, :_}) == [{self(), value2}]
        assert Registry.match(registry, "hello", {2.0, :_, :_}) == []
      end

      test "supports guards", %{registry: registry} do
        value1 = {1, :atom, 1}
        value2 = {2, :atom, 2}

        {:ok, _} = Registry.register(registry, "hello", value1)
        {:ok, _} = Registry.register(registry, "hello", value2)

        assert Registry.match(registry, "hello", {:"$1", :_, :_}, [{:<, :"$1", 2}]) ==
                 [{self(), value1}]

        assert Registry.match(registry, "hello", {:"$1", :_, :_}, [{:>, :"$1", 3}]) == []

        assert Registry.match(registry, "hello", {:"$1", :_, :_}, [{:<, :"$1", 3}]) |> Enum.sort() ==
                 [{self(), value1}, {self(), value2}]

        assert Registry.match(registry, "hello", {:_, :"$1", :_}, [{:is_atom, :"$1"}])
               |> Enum.sort() == [{self(), value1}, {self(), value2}]
      end

      test "count_match supports match patterns", %{registry: registry} do
        value = {1, :atom, 1}
        {:ok, _} = Registry.register(registry, "hello", value)
        assert 1 == Registry.count_match(registry, "hello", {1, :_, :_})
        assert 0 == Registry.count_match(registry, "hello", {1.0, :_, :_})
        assert 1 == Registry.count_match(registry, "hello", {:_, :atom, :_})
        assert 1 == Registry.count_match(registry, "hello", {:"$1", :_, :"$1"})
        assert 1 == Registry.count_match(registry, "hello", :_)
        assert 0 == Registry.count_match(registry, :_, :_)

        value2 = %{a: "a", b: "b"}
        {:ok, _} = Registry.register(registry, "world", value2)
        assert 1 == Registry.count_match(registry, "world", %{b: "b"})
      end

      test "count_match supports guard conditions", %{registry: registry} do
        value = {1, :atom, 2}
        {:ok, _} = Registry.register(registry, "hello", value)

        assert 1 == Registry.count_match(registry, "hello", {:_, :_, :"$1"}, [{:>, :"$1", 1}])
        assert 0 == Registry.count_match(registry, "hello", {:_, :_, :"$1"}, [{:>, :"$1", 2}])
        assert 1 == Registry.count_match(registry, "hello", {:_, :"$1", :_}, [{:is_atom, :"$1"}])
      end

      test "unregister_match supports patterns", %{registry: registry} do
        value1 = {1, :atom, 1}
        value2 = {2, :atom, 2}

        {:ok, _} = Registry.register(registry, "hello", value1)
        {:ok, _} = Registry.register(registry, "hello", value2)

        Registry.unregister_match(registry, "hello", {2, :_, :_})
        assert Registry.lookup(registry, "hello") == [{self(), value1}]

        {:ok, _} = Registry.register(registry, "hello", value2)
        Registry.unregister_match(registry, "hello", {2.0, :_, :_})
        assert Registry.lookup(registry, "hello") == [{self(), value1}, {self(), value2}]
        Registry.unregister_match(registry, "hello", {:_, :atom, :_})
        assert Registry.lookup(registry, "hello") == []
      end

      test "unregister_match supports guards", %{registry: registry} do
        value1 = {1, :atom, 1}
        value2 = {2, :atom, 2}

        {:ok, _} = Registry.register(registry, "hello", value1)
        {:ok, _} = Registry.register(registry, "hello", value2)

        Registry.unregister_match(registry, "hello", {:"$1", :_, :_}, [{:<, :"$1", 2}])
        assert Registry.lookup(registry, "hello") == [{self(), value2}]
      end

      test "unregister_match supports tricky keys", %{registry: registry} do
        {:ok, _} = Registry.register(registry, :_, :foo)
        {:ok, _} = Registry.register(registry, :_, :bar)
        {:ok, _} = Registry.register(registry, "hello", "a")
        {:ok, _} = Registry.register(registry, "hello", "b")

        Registry.unregister_match(registry, :_, :foo)
        assert Registry.lookup(registry, :_) == [{self(), :bar}]

        assert Registry.keys(registry, self()) |> Enum.sort() == [:_, "hello", "hello"]
      end

      @tag listener: :"duplicate_listener_#{partitions}"
      test "allows listeners", %{registry: registry, listener: listener} do
        Process.register(self(), listener)
        {_, task} = register_task(registry, "hello", :world)
        assert_received {:register, ^registry, "hello", ^task, :world}

        self = self()
        {:ok, _} = Registry.register(registry, "hello", :value)
        assert_received {:register, ^registry, "hello", ^self, :value}

        :ok = Registry.unregister(registry, "hello")
        assert_received {:unregister, ^registry, "hello", ^self}
      end

      test "links and unlinks on register/unregister", %{registry: registry} do
        {:ok, pid} = Registry.register(registry, "hello", :value)
        {:links, links} = Process.info(self(), :links)
        assert pid in links

        {:ok, pid} = Registry.register(registry, "world", :value)
        {:links, links} = Process.info(self(), :links)
        assert pid in links

        :ok = Registry.unregister(registry, "hello")
        {:links, links} = Process.info(self(), :links)
        assert pid in links

        :ok = Registry.unregister(registry, "world")
        {:links, links} = Process.info(self(), :links)
        refute pid in links
      end

      test "raises on unknown registry name" do
        assert_raise ArgumentError, ~r/unknown registry/, fn ->
          Registry.register(:unknown, "hello", :value)
        end
      end

      test "raises if attempt to be used on via", %{registry: registry} do
        assert_raise ArgumentError, ":via is not supported for duplicate registries", fn ->
          name = {:via, Registry, {registry, "hello"}}
          Agent.start_link(fn -> 0 end, name: name)
        end
      end

      test "empty list for empty registry", %{registry: registry} do
        assert Registry.select(registry, [{{:_, :_, :_}, [], [:"$_"]}]) == []
      end

      test "select all", %{registry: registry} do
        {:ok, _} = Registry.register(registry, "hello", :value)
        {:ok, _} = Registry.register(registry, "hello", :value)

        assert Registry.select(registry, [{{:"$1", :"$2", :"$3"}, [], [{{:"$1", :"$2", :"$3"}}]}])
               |> Enum.sort() ==
                 [{"hello", self(), :value}, {"hello", self(), :value}]
      end

      test "select supports full match specs", %{registry: registry} do
        value = {1, :atom, 1}
        {:ok, _} = Registry.register(registry, "hello", value)

        assert [{"hello", self(), value}] ==
                 Registry.select(registry, [
                   {{"hello", :"$2", :"$3"}, [], [{{"hello", :"$2", :"$3"}}]}
                 ])

        assert [{"hello", self(), value}] ==
                 Registry.select(registry, [
                   {{:"$1", self(), :"$3"}, [], [{{:"$1", self(), :"$3"}}]}
                 ])

        assert [{"hello", self(), value}] ==
                 Registry.select(registry, [
                   {{:"$1", :"$2", value}, [], [{{:"$1", :"$2", {value}}}]}
                 ])

        assert [] ==
                 Registry.select(registry, [
                   {{"world", :"$2", :"$3"}, [], [{{"world", :"$2", :"$3"}}]}
                 ])

        assert [] == Registry.select(registry, [{{:"$1", :"$2", {1.0, :_, :_}}, [], [:"$_"]}])

        assert [{"hello", self(), value}] ==
                 Registry.select(registry, [
                   {{:"$1", :"$2", {:"$3", :atom, :"$4"}}, [],
                    [{{:"$1", :"$2", {{:"$3", :atom, :"$4"}}}}]}
                 ])

        assert [{"hello", self(), {1, :atom, 1}}] ==
                 Registry.select(registry, [
                   {{:"$1", :"$2", {:"$3", :"$4", :"$3"}}, [],
                    [{{:"$1", :"$2", {{:"$3", :"$4", :"$3"}}}}]}
                 ])

        value2 = %{a: "a", b: "b"}
        {:ok, _} = Registry.register(registry, "world", value2)

        assert [:match] ==
                 Registry.select(registry, [{{"world", self(), %{b: "b"}}, [], [:match]}])

        assert ["hello", "world"] ==
                 Registry.select(registry, [{{:"$1", :_, :_}, [], [:"$1"]}]) |> Enum.sort()
      end

      test "select supports guard conditions", %{registry: registry} do
        value = {1, :atom, 2}
        {:ok, _} = Registry.register(registry, "hello", value)

        assert [{"hello", self(), {1, :atom, 2}}] ==
                 Registry.select(registry, [
                   {{:"$1", :"$2", {:"$3", :"$4", :"$5"}}, [{:>, :"$5", 1}],
                    [{{:"$1", :"$2", {{:"$3", :"$4", :"$5"}}}}]}
                 ])

        assert [] ==
                 Registry.select(registry, [
                   {{:_, :_, {:_, :_, :"$1"}}, [{:>, :"$1", 2}], [:"$_"]}
                 ])

        assert ["hello"] ==
                 Registry.select(registry, [
                   {{:"$1", :_, {:_, :"$2", :_}}, [{:is_atom, :"$2"}], [:"$1"]}
                 ])
      end

      test "select allows multiple specs", %{registry: registry} do
        {:ok, _} = Registry.register(registry, "hello", :value)
        {:ok, _} = Registry.register(registry, "world", :value)

        assert ["hello", "world"] ==
                 Registry.select(registry, [
                   {{"hello", :_, :_}, [], [{:element, 1, :"$_"}]},
                   {{"world", :_, :_}, [], [{:element, 1, :"$_"}]}
                 ])
                 |> Enum.sort()
      end
    end
  end

  # Note: those tests relies on internals
  for keys <- [:unique, :duplicate] do
    describe "clean up #{keys} registry on process crash" do
      @describetag keys: keys

      @tag partitions: 8
      test "with 8 partitions", %{registry: registry} do
        {_, task1} = register_task(registry, "hello", :value)
        {_, task2} = register_task(registry, "world", :value)

        kill_and_assert_down(task1)
        kill_and_assert_down(task2)

        # pid might be in different partition to key so need to sync with all
        # partitions before checking ETS tables are empty.
        for i <- 0..7 do
          [{_, _, {partition, _}}] = :ets.lookup(registry, i)
          GenServer.call(partition, :sync)
        end

        for i <- 0..7 do
          [{_, key, {_, pid}}] = :ets.lookup(registry, i)
          assert :ets.tab2list(key) == []
          assert :ets.tab2list(pid) == []
        end
      end

      @tag partitions: 1
      test "with 1 partition", %{registry: registry} do
        {_, task1} = register_task(registry, "hello", :value)
        {_, task2} = register_task(registry, "world", :value)

        kill_and_assert_down(task1)
        kill_and_assert_down(task2)

        [{-1, {_, _, key, {partition, pid}, _}}] = :ets.lookup(registry, -1)
        GenServer.call(partition, :sync)
        assert :ets.tab2list(key) == []
        assert :ets.tab2list(pid) == []
      end
    end
  end

  test "child_spec/1 uses :name as :id" do
    assert %{id: :custom_name} = Registry.child_spec(name: :custom_name)
    assert %{id: Registry} = Registry.child_spec([])
  end

  test "raises if :name is missing" do
    assert_raise ArgumentError, ~r/expected :name option to be present/, fn ->
      Registry.start_link(keys: :unique)
    end
  end

  test "raises if :name is not an atom" do
    assert_raise ArgumentError, ~r/expected :name to be an atom, got/, fn ->
      Registry.start_link(keys: :unique, name: [])
    end
  end

  defp register_task(registry, key, value) do
    parent = self()

    {:ok, task} =
      Task.start(fn ->
        send(parent, Registry.register(registry, key, value))
        Process.sleep(:infinity)
      end)

    assert_receive {:ok, owner}
    {owner, task}
  end

  defp kill_and_assert_down(pid) do
    ref = Process.monitor(pid)
    Process.exit(pid, :kill)
    assert_receive {:DOWN, ^ref, _, _, _}
  end
end
