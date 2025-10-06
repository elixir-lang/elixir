# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

Code.require_file("../test_helper.exs", __DIR__)

defmodule Registry.DuplicateTest do
  use ExUnit.Case,
    async: true,
    parameterize:
      for(
        keys <- [:duplicate, {:duplicate, :pid}, {:duplicate, :key}],
        partitions <- [1, 8],
        do: %{keys: keys, partitions: partitions}
      )

  setup config do
    keys = config.keys
    partitions = config.partitions

    listeners =
      List.wrap(config[:base_listener]) |> Enum.map(&:"#{&1}_#{partitions}_#{inspect(keys)}")

    name = :"#{config.test}_#{partitions}_#{inspect(keys)}"
    opts = [keys: config.keys, name: name, partitions: partitions, listeners: listeners]
    {:ok, _} = start_supervised({Registry, opts})
    %{registry: name, listeners: listeners}
  end

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
    assert Registry.values(registry, "hello", self()) == [:value]

    assert {:ok, pid} = Registry.register(registry, "hello", :value)
    assert is_pid(pid)
    assert Registry.keys(registry, self()) == ["hello", "hello"]
    assert Registry.values(registry, "hello", self()) == [:value, :value]

    {:ok, pid} = Registry.register(registry, "world", :value)
    assert is_pid(pid)
    assert Registry.keys(registry, self()) |> Enum.sort() == ["hello", "hello", "world"]
  end

  test "has duplicate registrations across processes", %{registry: registry} do
    {_, task} = register_task(registry, "hello", :world)
    assert Registry.keys(registry, self()) == []
    assert Registry.keys(registry, task) == ["hello"]
    assert Registry.values(registry, "hello", self()) == []
    assert Registry.values(registry, "hello", task) == [:world]

    assert {:ok, _pid} = Registry.register(registry, "hello", :value)
    assert Registry.keys(registry, self()) == ["hello"]
    assert Registry.values(registry, "hello", self()) == [:value]
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

  test "unregisters by key", %{registry: registry} do
    {:ok, _} = Registry.register(registry, "hello", :value)
    {:ok, _} = Registry.register(registry, "hello", :value)
    {:ok, _} = Registry.register(registry, "world", :value)
    assert Registry.keys(registry, self()) |> Enum.sort() == ["hello", "hello", "world"]

    :ok = Registry.unregister(registry, "hello")
    assert Registry.keys(registry, self()) == ["world"]

    :ok = Registry.unregister(registry, "world")
    assert Registry.keys(registry, self()) == []
  end

  test "unregisters with no entries", %{registry: registry} do
    assert Registry.unregister(registry, "hello") == :ok
  end

  test "unregisters with tricky keys", %{registry: registry} do
    {:ok, _} = Registry.register(registry, :_, :foo)
    {:ok, _} = Registry.register(registry, :_, :bar)
    {:ok, _} = Registry.register(registry, "hello", "a")
    {:ok, _} = Registry.register(registry, "hello", "b")

    Registry.unregister(registry, :_)
    assert Registry.keys(registry, self()) |> Enum.sort() == ["hello", "hello"]
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

  @tag base_listener: :unique_listener
  test "allows listeners", %{registry: registry, listeners: [listener]} do
    Process.register(self(), listener)
    {_, task} = register_task(registry, "hello", :world)
    assert_received {:register, ^registry, "hello", ^task, :world}

    self = self()
    {:ok, _} = Registry.register(registry, "hello", :value)
    assert_received {:register, ^registry, "hello", ^self, :value}

    :ok = Registry.unregister(registry, "hello")
    assert_received {:unregister, ^registry, "hello", ^self}
  after
    Process.unregister(listener)
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

  test "count_select supports match specs", %{registry: registry} do
    value = {1, :atom, 1}
    {:ok, _} = Registry.register(registry, "hello", value)
    assert 1 == Registry.count_select(registry, [{{:_, :_, value}, [], [true]}])
    assert 1 == Registry.count_select(registry, [{{"hello", :_, :_}, [], [true]}])
    assert 1 == Registry.count_select(registry, [{{:_, :_, {1, :atom, :_}}, [], [true]}])
    assert 1 == Registry.count_select(registry, [{{:_, :_, {:"$1", :_, :"$1"}}, [], [true]}])
    assert 0 == Registry.count_select(registry, [{{"hello", :_, nil}, [], [true]}])

    value2 = %{a: "a", b: "b"}
    {:ok, _} = Registry.register(registry, "world", value2)
    assert 1 == Registry.count_select(registry, [{{"world", :_, :_}, [], [true]}])
  end

  test "count_select supports guard conditions", %{registry: registry} do
    value = {1, :atom, 2}
    {:ok, _} = Registry.register(registry, "hello", value)

    assert 1 ==
             Registry.count_select(registry, [
               {{:_, :_, {:_, :"$1", :_}}, [{:is_atom, :"$1"}], [true]}
             ])

    assert 1 ==
             Registry.count_select(registry, [
               {{:_, :_, {:_, :_, :"$1"}}, [{:>, :"$1", 1}], [true]}
             ])

    assert 0 ==
             Registry.count_select(registry, [
               {{:_, :_, {:_, :_, :"$1"}}, [{:>, :"$1", 2}], [true]}
             ])
  end

  test "count_select allows multiple specs", %{registry: registry} do
    {:ok, _} = Registry.register(registry, "hello", :value)
    {:ok, _} = Registry.register(registry, "world", :value)

    assert 2 ==
             Registry.count_select(registry, [
               {{"hello", :_, :_}, [], [true]},
               {{"world", :_, :_}, [], [true]}
             ])
  end

  test "rejects invalid tuple syntax", %{partitions: partitions} do
    name = :"test_invalid_tuple_#{partitions}"

    assert_raise ArgumentError, ~r/expected :keys to be given and be one of/, fn ->
      Registry.start_link(keys: {:duplicate, :invalid}, name: name, partitions: partitions)
    end
  end

  test "update_value is not supported", %{registry: registry} do
    assert_raise ArgumentError, ~r/Registry.update_value\/3 is not supported/, fn ->
      Registry.update_value(registry, "hello", fn val -> val end)
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
end
