Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Sync.PubSubTest do
  use ExUnit.Case, async: true

  alias Mix.Sync.PubSub

  @pubsub_key inspect(__MODULE__)

  test "delivers broadcast to multiple subscribers for the same key" do
    parent = self()

    spawn_link(fn ->
      PubSub.subscribe(@pubsub_key)
      send(parent, :subscribed1)
      assert_receive %{event: "event1"}
      assert_receive %{event: "event2"}
      send(parent, :done1)
    end)

    spawn_link(fn ->
      PubSub.subscribe(@pubsub_key)
      send(parent, :subscribed2)
      assert_receive %{event: "event1"}
      assert_receive %{event: "event2"}
      send(parent, :done2)
    end)

    assert_receive :subscribed1
    assert_receive :subscribed2

    PubSub.broadcast(@pubsub_key, %{event: "event1"})
    PubSub.broadcast(@pubsub_key, %{event: "event2"})

    assert_receive :done1
    assert_receive :done2
  end

  test "delivers broadcast to subscribers for different keys" do
    parent = self()

    spawn_link(fn ->
      PubSub.subscribe([@pubsub_key, "1"])
      send(parent, :subscribed1)
      assert_receive %{event: "event1"}
      assert_receive %{event: "event3"}
      refute_received %{event: "event2"}
      send(parent, :done1)
    end)

    spawn_link(fn ->
      PubSub.subscribe([@pubsub_key, "2"])
      send(parent, :subscribed2)
      assert_receive %{event: "event2"}
      assert_receive %{event: "event3"}
      refute_received %{event: "event1"}
      send(parent, :done2)
    end)

    assert_receive :subscribed1
    assert_receive :subscribed2

    PubSub.broadcast([@pubsub_key, "1"], %{event: "event1"})
    PubSub.broadcast([@pubsub_key, "2"], %{event: "event2"})
    PubSub.broadcast([@pubsub_key, "1"], %{event: "event3"})
    PubSub.broadcast([@pubsub_key, "2"], %{event: "event3"})

    assert_receive :done1
    assert_receive :done2
  end

  test "evaluates lazy message only if there are subscribers" do
    lazy_message = fn ->
      send(self(), :lazy1)
      %{event: "event1"}
    end

    PubSub.broadcast([@pubsub_key, "lazy"], lazy_message)

    PubSub.subscribe([@pubsub_key, "lazy"])

    lazy_message = fn ->
      send(self(), :lazy2)
      %{event: "event2"}
    end

    PubSub.broadcast([@pubsub_key, "lazy"], lazy_message)

    refute_received :lazy1
    assert_received :lazy2
  end
end
