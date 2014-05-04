Code.require_file "test_helper.exs", __DIR__

defmodule ApplicationTest do
  use ExUnit.Case, async: true

  test "application environment" do
    assert Application.get_env(:elixir, :unknown) == nil
    assert Application.get_env(:elixir, :unknown, :default) == :default
    assert Application.fetch_env(:elixir, :unknown) == :error

    assert Application.put_env(:elixir, :unknown, :known) == :ok
    assert Application.fetch_env(:elixir, :unknown) == {:ok, :known}
    assert Application.get_env(:elixir, :unknown, :default) == :known

    assert Application.delete_env(:elixir, :unknown) == :ok
    assert Application.get_env(:elixir, :unknown, :default) == :default
  end

  test "application directory" do
    root = Path.expand("../../../..", __DIR__)
    assert Application.app_dir(:elixir) ==
           Path.join(root, "bin/../lib/elixir")
    assert Application.app_dir(:elixir, "priv") ==
           Path.join(root, "bin/../lib/elixir/priv")

    assert_raise ArgumentError, fn ->
      Application.app_dir(:unknown)
    end
  end
end
