Code.require_file "test_helper.exs", __DIR__

defmodule ApplicationTest do
  use ExUnit.Case, async: true

  import PathHelpers

  test "application environment" do
    assert Application.get_env(:elixir, :unknown) == nil
    assert Application.get_env(:elixir, :unknown, :default) == :default
    assert Application.fetch_env(:elixir, :unknown) == :error
    assert_raise ArgumentError, fn ->
      Application.fetch_env!(:elixir, :unknown)
    end

    assert Application.put_env(:elixir, :unknown, :known) == :ok
    assert Application.fetch_env(:elixir, :unknown) == {:ok, :known}
    assert Application.fetch_env!(:elixir, :unknown) == :known
    assert Application.get_env(:elixir, :unknown, :default) == :known
    assert {:unknown, :known} in Application.get_all_env(:elixir)

    assert Application.delete_env(:elixir, :unknown) == :ok
    assert Application.get_env(:elixir, :unknown, :default) == :default
  end

  test "loaded and started applications" do
    started = Application.started_applications
    assert is_list(started)
    assert {:elixir, 'elixir', _} = List.keyfind(started, :elixir, 0)

    started_timeout = Application.started_applications(7000)
    assert is_list(started_timeout)
    assert {:elixir, 'elixir', _} = List.keyfind(started_timeout, :elixir, 0)

    loaded = Application.loaded_applications
    assert is_list(loaded)
    assert {:elixir, 'elixir', _} = List.keyfind(loaded, :elixir, 0)
  end

  test "application specification" do
    assert is_list Application.spec(:elixir)
    assert Application.spec(:unknown) == nil
    assert Application.spec(:unknown, :description) == nil

    assert Application.spec(:elixir, :description) == 'elixir'
    assert_raise FunctionClauseError, fn -> Application.spec(:elixir, :unknown) end
  end

  test "application module" do
    assert Application.get_application(String) == :elixir
    assert Application.get_application(__MODULE__) == nil
    assert Application.get_application(__MODULE__.Unknown) == nil
  end

  test "application directory" do
    root = Path.expand("../../../..", __DIR__)
    assert normalize_app_dir(Application.app_dir(:elixir)) ==
           normalize_app_dir(Path.join(root, "bin/../lib/elixir"))
    assert normalize_app_dir(Application.app_dir(:elixir, "priv")) ==
           normalize_app_dir(Path.join(root, "bin/../lib/elixir/priv"))
    assert normalize_app_dir(Application.app_dir(:elixir, ["priv", "foo"])) ==
           normalize_app_dir(Path.join(root, "bin/../lib/elixir/priv/foo"))

    assert_raise ArgumentError, fn ->
      Application.app_dir(:unknown)
    end
  end

  if windows?() do
    defp normalize_app_dir(path) do
      path |> String.downcase |> Path.expand
    end
  else
    defp normalize_app_dir(path) do
      path |> String.downcase
    end
  end
end
