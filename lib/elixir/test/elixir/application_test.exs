Code.require_file("test_helper.exs", __DIR__)

defmodule ApplicationTest do
  use ExUnit.Case, async: true

  import PathHelpers
  import ExUnit.CaptureIO

  test "application environment" do
    assert_raise ArgumentError, ~r/because the application was not loaded\/started/, fn ->
      Application.fetch_env!(:unknown, :unknown)
    end

    assert_raise ArgumentError, ~r/because configuration at :unknown was not set/, fn ->
      Application.fetch_env!(:elixir, :unknown)
    end

    assert Application.get_env(:elixir, :unknown) == nil
    assert Application.get_env(:elixir, :unknown, :default) == :default
    assert Application.fetch_env(:elixir, :unknown) == :error

    assert Application.put_env(:elixir, :unknown, :known) == :ok
    assert Application.fetch_env(:elixir, :unknown) == {:ok, :known}
    assert Application.fetch_env!(:elixir, :unknown) == :known
    assert Application.get_env(:elixir, :unknown, :default) == :known
    assert {:unknown, :known} in Application.get_all_env(:elixir)

    assert Application.delete_env(:elixir, :unknown) == :ok
    assert Application.get_env(:elixir, :unknown, :default) == :default
  after
    Application.delete_env(:elixir, :unknown)
  end

  test "deprecated non-atom keys" do
    assert_deprecated(fn ->
      Application.put_env(:elixir, [:a, :b], :c)
    end)

    assert_deprecated(fn ->
      assert Application.get_env(:elixir, [:a, :b]) == :c
    end)

    assert_deprecated(fn ->
      assert Application.fetch_env!(:elixir, [:a, :b]) == :c
    end)
  after
    assert_deprecated(fn ->
      Application.delete_env(:elixir, [:a, :b])
    end)
  end

  defp assert_deprecated(fun) do
    assert capture_io(:stderr, fun) =~ ~r/passing non-atom as application env key is deprecated/
  end

  describe "compile environment" do
    test "invoked at compile time" do
      assert_raise ArgumentError, ~r/because the application was not loaded\/started/, fn ->
        compile_env!(:unknown, :unknown)
      end

      assert_received {:compile_env, :unknown, [:unknown], :error}

      assert_raise ArgumentError, ~r/because configuration at :unknown was not set/, fn ->
        compile_env!(:elixir, :unknown)
      end

      assert_received {:compile_env, :elixir, [:unknown], :error}

      assert compile_env(:elixir, :unknown) == nil
      assert_received {:compile_env, :elixir, [:unknown], :error}

      assert compile_env(:elixir, :unknown, :default) == :default
      assert_received {:compile_env, :elixir, [:unknown], :error}

      assert Application.put_env(:elixir, :unknown, nested: [key: :value]) == :ok

      assert compile_env(:elixir, :unknown, :default) == [nested: [key: :value]]
      assert_received {:compile_env, :elixir, [:unknown], {:ok, [nested: [key: :value]]}}

      assert compile_env(:elixir, :unknown) == [nested: [key: :value]]
      assert_received {:compile_env, :elixir, [:unknown], {:ok, [nested: [key: :value]]}}

      assert compile_env!(:elixir, :unknown) == [nested: [key: :value]]
      assert_received {:compile_env, :elixir, [:unknown], {:ok, [nested: [key: :value]]}}

      assert compile_env(:elixir, [:unknown, :nested]) == [key: :value]
      assert_received {:compile_env, :elixir, [:unknown, :nested], {:ok, [key: :value]}}

      assert compile_env!(:elixir, [:unknown, :nested]) == [key: :value]
      assert_received {:compile_env, :elixir, [:unknown, :nested], {:ok, [key: :value]}}

      assert compile_env(:elixir, [:unknown, :nested, :key]) == :value
      assert_received {:compile_env, :elixir, [:unknown, :nested, :key], {:ok, :value}}

      assert compile_env!(:elixir, [:unknown, :nested, :key]) == :value
      assert_received {:compile_env, :elixir, [:unknown, :nested, :key], {:ok, :value}}

      assert compile_env(:elixir, [:unknown, :unknown, :key], :default) == :default
      assert_received {:compile_env, :elixir, [:unknown, :unknown, :key], :error}

      assert compile_env(:elixir, [:unknown, :nested, :unkown], :default) == :default
      assert_received {:compile_env, :elixir, [:unknown, :nested, :unkown], :error}
    after
      Application.delete_env(:elixir, :unknown)
    end

    def trace({:compile_env, _, _, _} = msg, %Macro.Env{}) do
      send(self(), msg)
      :ok
    end

    def trace(_, _), do: :ok

    defp compile_env(app, key, default \\ nil) do
      code = quote do: Application.compile_env(unquote(app), unquote(key), unquote(default))
      {result, _binding} = Code.eval_quoted(code, [], tracers: [__MODULE__])
      result
    end

    defp compile_env!(app, key) do
      code = quote do: Application.compile_env!(unquote(app), unquote(key))
      {result, _binding} = Code.eval_quoted(code, [], tracers: [__MODULE__])
      result
    end
  end

  test "loaded and started applications" do
    started = Application.started_applications()
    assert is_list(started)
    assert {:elixir, 'elixir', _} = List.keyfind(started, :elixir, 0)

    started_timeout = Application.started_applications(7000)
    assert is_list(started_timeout)
    assert {:elixir, 'elixir', _} = List.keyfind(started_timeout, :elixir, 0)

    loaded = Application.loaded_applications()
    assert is_list(loaded)
    assert {:elixir, 'elixir', _} = List.keyfind(loaded, :elixir, 0)
  end

  test "application specification" do
    assert is_list(Application.spec(:elixir))
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
      path |> String.downcase() |> Path.expand()
    end
  else
    defp normalize_app_dir(path) do
      path |> String.downcase()
    end
  end
end
