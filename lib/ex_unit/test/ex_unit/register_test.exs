Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.RegisterTest do
  use ExUnit.Case

  import ExUnit.CaptureIO

  test "singular test types" do
    on_exit(fn ->
      ExUnit.configure(plural_rules: %{})
    end)

    ExUnit.plural_rule("property", "properties")

    defmodule SingularTestTypeCase do
      use ExUnit.Case

      :"property is true" =
        ExUnit.Case.register_test(
          __ENV__.module,
          __ENV__.file,
          __ENV__.line,
          :property,
          "is true",
          []
        )

      def unquote(:"property is true")(_) do
        assert succeed()
      end

      test "test true" do
        assert succeed()
      end

      defp succeed, do: true
    end

    assert capture_io(fn ->
             assert ExUnit.run() == %{failures: 0, skipped: 0, total: 2, excluded: 0}
           end) =~ "1 property, 1 test, 0 failures"
  end

  test "plural test types" do
    on_exit(fn ->
      ExUnit.configure(plural_rules: %{})
    end)

    ExUnit.plural_rule("property", "properties")

    defmodule PluralTestTypeCase do
      use ExUnit.Case

      :"property is true" =
        ExUnit.Case.register_test(
          __ENV__.module,
          __ENV__.file,
          __ENV__.line,
          :property,
          "is true",
          []
        )

      def unquote(:"property is true")(_) do
        assert succeed()
      end

      :"property is also true" =
        ExUnit.Case.register_test(
          __ENV__.module,
          __ENV__.file,
          __ENV__.line,
          :property,
          "is also true",
          []
        )

      def unquote(:"property is also true")(_) do
        assert succeed()
      end

      test "test true" do
        assert succeed()
      end

      test "test true also" do
        assert succeed()
      end

      defp succeed, do: true
    end

    assert capture_io(fn ->
             assert ExUnit.run() == %{failures: 0, skipped: 0, total: 4, excluded: 0}
           end) =~ "2 properties, 2 tests, 0 failures"
  end
end
