Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.Tasks.Deps.AddTest do
  use ExUnit.Case

  alias Mix.Tasks.Deps.Add

  @mix_exs """
  defmodule DepsAddTest.MixProject do
    defp deps do
      [
        {:foo, "~> 0.8.1"}
      ]
    end
  end
  """

  @mix_exs_empty_deps """
  defmodule DepsAddTest.MixProject do
    defp deps do
      []
    end
  end
  """

  @mix_exs_default_deps """
  defmodule DepsAddTest.MixProject do
    defp deps do
      [
        # {:dep_from_hexpm, "~> 0.3.0"},
        # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
      ]
    end
  end
  """

  describe "adding deps with bad values" do
    test "dep already exists" do
      assert_raise Mix.Error, "foo already exists in mix.exs as {:foo, \"~> 0.8.1\"}", fn ->
        Add.add(["foo"], @mix_exs)
      end
    end

    test "non snake-case dep" do
      assert_raise Mix.Error, "Invalid package: \"camelCaseDep\"", fn ->
        Add.add(["camelCaseDep"], @mix_exs)
      end
    end

    test "invalid opt" do
      assert_raise Mix.Error, "Invalid options: [{\"--not-real-opt\", nil}]", fn ->
        Add.add(["bar", "--not-real-opt"], @mix_exs)
      end
    end

    test "both version and path" do
      assert_raise Mix.Error, "Cannot specify both version and path", fn ->
        Add.add(["bar", "--version", "1.0.0", "--path", "../bar"], @mix_exs)
      end
    end
  end

  describe "adding dep" do
    test "with existing" do
      assert Add.add(["asdf", "--version", "1.0.0"], @mix_exs) == """
             defmodule DepsAddTest.MixProject do
               defp deps do
                 [
                   {:asdf, "~> 1.0.0"},
                   {:foo, "~> 0.8.1"}
                 ]
               end
             end
             """

      assert_received {:mix_shell, :info, ["Added asdf to mix.exs: {:asdf, \"~> 1.0.0\"}"]}
    end

    test "without existing" do
      assert Add.add(["asdf", "--version", "1.0.0"], @mix_exs_empty_deps) == """
             defmodule DepsAddTest.MixProject do
               defp deps do
                 [
                   {:asdf, "~> 1.0.0"}
                 ]
               end
             end
             """

      assert_received {:mix_shell, :info, ["Added asdf to mix.exs: {:asdf, \"~> 1.0.0\"}"]}
    end

    test "with default deps" do
      assert Add.add(["asdf", "--version", "1.0.0"], @mix_exs_default_deps) == """
             defmodule DepsAddTest.MixProject do
               defp deps do
                 [
                   {:asdf, "~> 1.0.0"}
                   # {:dep_from_hexpm, "~> 0.3.0"},
                   # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
                 ]
               end
             end
             """

      assert_received {:mix_shell, :info, ["Added asdf to mix.exs: {:asdf, \"~> 1.0.0\"}"]}
    end

    test "with colon on deps arg" do
      assert Add.add([":asdf", "--version", "1.0.0"], @mix_exs) == """
             defmodule DepsAddTest.MixProject do
               defp deps do
                 [
                   {:asdf, "~> 1.0.0"},
                   {:foo, "~> 0.8.1"}
                 ]
               end
             end
             """

      assert_received {:mix_shell, :info, ["Added asdf to mix.exs: {:asdf, \"~> 1.0.0\"}"]}
    end

    test "with 0.0.0" do
      assert Add.add(["asdf", "--version", "0.0.0"], @mix_exs) == """
             defmodule DepsAddTest.MixProject do
               defp deps do
                 [
                   {:asdf, ">= 0.0.0"},
                   {:foo, "~> 0.8.1"}
                 ]
               end
             end
             """

      assert_received {:mix_shell, :info, ["Added asdf to mix.exs: {:asdf, \">= 0.0.0\"}"]}
    end

    test "with path" do
      assert Add.add([":asdf", "--path", "../asdf_local"], @mix_exs) == """
             defmodule DepsAddTest.MixProject do
               defp deps do
                 [
                   {:asdf, path: "../asdf_local"},
                   {:foo, "~> 0.8.1"}
                 ]
               end
             end
             """

      assert_received {:mix_shell, :info,
                       ["Added asdf to mix.exs: {:asdf, path: \"../asdf_local\"}"]}
    end

    test "no runtime" do
      assert Add.add(["asdf", "--version", "1.0.0", "--no-runtime"], @mix_exs) == """
             defmodule DepsAddTest.MixProject do
               defp deps do
                 [
                   {:asdf, "~> 1.0.0", runtime: false},
                   {:foo, "~> 0.8.1"}
                 ]
               end
             end
             """

      assert_received {:mix_shell, :info,
                       ["Added asdf to mix.exs: {:asdf, \"~> 1.0.0\", runtime: false}"]}
    end

    test "only test" do
      assert Add.add(
               ["asdf", "--version", "1.0.0", "--only", "test"],
               @mix_exs_empty_deps
             ) == """
             defmodule DepsAddTest.MixProject do
               defp deps do
                 [
                   {:asdf, "~> 1.0.0", only: :test}
                 ]
               end
             end
             """

      assert_received {:mix_shell, :info,
                       ["Added asdf to mix.exs: {:asdf, \"~> 1.0.0\", only: :test}"]}
    end

    test "only :test" do
      assert Add.add(
               ["asdf", "--version", "1.0.0", "--only", ":test"],
               @mix_exs_empty_deps
             ) == """
             defmodule DepsAddTest.MixProject do
               defp deps do
                 [
                   {:asdf, "~> 1.0.0", only: :test}
                 ]
               end
             end
             """

      assert_received {:mix_shell, :info,
                       ["Added asdf to mix.exs: {:asdf, \"~> 1.0.0\", only: :test}"]}
    end

    test "only test and dev" do
      assert Add.add(
               ["asdf", "--version", "1.0.0", "--only", "test", "--only", ":dev"],
               @mix_exs
             ) ==
               """
               defmodule DepsAddTest.MixProject do
                 defp deps do
                   [
                     {:asdf, "~> 1.0.0", only: [:test, :dev]},
                     {:foo, "~> 0.8.1"}
                   ]
                 end
               end
               """

      assert_received {:mix_shell, :info,
                       ["Added asdf to mix.exs: {:asdf, \"~> 1.0.0\", only: [:test, :dev]}"]}
    end
  end
end
