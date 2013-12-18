Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.ExpansionTest do
  use ExUnit.Case, async: true

  ## __block__

  test "__block__: expands to nil when empty" do
    assert expand(quote do: __block__()) == nil
  end

  test "__block__: expands to argument when arity is 1" do
    assert expand(quote do: __block__(1)) == 1
  end

  test "__block__: is recursive to argument when arity is 1" do
    assert expand(quote do: __block__(1, __block__(2))) == quote do: __block__(1, 2)
  end

  ## alias

  test "alias: expand args, defines alias and returns itself" do
    alias true, as: True

    input = quote do: (alias :hello, as: World, warn: True)
    { output, env } = expand_env(input, __ENV__)

    assert output == quote do: (alias :hello, as: :"Elixir.World", warn: true)
    assert env.aliases == [{:"Elixir.True", true},{:"Elixir.World", :hello}]
  end

  ## __aliases__

  test "__aliases__: expands even if no alias" do
    assert expand(quote do: World) == :"Elixir.World"
    assert expand(quote do: Elixir.World) == :"Elixir.World"
  end

  test "__aliases__: expands with alias" do
    alias Hello, as: World
    assert expand_env(quote(do: World), __ENV__) |> elem(0) == :"Elixir.Hello"
  end

  test "__aliases__: expands with alias is recursive" do
    alias Source, as: Hello
    alias Hello, as: World
    assert expand_env(quote(do: World), __ENV__) |> elem(0) == :"Elixir.Source"
  end

  ## Helpers

  defp expand(expr) do
    expand_env(expr, __ENV__) |> elem(0)
  end

  defp expand_env(expr, env) do
    { expr, env } = :elixir_exp.expand(expr, :elixir_env.ex_to_env(env))
    { expr, set_elem(env, 0, Macro.Env) }
  end
end
