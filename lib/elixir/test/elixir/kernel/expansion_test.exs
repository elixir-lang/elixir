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
    assert env.aliases == [{:"Elixir.True", true}, {:"Elixir.World", :hello}]
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

  ## =

  test "=: sets context to match" do
    # TODO
  end

  test "=: defines vars" do
    { output, env } = expand_env(quote(do: a = 1), __ENV__)
    assert output == quote(do: a = 1)
    assert { :a, __MODULE__ } in env.vars
  end

  ## Pseudo vars

  test "__MODULE__" do
    assert expand(quote do: __MODULE__) == __MODULE__
  end

  test "__FILE__" do
    assert expand(quote do: __FILE__) == __FILE__
  end

  test "__DIR__" do
    assert expand(quote do: __DIR__) == __DIR__
  end

  test "__CALLER__" do
    assert expand(quote do: __CALLER__) == quote do: __CALLER__
  end

  test "__ENV__" do
    env = __ENV__
    assert expand_env(quote(do: __ENV__), env) == { env, env }
  end

  test "__ENV__.accessor" do
    assert expand(quote(do: __ENV__.file)) == __FILE__
  end

  ## Vars

  test "vars: expand to local call" do
    { output, env } = expand_env(quote(do: a), __ENV__)
    assert output == quote(do: a())
    assert env.vars == []
  end

  test "vars: considers hygiene" do
    # TODO
  end

  test "vars: forces variable on exist" do
    # TODO
  end

  test "vars: considers var scope" do
    # TODO
  end

  ## Locals

  test "locals: expands to remote calls" do
    assert expand(quote do: a =~ b) == quote do: Kernel.=~(a(), b())
  end

  test "locals: raises on match" do
    assert_raise CompilationError, fn ->
      expand(quote do: (a =~ b) = c)
    end
  end

  test "locals: raises on guards" do
    # TODO
  end

  test "locals: expands to configured local" do
    assert expand_env(quote(do: a), __ENV__.local(Hello)) |> elem(0) ==
           quote do: Hello.a())
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
