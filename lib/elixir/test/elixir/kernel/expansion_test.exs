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

  test "__block__: accumulates vars" do
    assert expand(quote(do: (a = 1; a))) == quote do: (a = 1; a)
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
    assert expand(quote do: __ENV__.context = :match) == quote do: :match = :match
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

  ## Super

  test "super: expand args" do
    assert expand(quote do: super(a, b)) == quote do: super(a(), b())
  end

  ## Vars

  test "vars: expand to local call" do
    { output, env } = expand_env(quote(do: a), __ENV__)
    assert output == quote(do: a())
    assert env.vars == []
  end

  test "vars: forces variable to exist" do
    assert expand(quote do: (var!(a) = 1; var!(a)))

    message = %r"expected var a to expand to an existing variable or be a part of a match"
    assert_raise CompileError, message, fn -> expand(quote do: var!(a)) end

    message = %r"expected var a \(context Unknown\) to expand to an existing variable or be a part of a match"
    assert_raise CompileError, message, fn -> expand(quote do: var!(a, Unknown)) end
  end

  test "^: expands args" do
    assert expand(quote do: ^a = 1) == quote do: ^a = 1
  end

  test "^: raises outside match" do
    assert_raise CompileError, %r"cannot use \^a outside of match clauses", fn ->
      expand(quote do: ^a)
    end
  end

  test "^: raises without var" do
    assert_raise CompileError, %r"invalid args for unary operator \^, expected an existing variable, got \^1", fn ->
      expand(quote do: ^1 = 1)
    end
  end

  ## Locals

  test "locals: expands to remote calls" do
    assert { {:., _, [Kernel, :=~] }, _, [{:a, _, []}, {:b, _, []}] } =
          expand(quote do: a =~ b)
  end

  test "locals: raises on guards" do
    # TODO
  end

  test "locals: expands to configured local" do
    assert expand_env(quote(do: a), __ENV__.local(Hello)) |> elem(0) ==
           quote(do: :"Elixir.Hello".a())
  end

  ## Tuples

  test "tuples: expanded as arguments" do
    assert expand(quote(do: { a = 1, a })) == quote do: { a = 1, a() }
    assert expand(quote(do: { b, a = 1, a })) == quote do: { b(), a = 1, a() }
  end

  ## quote

  test "quote: expanded to raw forms" do
    assert expand(quote do: (quote do: hello)) == { :{}, [], [:hello, [], __MODULE__] }
  end

  ## Anonymous calls

  test "anonymous calls: expands base and args" do
    assert expand(quote do: a.(b)) == quote do: a().(b())
  end

  test "anonymous calls: raises on atom base" do
    assert_raise CompileError, %r"invalid function call :foo.()", fn ->
      expand(quote do: :foo.(a))
    end
  end

  ## Remote calls

  test "remote calls: expands to erlang" do
    assert expand(quote do: Kernel.is_atom(a)) == quote do: :erlang.is_atom(a())
  end

  test "remote calls: expands macros" do
    assert expand(quote do: Kernel.ExpansionTest.thirteen) == 13
  end

  test "remote calls: expands receiver and args" do
    assert expand(quote do: a.is_atom(b)) == quote do: a().is_atom(b())
  end

  ## Invalid

  test "handles invalid expressions" do
    assert_raise CompileError, %r"invalid quoted expression: {1, 2, 3}", fn ->
      expand(quote do: unquote({ 1, 2, 3 }))
    end
  end

  ## Helpers

  defmacro thirteen do
    13
  end

  defp expand(expr) do
    expand_env(expr, __ENV__) |> elem(0)
  end

  defp expand_env(expr, env) do
    { expr, env } = :elixir_exp.expand(expr, :elixir_env.ex_to_env(env))
    { expr, set_elem(env, 0, Macro.Env) }
  end
end
