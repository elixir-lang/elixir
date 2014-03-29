Code.require_file "../test_helper.exs", __DIR__

defmodule Kernel.ExpansionTarget do
  defmacro seventeen, do: 17
end

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

  test "__aliases__: expands to elixir_aliases on runtime" do
    assert expand(quote do: hello.World) ==
           quote do: :elixir_aliases.concat([hello(), :World])
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

  test "=: does not carry rhs imports" do
    assert expand(quote(do: flatten([1,2,3]) = import List)) ==
           quote(do: flatten([1,2,3]) = import :"Elixir.List", [])
  end

  test "=: does not define _" do
    { output, env } = expand_env(quote(do: _ = 1), __ENV__)
    assert output == quote(do: _ = 1)
    assert env.vars == []
  end

  ## Pseudo vars

  test "__MODULE__" do
    assert expand(quote do: __MODULE__) == __MODULE__
  end

  test "__DIR__" do
    assert expand(quote do: __DIR__) == __DIR__
  end

  test "__CALLER__" do
    assert expand(quote do: __CALLER__) == quote do: __CALLER__
  end

  test "__ENV__" do
    env = __ENV__
    assert expand_env(quote(do: __ENV__), env) ==
           { { :{}, [], tuple_to_list(env.line(0)) }, env }
  end

  test "__ENV__.accessor" do
    env = __ENV__
    assert expand_env(quote(do: __ENV__.file), env) == { __ENV__.file, env }
    assert expand_env(quote(do: __ENV__.unknown), env) ==
           { quote(do: unquote({ :{}, [], tuple_to_list(env.line(0)) }).unknown), env }
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

    message = ~r"expected var a to expand to an existing variable or be a part of a match"
    assert_raise CompileError, message, fn -> expand(quote do: var!(a)) end

    message = ~r"expected var a \(context Unknown\) to expand to an existing variable or be a part of a match"
    assert_raise CompileError, message, fn -> expand(quote do: var!(a, Unknown)) end
  end

  test "^: expands args" do
    assert expand(quote do: ^a = 1) == quote do: ^a = 1
  end

  test "^: raises outside match" do
    assert_raise CompileError, ~r"cannot use \^a outside of match clauses", fn ->
      expand(quote do: ^a)
    end
  end

  test "^: raises without var" do
    assert_raise CompileError, ~r"invalid argument for unary operator \^, expected an existing variable, got: \^1", fn ->
      expand(quote do: ^1 = 1)
    end
  end

  ## Locals

  test "locals: expands to remote calls" do
    assert { {:., _, [Kernel, :=~] }, _, [{:a, _, []}, {:b, _, []}] } =
          expand(quote do: a =~ b)
  end

  test "locals: expands to configured local" do
    assert expand_env(quote(do: a), __ENV__.local(Hello)) |> elem(0) ==
           quote(do: :"Elixir.Hello".a())
  end

  test "locals: in guards" do
    assert expand(quote(do: fn pid when :erlang.==(pid, self) -> pid end)) ==
           quote(do: fn pid when :erlang.==(pid, :erlang.self()) -> pid end)
  end

  test "locals: custom imports" do
    assert expand(quote do: (import Kernel.ExpansionTarget; seventeen)) ==
           quote do: (import :"Elixir.Kernel.ExpansionTarget", []; 17)
  end

  ## Tuples

  test "tuples: expanded as arguments" do
    assert expand(quote(do: { a = 1, a })) == quote do: { a = 1, a() }
    assert expand(quote(do: { b, a = 1, a })) == quote do: { b(), a = 1, a() }
  end

  ## Maps & structs

  test "maps: expanded as arguments" do
    assert expand(quote(do: %{ a: a = 1, b: a })) == quote do: %{ a: a = 1, b: a() }
  end

  test "structs: expanded as arguments" do
    assert expand(quote(do: %:elixir{ a: a = 1, b: a })) ==
           quote do: %:elixir{ a: a = 1, b: a() }

    assert expand(quote(do: %:"Elixir.Kernel"{ a: a = 1, b: a })) ==
           quote do: %:"Elixir.Kernel"{ a: a = 1, b: a() }
  end

  test "structs: expects atoms" do
    assert_raise CompileError, ~r"expected struct name to be a compile time atom or alias", fn ->
      expand(quote do: %unknown{ a: 1 })
    end
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
    assert_raise CompileError, ~r"invalid function call :foo.()", fn ->
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
    assert expand(quote do: (a = :foo).is_atom(a)) == quote do: (a = :foo).is_atom(a())
  end

  test "remote calls: modules must be required for macros" do
    assert expand(quote do: (require Kernel.ExpansionTarget; Kernel.ExpansionTarget.seventeen)) ==
           quote do: (require :"Elixir.Kernel.ExpansionTarget", []; 17)
  end

  test "remote calls: raises when not required" do
    msg = ~r"you must require Kernel\.ExpansionTarget before invoking the macro Kernel\.ExpansionTarget\.seventeen/0"
    assert_raise CompileError, msg, fn ->
      expand(quote do: Kernel.ExpansionTarget.seventeen)
    end
  end

  ## Comprehensions

  test "variables inside comprehensions do not leak with enums" do
    assert expand(quote do: (for(a <- b, do: c = 1); c)) ==
           quote do: (for(a <- b(), do: c = 1); c())
  end

  test "variables inside comprehensions do not leak with binaries" do
    assert expand(quote do: (for(<<a <- b>>, do: c = 1); c)) ==
           quote do: (for(<< <<a>> <- b() >>, do: c = 1); c())
  end

  test "variables inside filters are available in blocks" do
    assert expand(quote do: for(a <- b, c = a, do: c)) ==
           quote do: (for(a <- b(), c = a, do: c))
  end

  # test "variables inside comprehensions options do not leak" do
  #   assert expand(quote do: (for(a <- b, into: c = [], do: 1); c)) ==
  #          quote do: (for(a <- b(), do: 1, into: c = []); c())
  # end

  ## Capture

  test "&: keeps locals" do
    assert expand(quote do: &unknown/2) ==
           {:&, [], [{:/, [], [{:unknown,[],nil}, 2]}]}
    assert expand(quote do: &unknown(&1, &2)) ==
           {:&, [], [{:/, [], [{:unknown,[],nil}, 2]}]}
  end

  test "&: expands remotes" do
    assert expand(quote do: &List.flatten/2) ==
           quote do: :erlang.make_fun(:"Elixir.List", :flatten, 2)

    assert expand(quote do: &Kernel.is_atom/1) ==
           quote do: :erlang.make_fun(:erlang, :is_atom, 1)
  end

  test "&: expands macros" do

    assert expand(quote do: (require Kernel.ExpansionTarget; &Kernel.ExpansionTarget.seventeen/0)) ==
           quote do: (require :"Elixir.Kernel.ExpansionTarget", []; fn -> 17 end)
  end

  ## fn

  test "fn: expands each clause" do
    assert expand(quote do: fn x -> x; _ -> x end) ==
           quote do: fn x -> x; _ -> x() end
  end

  test "fn: does not share lexical in between clauses" do
    assert expand(quote do: fn 1 -> import List; 2 -> flatten([1,2,3]) end) ==
           quote do: fn 1 -> import :"Elixir.List", []; 2 -> flatten([1,2,3]) end
  end

  test "fn: expands guards" do
    assert expand(quote do: fn x when x when __ENV__.context -> true end) ==
           quote do: fn x when x when :guard -> true end
  end

  test "fn: does not leak vars" do
    assert expand(quote do: (fn x -> x end; x)) ==
           quote do: (fn x -> x end; x())
  end

  ## Case

  test "case: expands each clause" do
    assert expand_and_clean(quote do: (case w do x -> x; _ -> x end)) ==
           quote do: (case w() do x -> x; _ -> x() end)
  end

  test "case: does not share lexical in between clauses" do
    assert expand_and_clean(quote do: (case w do 1 -> import List; 2 -> flatten([1,2,3]) end)) ==
           quote do: (case w() do 1 -> import :"Elixir.List", []; 2 -> flatten([1,2,3]) end)
  end

  test "case: expands guards" do
    assert expand_and_clean(quote do: (case w do x when x when __ENV__.context -> true end)) ==
           quote do: (case w() do x when x when :guard -> true end)
  end

  test "case: does not leaks vars on head" do
    assert expand_and_clean(quote do: (case w do x -> x; y -> y end; :erlang.+(x, y))) ==
           quote do: (case w() do x -> x; y -> y end; :erlang.+(x(), y()))
  end

  test "case: leaks vars" do
    assert expand_and_clean(quote do: (case w do x -> x = x; y -> y = y end; :erlang.+(x, y))) ==
           quote do: (case w() do x -> x = x; y -> y = y end; :erlang.+(x, y))
  end

  ## Receive

  test "receive: expands each clause" do
    assert expand_and_clean(quote do: (receive do x -> x; _ -> x end)) ==
           quote do: (receive do x -> x; _ -> x() end)
  end

  test "receive: does not share lexical in between clauses" do
    assert expand_and_clean(quote do: (receive do 1 -> import List; 2 -> flatten([1,2,3]) end)) ==
           quote do: (receive do 1 -> import :"Elixir.List", []; 2 -> flatten([1,2,3]) end)
  end

  test "receive: expands guards" do
    assert expand_and_clean(quote do: (receive do x when x when __ENV__.context -> true end)) ==
           quote do: (receive do x when x when :guard -> true end)
  end

  test "receive: does not leaks clause vars" do
    assert expand_and_clean(quote do: (receive do x -> x; y -> y end; :erlang.+(x, y))) ==
           quote do: (receive do x -> x; y -> y end; :erlang.+(x(), y()))
  end

  test "receive: leaks vars" do
    assert expand_and_clean(quote do: (receive do x -> x = x; y -> y = y end; :erlang.+(x, y))) ==
           quote do: (receive do x -> x = x; y -> y = y end; :erlang.+(x, y))
  end

  test "receive: leaks vars on after" do
    assert expand_and_clean(quote do: (receive do x -> x = x after y -> y; w = y end; :erlang.+(x, w))) ==
           quote do: (receive do x -> x = x after y() -> y(); w = y() end; :erlang.+(x, w))
  end

  ## Try

  test "try: expands do" do
    assert expand(quote do: (try do x = y end; x)) ==
           quote do: (try do x = y() end; x())
  end

  test "try: expands catch" do
    assert expand(quote do: (try do x catch x, y -> z = :erlang.+(x, y) end; z)) ==
           quote do: (try do x() catch x, y -> z = :erlang.+(x, y) end; z())
  end

  test "try: expands after" do
    assert expand(quote do: (try do x after z = y end; z)) ==
           quote do: (try do x() after z = y() end; z())
  end

  test "try: expands else" do
    assert expand(quote do: (try do x else z -> z end; z)) ==
           quote do: (try do x() else z -> z end; z())
  end

  test "try: expands rescue" do
    assert expand(quote do: (try do x rescue x -> x; Error -> x end; x)) ==
           quote do: (try do x() rescue unquote(:in)(x, _) -> x; unquote(:in)(_, [:"Elixir.Error"]) -> x() end; x())
  end

  ## Binaries

  test "bitstrings: expands modifiers" do
    assert expand(quote do: (import Kernel.ExpansionTarget; << x :: seventeen >>)) ==
           quote do: (import :"Elixir.Kernel.ExpansionTarget", []; << x() :: [unquote(:size)(17)] >>)

    assert expand(quote do: (import Kernel.ExpansionTarget; << seventeen :: seventeen, x :: size(seventeen) >> = 1)) ==
           quote do: (import :"Elixir.Kernel.ExpansionTarget", [];
                      << seventeen :: [unquote(:size)(17)], x :: [unquote(:size)(seventeen)] >> = 1)
  end

  test "bitstrings: expands modifiers args" do
    assert expand(quote do: (require Kernel.ExpansionTarget; << x :: size(Kernel.ExpansionTarget.seventeen) >>)) ==
           quote do: (require :"Elixir.Kernel.ExpansionTarget", []; << x() :: [unquote(:size)(17)] >>)
  end

  ## Invalid

  test "handles invalid expressions" do
    assert_raise CompileError, ~r"invalid quoted expression: {1, 2, 3}", fn ->
      expand(quote do: unquote({ 1, 2, 3 }))
    end

    assert_raise CompileError, ~r"invalid quoted expression: #Function<", fn ->
      expand(quote do: unquote({ :sample, fn -> end }))
    end
  end

  ## Helpers

  defmacro thirteen do
    13
  end

  defp expand_and_clean(expr) do
    expand_env(expr, __ENV__) |> elem(0) |> Macro.update_meta(&Keyword.drop(&1, [:export]))
  end

  defp expand(expr) do
    expand_env(expr, __ENV__) |> elem(0)
  end

  defp expand_env(expr, env) do
    { expr, env } = :elixir_exp.expand(expr, :elixir_env.ex_to_env(env))
    { expr, set_elem(env, 0, Macro.Env) }
  end
end
