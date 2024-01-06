Code.require_file("../../test_helper.exs", __DIR__)

defmodule TypeHelper do
  alias Module.Types
  alias Module.Types.{Pattern, Expr}

  @doc """
  Main helper for checking the given AST type checks without warnings.
  """
  defmacro typecheck!(patterns \\ [], guards \\ [], body) do
    quote do
      unquote(typecheck(patterns, guards, body, __CALLER__))
      |> TypeHelper.__typecheck__!()
    end
  end

  def __typecheck__!({:ok, type, %{warnings: []}}), do: type

  def __typecheck__!({:ok, _type, %{warnings: warnings}}),
    do: raise("type checking ok but with warnings: #{inspect(warnings)}")

  def __typecheck__!({:error, %{warnings: warnings}}),
    do: raise("type checking errored with warnings: #{inspect(warnings)}")

  @doc """
  Building block for typechecking a given AST.
  """
  def typecheck(patterns, guards, body, env) do
    fun =
      quote do
        fn unquote(patterns) when unquote(guards) -> unquote(body) end
      end

    {ast, _, _} = :elixir_expand.expand(fun, :elixir_env.env_to_ex(env), env)
    {:fn, _, [{:->, _, [[{:when, _, [patterns, guards]}], body]}]} = ast

    quote do
      TypeHelper.__typecheck__(
        unquote(Macro.escape(patterns)),
        unquote(Macro.escape(guards)),
        unquote(Macro.escape(body))
      )
    end
  end

  def __typecheck__(patterns, guards, body) do
    stack = new_stack()

    with {:ok, _types, context} <- Pattern.of_head(patterns, guards, stack, new_context()),
         {:ok, type, context} <- Expr.of_expr(body, stack, context) do
      {:ok, type, context}
    end
  end

  def new_stack() do
    Types.stack("types_test.ex", TypesTest, {:test, 0}, [], Module.ParallelChecker.test_cache())
  end

  def new_context() do
    Types.context()
  end
end
