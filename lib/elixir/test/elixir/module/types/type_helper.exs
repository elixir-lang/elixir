Code.require_file("../../test_helper.exs", __DIR__)

defmodule Point do
  defstruct [:x, :y, z: 0]
end

defmodule TypeHelper do
  alias Module.Types
  alias Module.Types.{Pattern, Expr, Descr}

  @doc """
  Main helper for inferring the given pattern + guards.
  """
  defmacro typeinfer!(patterns \\ [], guards \\ []) do
    quote do
      unquote(typeinfer(patterns, guards, __CALLER__))
      |> TypeHelper.__typecheck__!()
    end
  end

  @doc """
  Main helper for checking the given AST type checks without warnings.
  """
  defmacro typecheck!(patterns \\ [], guards \\ [], body) do
    quote do
      unquote(typecheck(patterns, guards, body, __CALLER__))
      |> TypeHelper.__typecheck__!()
    end
  end

  @doc """
  Main helper for checking the given AST type checks errors.
  """
  defmacro typeerror!(patterns \\ [], guards \\ [], body) do
    quote do
      unquote(typecheck(patterns, guards, body, __CALLER__))
      |> TypeHelper.__typeerror__!()
    end
  end

  @doc """
  Main helper for checking the given AST type warns.
  """
  defmacro typewarn!(patterns \\ [], guards \\ [], body) do
    quote do
      unquote(typecheck(patterns, guards, body, __CALLER__))
      |> TypeHelper.__typewarn__!()
    end
  end

  @doc """
  Main helper for checking the diagnostic of a given AST.
  """
  defmacro typediag!(patterns \\ [], guards \\ [], body) do
    quote do
      unquote(typecheck(patterns, guards, body, __CALLER__))
      |> TypeHelper.__typediag__!()
    end
  end

  @doc false
  def __typecheck__!({:ok, type, %{warnings: []}}), do: type

  def __typecheck__!({:ok, _type, %{warnings: warnings}}),
    do: raise("type checking ok but with warnings: #{inspect(warnings)}")

  def __typecheck__!({:error, %{warnings: warnings}}),
    do: raise("type checking errored with warnings: #{inspect(warnings)}")

  @doc false
  def __typeerror__!({:error, %{warnings: [{module, warning, _locs} | _]}}),
    do: module.format_diagnostic(warning).message

  def __typeerror__!({:ok, type, _context}),
    do: raise("type checking ok but expected error: #{Descr.to_quoted_string(type)}")

  @doc false
  def __typediag__!({:ok, type, %{warnings: [{module, warning, _locs}]}}),
    do: {type, module.format_diagnostic(warning)}

  def __typediag__!({:ok, type, %{warnings: []}}),
    do: raise("type checking ok without warnings: #{Descr.to_quoted_string(type)}")

  def __typediag__!({:ok, _type, %{warnings: warnings}}),
    do: raise("type checking ok but many warnings: #{inspect(warnings)}")

  def __typediag__!({:error, %{warnings: warnings}}),
    do: raise("type checking errored with warnings: #{inspect(warnings)}")

  @doc false
  def __typewarn__!(result) do
    {type, %{message: message}} = __typediag__!(result)
    {type, message}
  end

  @doc """
  Building block for typeinferring a given AST.
  """
  def typeinfer(patterns, guards, env) do
    fun =
      quote do
        fn unquote(patterns) when unquote(guards) -> :ok end
      end

    {ast, _, _} = :elixir_expand.expand(fun, :elixir_env.env_to_ex(env), env)
    {:fn, _, [{:->, _, [[{:when, _, [patterns, guards]}], _body]}]} = ast

    quote do
      TypeHelper.__typeinfer__(
        unquote(Macro.escape(patterns)),
        unquote(Macro.escape(guards))
      )
    end
  end

  def __typeinfer__(patterns, guards) do
    Pattern.of_head(patterns, guards, [], new_stack(), new_context())
  end

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

    with {:ok, _types, context} <- Pattern.of_head(patterns, guards, [], stack, new_context()),
         {:ok, type, context} <- Expr.of_expr(body, stack, context) do
      {:ok, type, context}
    end
  end

  defp new_stack() do
    Types.stack("types_test.ex", TypesTest, {:test, 0}, [], Module.ParallelChecker.test_cache())
  end

  defp new_context() do
    Types.context()
  end

  @doc """
  Interpolate the given hints.
  """
  def hints(hints) do
    hints
    |> List.wrap()
    |> Module.Types.Helpers.format_hints()
    |> IO.iodata_to_binary()
    |> String.trim()
  end

  @doc """
  A string-like sigil that replaces LINE references by actual line.
  """
  defmacro sigil_l({:<<>>, meta, parts}, []) do
    parts =
      for part <- parts do
        if is_binary(part) do
          part
          |> replace_line(__CALLER__.line)
          |> :elixir_interpolation.unescape_string()
        else
          part
        end
      end

    {:<<>>, meta, parts}
  end

  defp replace_line(string, line) do
    [head | rest] = String.split(string, "LINE")

    rest =
      for part <- rest do
        case part do
          <<?-, num, part::binary>> when num in ?0..?9 ->
            [Integer.to_string(line - num + ?0), part]

          part ->
            [Integer.to_string(line), part]
        end
      end

    IO.iodata_to_binary([head | rest])
  end
end
