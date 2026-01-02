# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

Code.require_file("../../test_helper.exs", __DIR__)

defmodule Point do
  defstruct [:x, :y, z: 0]
end

defmodule TypeHelper do
  alias Module.Types
  alias Module.Types.{Pattern, Expr, Descr}

  @doc """
  Main helper for checking the given AST type checks without warnings.
  """
  defmacro typedyn!(patterns \\ [], guards \\ true, body) do
    quote do
      unquote(typecheck(:dynamic, patterns, guards, body, __CALLER__))
      |> TypeHelper.__typecheck__!()
    end
  end

  @doc """
  Main helper for checking the given AST type checks without warnings.
  """
  defmacro typecheck!(patterns \\ [], guards \\ true, body) do
    quote do
      unquote(typecheck(:static, patterns, guards, body, __CALLER__))
      |> TypeHelper.__typecheck__!()
    end
  end

  @doc """
  Main helper for checking the given AST type checks errors.
  """
  defmacro typeerror!(patterns \\ [], guards \\ true, body) do
    [patterns, guards, body] = prune_columns([patterns, guards, body])

    quote do
      unquote(typecheck(:static, patterns, guards, body, __CALLER__))
      |> TypeHelper.__typeerror__!()
    end
  end

  @doc """
  Main helper for checking the given AST type warns.
  """
  defmacro typewarn!(patterns \\ [], guards \\ true, body) do
    [patterns, guards, body] = prune_columns([patterns, guards, body])

    quote do
      unquote(typecheck(:static, patterns, guards, body, __CALLER__))
      |> TypeHelper.__typewarn__!()
    end
  end

  @doc """
  Main helper for checking the diagnostic of a given AST.
  """
  defmacro typediag!(patterns \\ [], guards \\ true, body) do
    quote do
      unquote(typecheck(:static, patterns, guards, body, __CALLER__))
      |> TypeHelper.__typediag__!()
    end
  end

  @doc false
  def __typecheck__!({type, %{warnings: []}}), do: type

  def __typecheck__!({_type, %{warnings: warnings, failed: false}}),
    do: raise("type checking ok but with warnings: #{inspect(warnings)}")

  def __typecheck__!({_type, %{warnings: warnings, failed: true}}),
    do: raise("type checking errored with warnings: #{inspect(warnings)}")

  @doc false
  def __typeerror__!({_type, %{warnings: [{module, warning, _locs} | _], failed: true}}),
    do: module.format_diagnostic(warning).message

  def __typeerror__!({_type, %{warnings: warnings, failed: false}}),
    do: raise("type checking with warnings but expected error: #{inspect(warnings)}")

  def __typeerror__!({type, _}),
    do: raise("type checking ok but expected error: #{Descr.to_quoted_string(type)}")

  @doc false
  def __typediag__!({type, %{warnings: [_ | _] = warnings}}),
    do: {type, for({module, arg, _} <- warnings, do: module.format_diagnostic(arg))}

  def __typediag__!({type, %{warnings: []}}),
    do: raise("type checking without diagnostics: #{Descr.to_quoted_string(type)}")

  @doc false
  def __typewarn__!({type, %{warnings: [{module, warning, _locs}], failed: false}}),
    do: {type, module.format_diagnostic(warning).message}

  def __typewarn__!({type, %{warnings: []}}),
    do: raise("type checking ok without warnings: #{Descr.to_quoted_string(type)}")

  def __typewarn__!({_type, %{warnings: warnings, failed: false}}),
    do: raise("type checking ok but many warnings: #{inspect(warnings)}")

  def __typewarn__!({_type, %{warnings: warnings, failed: true}}),
    do: raise("type checking errored with warnings: #{inspect(warnings)}")

  defp typecheck(mode, patterns, guards, body, env) do
    {patterns, guards, body} = expand_and_unpack(patterns, guards, body, env)

    quote do
      TypeHelper.__typecheck__(
        unquote(mode),
        unquote(Macro.escape(patterns)),
        unquote(Macro.escape(guards)),
        unquote(Macro.escape(body))
      )
    end
  end

  def __typecheck__(mode, patterns, guards, body) do
    stack = new_stack(mode)
    expected = Enum.map(patterns, fn _ -> Descr.dynamic() end)

    {_trees, context} =
      Pattern.of_head(patterns, guards, expected, :default, [], stack, new_context())

    Expr.of_expr(body, Descr.term(), :ok, stack, context)
  end

  defp expand_and_unpack(patterns, guards, body, env) do
    fun =
      quote do
        fn unquote_splicing(patterns) when unquote(guards) -> unquote(body) end
      end

    {ast, _, _} = :elixir_expand.expand(fun, :elixir_env.env_to_ex(env), env)
    {:fn, _, [{:->, _, [[{:when, _, args}], body]}]} = ast
    {patterns, guards} = Enum.split(args, -1)
    {patterns, guards, body}
  end

  defp new_stack(mode) do
    cache =
      if mode == :infer do
        :none
      else
        {:ok, cache} = Module.ParallelChecker.start_link()
        cache
      end

    handler = fn _, fun_arity, _, _ -> raise "no local lookup for: #{inspect(fun_arity)}" end
    Types.stack(mode, "types_test.ex", TypesTest, {:test, 0}, [], cache, handler)
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

  @strip_ansi [IO.ANSI.green(), IO.ANSI.red(), IO.ANSI.reset()]

  @doc """
  Strip ansi escapes from message.
  """
  def strip_ansi(doc) do
    String.replace(doc, @strip_ansi, "")
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

  defp prune_columns(ast) do
    Macro.prewalk(ast, fn node ->
      Macro.update_meta(node, &Keyword.delete(&1, :column))
    end)
  end
end
