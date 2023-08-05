Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.HasUnquoteTest do
  use ExUnit.Case, async: true

  test "expression without unquote returns false" do
    ast =
      quote unquote: false do
        opts = [x: 5]
        x = Keyword.fetch!(opts, :x)
        x + 1
      end

    refute :elixir_quote.has_unquotes(ast)
  end

  test "expression with unquote returns true" do
    ast =
      quote unquote: false do
        [x: unquote(x)]
      end

    assert :elixir_quote.has_unquotes(ast)

    ast =
      quote unquote: false do
        unquote(module).fun(x)
      end

    assert :elixir_quote.has_unquotes(ast)

    ast =
      quote unquote: false do
        module.unquote(fun)(x)
      end

    assert :elixir_quote.has_unquotes(ast)

    ast =
      quote unquote: false do
        module.fun(unquote(x))
      end

    assert :elixir_quote.has_unquotes(ast)

    ast =
      quote unquote: false do
        module.fun(unquote_splicing(args))
      end

    assert :elixir_quote.has_unquotes(ast)
  end

  test "expression with unquote within quote returns false" do
    ast =
      quote unquote: false do
        quote do
          x + unquote(y)
        end
      end

    refute :elixir_quote.has_unquotes(ast)

    ast =
      quote unquote: false do
        quote do
          foo = bar(unquote_splicing(args))
        end
      end

    refute :elixir_quote.has_unquotes(ast)
  end

  test "expression with unquote within unquote within quote returns true" do
    ast =
      quote unquote: false do
        quote do
          x + unquote(unquote(y))
        end
      end

    assert :elixir_quote.has_unquotes(ast)

    ast =
      quote unquote: false do
        quote do
          foo = bar(unquote_splicing(unquote(args)))
        end
      end

    assert :elixir_quote.has_unquotes(ast)

    ast =
      quote unquote: false do
        quote do
          foo = bar(unquote(unquote_splicing(args)))
        end
      end

    assert :elixir_quote.has_unquotes(ast)
  end

  test "expression within quote disabling unquotes always returns false" do
    ast =
      quote unquote: false do
        quote unquote: false do
          x + unquote(unquote(y))
        end
      end

    refute :elixir_quote.has_unquotes(ast)

    ast =
      quote unquote: false do
        quote bind_quoted: [x: x] do
          x + unquote(unquote(y))
        end
      end

    refute :elixir_quote.has_unquotes(ast)
  end
end
