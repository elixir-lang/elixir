Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.StringTokenizerTest do
  use ExUnit.Case, async: true

  defp var({var, _, nil}), do: var
  defp aliases({:__aliases__, _, [alias]}), do: alias

  test "tokenizes vars" do
    assert Code.string_to_quoted!("_12") |> var() == :_12
    assert Code.string_to_quoted!("ola") |> var() == :ola
    assert Code.string_to_quoted!("ólá") |> var() == :ólá
    assert Code.string_to_quoted!("óLÁ") |> var() == :óLÁ
    assert Code.string_to_quoted!("ólá?") |> var() == :ólá?
    assert Code.string_to_quoted!("ólá!") |> var() == :ólá!
    assert Code.string_to_quoted!("こんにちは世界") |> var() == :こんにちは世界
    assert {:error, _} = Code.string_to_quoted("v@r")
    assert {:error, _} = Code.string_to_quoted("1var")
  end

  test "tokenizes atoms" do
    assert Code.string_to_quoted!(":_12") == :_12
    assert Code.string_to_quoted!(":ola") == :ola
    assert Code.string_to_quoted!(":ólá") == :ólá
    assert Code.string_to_quoted!(":ólá?") == :ólá?
    assert Code.string_to_quoted!(":ólá!") == :ólá!
    assert Code.string_to_quoted!(":ól@") == :ól@
    assert Code.string_to_quoted!(":ól@!") == :ól@!
    assert Code.string_to_quoted!(":ó@@!") == :ó@@!
    assert Code.string_to_quoted!(":Ola") == :Ola
    assert Code.string_to_quoted!(":Ólá") == :Ólá
    assert Code.string_to_quoted!(":ÓLÁ") == :ÓLÁ
    assert Code.string_to_quoted!(":ÓLÁ?") == :ÓLÁ?
    assert Code.string_to_quoted!(":ÓLÁ!") == :ÓLÁ!
    assert Code.string_to_quoted!(":ÓL@!") == :ÓL@!
    assert Code.string_to_quoted!(":Ó@@!") == :Ó@@!
    assert Code.string_to_quoted!(":こんにちは世界") == :こんにちは世界
    assert {:error, _} = Code.string_to_quoted(":123")
    assert {:error, _} = Code.string_to_quoted(":@123")
  end

  test "tokenizes keywords" do
    assert Code.string_to_quoted!("[_12: 0]") == [_12: 0]
    assert Code.string_to_quoted!("[ola: 0]") == [ola: 0]
    assert Code.string_to_quoted!("[ólá: 0]") == [ólá: 0]
    assert Code.string_to_quoted!("[ólá?: 0]") == [ólá?: 0]
    assert Code.string_to_quoted!("[ólá!: 0]") == [ólá!: 0]
    assert Code.string_to_quoted!("[ól@: 0]") == [ól@: 0]
    assert Code.string_to_quoted!("[ól@!: 0]") == [ól@!: 0]
    assert Code.string_to_quoted!("[ó@@!: 0]") == [ó@@!: 0]
    assert Code.string_to_quoted!("[Ola: 0]") == [Ola: 0]
    assert Code.string_to_quoted!("[Ólá: 0]") == [Ólá: 0]
    assert Code.string_to_quoted!("[ÓLÁ: 0]") == [ÓLÁ: 0]
    assert Code.string_to_quoted!("[ÓLÁ?: 0]") == [ÓLÁ?: 0]
    assert Code.string_to_quoted!("[ÓLÁ!: 0]") == [ÓLÁ!: 0]
    assert Code.string_to_quoted!("[ÓL@!: 0]") == [ÓL@!: 0]
    assert Code.string_to_quoted!("[Ó@@!: 0]") == [Ó@@!: 0]
    assert Code.string_to_quoted!("[こんにちは世界: 0]") == [こんにちは世界: 0]
    assert {:error, _} = Code.string_to_quoted("[123: 0]")
    assert {:error, _} = Code.string_to_quoted("[@123: 0]")
  end

  test "tokenizes aliases" do
    assert Code.string_to_quoted!("Ola") |> aliases() == String.to_atom("Ola")
    assert Code.string_to_quoted!("M_123") |> aliases() == String.to_atom("M_123")
    assert {:error, _} = Code.string_to_quoted("Óla")
    assert {:error, _} = Code.string_to_quoted("Olá")
    assert {:error, _} = Code.string_to_quoted("Ol@")
    assert {:error, _} = Code.string_to_quoted("Ola?")
    assert {:error, _} = Code.string_to_quoted("Ola!")
  end
end
