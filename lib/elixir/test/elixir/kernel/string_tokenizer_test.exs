Code.require_file("../test_helper.exs", __DIR__)

# TODO: Remove this check once we depend only on 20
# TODO: Remove String.to_atom/1 when we support 20+
if :erlang.system_info(:otp_release) >= '20' do
  defmodule Kernel.StringTokenizerTest do
    use ExUnit.Case, async: true

    defp var({var, _, nil}), do: var
    defp aliases({:__aliases__, _, [alias]}), do: alias

    test "tokenizes vars" do
      assert Code.string_to_quoted!("_12") |> var() == String.to_atom("_12")
      assert Code.string_to_quoted!("ola") |> var() == String.to_atom("ola")
      assert Code.string_to_quoted!("ólá") |> var() == String.to_atom("ólá")
      assert Code.string_to_quoted!("óLÁ") |> var() == String.to_atom("óLÁ")
      assert Code.string_to_quoted!("ólá?") |> var() == String.to_atom("ólá?")
      assert Code.string_to_quoted!("ólá!") |> var() == String.to_atom("ólá!")
      assert Code.string_to_quoted!("こんにちは世界") |> var() == String.to_atom("こんにちは世界")
      assert {:error, _} = Code.string_to_quoted("v@r")
      assert {:error, _} = Code.string_to_quoted("1var")
    end

    test "tokenizes atoms" do
      assert Code.string_to_quoted!(":_12") == String.to_atom("_12")
      assert Code.string_to_quoted!(":ola") == String.to_atom("ola")
      assert Code.string_to_quoted!(":ólá") == String.to_atom("ólá")
      assert Code.string_to_quoted!(":ólá?") == String.to_atom("ólá?")
      assert Code.string_to_quoted!(":ólá!") == String.to_atom("ólá!")
      assert Code.string_to_quoted!(":ól@") == String.to_atom("ól@")
      assert Code.string_to_quoted!(":ól@!") == String.to_atom("ól@!")
      assert Code.string_to_quoted!(":ó@@!") == String.to_atom("ó@@!")
      assert Code.string_to_quoted!(":Ola") == String.to_atom("Ola")
      assert Code.string_to_quoted!(":Ólá") == String.to_atom("Ólá")
      assert Code.string_to_quoted!(":ÓLÁ") == String.to_atom("ÓLÁ")
      assert Code.string_to_quoted!(":ÓLÁ?") == String.to_atom("ÓLÁ?")
      assert Code.string_to_quoted!(":ÓLÁ!") == String.to_atom("ÓLÁ!")
      assert Code.string_to_quoted!(":ÓL@!") == String.to_atom("ÓL@!")
      assert Code.string_to_quoted!(":Ó@@!") == String.to_atom("Ó@@!")
      assert Code.string_to_quoted!(":こんにちは世界") == String.to_atom("こんにちは世界")
      assert {:error, _} = Code.string_to_quoted(":123")
      assert {:error, _} = Code.string_to_quoted(":@123")
    end

    test "tokenizes keywords" do
      assert Code.string_to_quoted!("[_12: 0]") == [{String.to_atom("_12"), 0}]
      assert Code.string_to_quoted!("[ola: 0]") == [{String.to_atom("ola"), 0}]
      assert Code.string_to_quoted!("[ólá: 0]") == [{String.to_atom("ólá"), 0}]
      assert Code.string_to_quoted!("[ólá?: 0]") == [{String.to_atom("ólá?"), 0}]
      assert Code.string_to_quoted!("[ólá!: 0]") == [{String.to_atom("ólá!"), 0}]
      assert Code.string_to_quoted!("[ól@: 0]") == [{String.to_atom("ól@"), 0}]
      assert Code.string_to_quoted!("[ól@!: 0]") == [{String.to_atom("ól@!"), 0}]
      assert Code.string_to_quoted!("[ó@@!: 0]") == [{String.to_atom("ó@@!"), 0}]
      assert Code.string_to_quoted!("[Ola: 0]") == [{String.to_atom("Ola"), 0}]
      assert Code.string_to_quoted!("[Ólá: 0]") == [{String.to_atom("Ólá"), 0}]
      assert Code.string_to_quoted!("[ÓLÁ: 0]") == [{String.to_atom("ÓLÁ"), 0}]
      assert Code.string_to_quoted!("[ÓLÁ?: 0]") == [{String.to_atom("ÓLÁ?"), 0}]
      assert Code.string_to_quoted!("[ÓLÁ!: 0]") == [{String.to_atom("ÓLÁ!"), 0}]
      assert Code.string_to_quoted!("[ÓL@!: 0]") == [{String.to_atom("ÓL@!"), 0}]
      assert Code.string_to_quoted!("[Ó@@!: 0]") == [{String.to_atom("Ó@@!"), 0}]
      assert Code.string_to_quoted!("[こんにちは世界: 0]") == [{String.to_atom("こんにちは世界"), 0}]
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
end
