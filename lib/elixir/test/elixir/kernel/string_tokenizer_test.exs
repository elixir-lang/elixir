Code.require_file "../test_helper.exs", __DIR__

# TODO: Remove this check once we depend only on 20
if :erlang.system_info(:otp_release) >= '20' do
defmodule Kernel.StringTokenizerTest do
  use ExUnit.Case, async: true

  test "tokenizes vars" do
    assert {:"_12", _, nil} = Code.string_to_quoted!("_12")
    assert {:"ola", _, nil} = Code.string_to_quoted!("ola")
    assert {:"ólá", _, nil} = Code.string_to_quoted!("ólá")
    assert {:"óLÁ", _, nil} = Code.string_to_quoted!("óLÁ")
    assert {:"ólá?", _, nil} = Code.string_to_quoted!("ólá?")
    assert {:"ólá!", _, nil} = Code.string_to_quoted!("ólá!")
    assert {:"こんにちは世界", _, nil} = Code.string_to_quoted!("こんにちは世界")
    assert {:error, _} = Code.string_to_quoted("v@r")
    assert {:error, _} = Code.string_to_quoted("1var")
  end

  test "tokenizes atoms" do
    assert :"_12" = Code.string_to_quoted!(":_12")
    assert :"ola" = Code.string_to_quoted!(":ola")
    assert :"ólá" = Code.string_to_quoted!(":ólá")
    assert :"ólá?" = Code.string_to_quoted!(":ólá?")
    assert :"ólá!" = Code.string_to_quoted!(":ólá!")
    assert :"ól@" = Code.string_to_quoted!(":ól@")
    assert :"ól@!" = Code.string_to_quoted!(":ól@!")
    assert :"ó@@!" = Code.string_to_quoted!(":ó@@!")
    assert :"Ola" = Code.string_to_quoted!(":Ola")
    assert :"Ólá" = Code.string_to_quoted!(":Ólá")
    assert :"ÓLÁ" = Code.string_to_quoted!(":ÓLÁ")
    assert :"ÓLÁ?" = Code.string_to_quoted!(":ÓLÁ?")
    assert :"ÓLÁ!" = Code.string_to_quoted!(":ÓLÁ!")
    assert :"ÓL@!" = Code.string_to_quoted!(":ÓL@!")
    assert :"Ó@@!" = Code.string_to_quoted!(":Ó@@!")
    assert :"こんにちは世界" = Code.string_to_quoted!(":こんにちは世界")
    assert {:error, _} = Code.string_to_quoted(":123")
    assert {:error, _} = Code.string_to_quoted(":@123")
  end

  test "tokenizes keywords" do
    assert ["_12": 0] = Code.string_to_quoted!("[_12: 0]")
    assert ["ola": 0] = Code.string_to_quoted!("[ola: 0]")
    assert ["ólá": 0] = Code.string_to_quoted!("[ólá: 0]")
    assert ["ólá?": 0] = Code.string_to_quoted!("[ólá?: 0]")
    assert ["ólá!": 0] = Code.string_to_quoted!("[ólá!: 0]")
    assert ["ól@": 0] = Code.string_to_quoted!("[ól@: 0]")
    assert ["ól@!": 0] = Code.string_to_quoted!("[ól@!: 0]")
    assert ["ó@@!": 0] = Code.string_to_quoted!("[ó@@!: 0]")
    assert ["Ola": 0] = Code.string_to_quoted!("[Ola: 0]")
    assert ["Ólá": 0] = Code.string_to_quoted!("[Ólá: 0]")
    assert ["ÓLÁ": 0] = Code.string_to_quoted!("[ÓLÁ: 0]")
    assert ["ÓLÁ?": 0] = Code.string_to_quoted!("[ÓLÁ?: 0]")
    assert ["ÓLÁ!": 0] = Code.string_to_quoted!("[ÓLÁ!: 0]")
    assert ["ÓL@!": 0] = Code.string_to_quoted!("[ÓL@!: 0]")
    assert ["Ó@@!": 0] = Code.string_to_quoted!("[Ó@@!: 0]")
    assert ["こんにちは世界": 0] = Code.string_to_quoted!("[こんにちは世界: 0]")
    assert {:error, _} = Code.string_to_quoted("[123: 0]")
    assert {:error, _} = Code.string_to_quoted("[@123: 0]")
  end

  test "tokenizes aliases" do
    assert {:__aliases__, _, [:Ola]} = Code.string_to_quoted!("Ola")
    assert {:__aliases__, _, [:M_123]} = Code.string_to_quoted!("M_123")
    assert {:error, _} = Code.string_to_quoted("Óla")
    assert {:error, _} = Code.string_to_quoted("Olá")
    assert {:error, _} = Code.string_to_quoted("Ol@")
    assert {:error, _} = Code.string_to_quoted("Ola?")
    assert {:error, _} = Code.string_to_quoted("Ola!")
  end
end
end