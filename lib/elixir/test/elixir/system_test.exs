Code.require_file "test_helper.exs", __DIR__

defmodule SystemTest do
  use ExUnit.Case
  import PathHelpers

  test "build_info" do
    assert not nil?(System.build_info[:version])
    assert not nil?(System.build_info[:tag])
    assert not nil?(System.build_info[:date])
  end

  test "cwd" do
    assert is_binary System.cwd
    assert is_binary System.cwd!
  end

  if :file.native_name_encoding == :utf8 do
    test "cwd_with_utf8" do
      File.mkdir_p(tmp_path("héllò"))

      File.cd!(tmp_path("héllò"), fn ->
        assert Path.basename(System.cwd!) == "héllò"
      end)
    after
      File.rm_rf tmp_path("héllò")
    end
  end

  test "user_home" do
    assert is_binary System.user_home
    assert is_binary System.user_home!
  end

  test "tmp_dir" do
    assert is_binary System.tmp_dir
    assert is_binary System.tmp_dir!
  end

  test "argv" do
    list = elixir('-e "IO.inspect System.argv" -- -o opt arg1 arg2 --long-opt 10')
    { args, _ } = Code.eval_string list, []
    assert args == ["-o", "opt", "arg1", "arg2", "--long-opt", "10"]
  end

  @test_var "SYSTEM_ELIXIR_ENV_TEST_VAR"

  test "env" do
    assert System.get_env(@test_var) == nil
    System.put_env(@test_var, "SAMPLE")
    assert System.get_env(@test_var) == "SAMPLE"
    assert System.get_env()[@test_var] == "SAMPLE"

    System.delete_env(@test_var)
    assert System.get_env(@test_var) == nil

    System.put_env([{ @test_var, "OTHER_SAMPLE" }])
    assert System.get_env(@test_var) == "OTHER_SAMPLE"
  end

  test "cmd" do
    assert is_binary(System.cmd "echo hello")
    assert is_list(System.cmd 'echo hello')
  end

  test "find_executable with binary" do
    assert System.find_executable("erl")
    assert is_binary System.find_executable("erl")
    assert !System.find_executable("does-not-really-exist-from-elixir")
  end

  test "find_executable with list" do
    assert System.find_executable('erl')
    assert is_list System.find_executable('erl')
    assert !System.find_executable('does-not-really-exist-from-elixir')
  end
end
