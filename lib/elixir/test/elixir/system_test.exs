Code.require_file "test_helper.exs", __DIR__

defmodule SystemTest do
  use ExUnit.Case
  import PathHelpers

  test "build_info/0" do
    assert is_map System.build_info
    assert not is_nil(System.build_info[:version])
    assert not is_nil(System.build_info[:tag])
    assert not is_nil(System.build_info[:date])
  end

  test "cwd/0" do
    assert is_binary System.cwd
    assert is_binary System.cwd!
  end

  if :file.native_name_encoding == :utf8 do
    test "cwd/0 with utf8" do
      File.mkdir_p(tmp_path("héllò"))

      File.cd!(tmp_path("héllò"), fn ->
        assert Path.basename(System.cwd!) == "héllò"
      end)
    after
      File.rm_rf tmp_path("héllò")
    end
  end

  test "user_home/0" do
    assert is_binary System.user_home
    assert is_binary System.user_home!
  end

  test "tmp_dir/0" do
    assert is_binary System.tmp_dir
    assert is_binary System.tmp_dir!
  end

  test "argv/0" do
    list = elixir('-e "IO.inspect System.argv" -- -o opt arg1 arg2 --long-opt 10')
    {args, _} = Code.eval_string list, []
    assert args == ["-o", "opt", "arg1", "arg2", "--long-opt", "10"]
  end

  @test_var "SYSTEM_ELIXIR_ENV_TEST_VAR"

  test "*_env/*" do
    assert System.get_env(@test_var) == nil
    System.put_env(@test_var, "SAMPLE")
    assert System.get_env(@test_var) == "SAMPLE"
    assert System.get_env()[@test_var] == "SAMPLE"

    System.delete_env(@test_var)
    assert System.get_env(@test_var) == nil

    System.put_env(%{@test_var => "OTHER_SAMPLE"})
    assert System.get_env(@test_var) == "OTHER_SAMPLE"
  end

  test "cmd/2" do
    assert {"hello\n", 0} = System.cmd "echo", ["hello"]
  end

  test "cmd/3 (with options)" do
    assert {["hello\n"], 0} = System.cmd "echo", ["hello"],
                                into: [], cd: System.cwd!, env: %{"foo" => "bar"},
                                arg0: "hecho", stderr_to_stdout: true, parallelism: true
  end

  test "cmd/2 with absolute and relative paths" do
    echo2 = tmp_path("echo2")
    File.mkdir_p! Path.dirname(echo2)
    File.cp! System.find_executable("echo"), echo2

    File.cd! Path.dirname(echo2), fn ->
      assert :enoent = catch_error(System.cmd("echo2", ["hello"]))
      assert {"hello\n", 0} = System.cmd(Path.join(System.cwd!, "echo2"), ["hello"])
    end
  after
    File.rm_rf(tmp_path("echo2"))
  end

  test "find_executable/1" do
    assert System.find_executable("erl")
    assert is_binary System.find_executable("erl")
    assert !System.find_executable("does-not-really-exist-from-elixir")
  end
end
