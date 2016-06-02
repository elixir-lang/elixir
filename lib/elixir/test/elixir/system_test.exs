Code.require_file "test_helper.exs", __DIR__

defmodule SystemTest do
  use ExUnit.Case
  import PathHelpers

  test "build_info/0" do
    build_info = System.build_info
    assert is_map build_info
    assert is_binary build_info[:build]
    assert is_binary build_info[:date]
    assert is_binary build_info[:revision]
    assert is_binary build_info[:version]

    if build_info[:revision] != "" do
      assert String.length(build_info[:revision]) == 7
    end

    version_file = Path.join([__DIR__, "../../../..", "VERSION"]) |> Path.expand
    {:ok, version} = File.read(version_file)
    assert build_info[:version] == String.trim(version)
    assert build_info[:build] != ""
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

  test "endianness/0" do
    assert System.endianness in [:little, :big]
    assert System.endianness == System.compiled_endianness
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

  if windows? do

    test "cmd/2 win" do
      assert {"hello\r\n", 0} = System.cmd "cmd", ~w[/c echo hello]
    end

    test "cmd/3 (with options) win" do
      assert {["hello\r\n"], 0} = System.cmd "cmd", ~w[/c echo hello],
                                  into: [], cd: System.cwd!, env: %{"foo" => "bar", "baz" => nil},
                                  arg0: "echo", stderr_to_stdout: true, parallelism: true
    end

    @echo "echo-elixir-test"

    test "cmd/2 with absolute and relative paths win" do
      echo = tmp_path(@echo)
      File.mkdir_p! Path.dirname(echo)
      File.cp! System.find_executable("cmd"), echo

      File.cd! Path.dirname(echo), fn ->
        # There is a bug in OTP where find_executable is finding
        # entries on the current directory. If this is the case,
        # we should avoid the assertion below.
        unless System.find_executable(@echo) do
          assert :enoent = catch_error(System.cmd(@echo, ~w[/c echo hello]))
        end

        assert {"hello\r\n", 0} = System.cmd(Path.join(System.cwd!, @echo), ~w[/c echo hello], [{:arg0, "echo"}])
      end
    after
      File.rm_rf! Path.dirname(tmp_path(@echo))
    end

  else

    test "cmd/2 unix" do
      assert {"hello\n", 0} = System.cmd "echo", ["hello"]
    end

    test "cmd/3 (with options) unix" do
      assert {["hello\n"], 0} = System.cmd "echo", ["hello"],
                                  into: [], cd: System.cwd!, env: %{"foo" => "bar", "baz" => nil},
                                  arg0: "echo", stderr_to_stdout: true, parallelism: true
    end

    @echo "echo-elixir-test"

    test "cmd/2 with absolute and relative paths unix" do
      echo = tmp_path(@echo)
      File.mkdir_p! Path.dirname(echo)
      File.cp! System.find_executable("echo"), echo

      File.cd! Path.dirname(echo), fn ->
        # There is a bug in OTP where find_executable is finding
        # entries on the current directory. If this is the case,
        # we should avoid the assertion below.
        unless System.find_executable(@echo) do
          assert :enoent = catch_error(System.cmd(@echo, ["hello"]))
        end

        assert {"hello\n", 0} = System.cmd(Path.join(System.cwd!, @echo), ["hello"], [{:arg0, "echo"}])
      end
    after
      File.rm_rf! tmp_path(@echo)
    end

  end


  test "find_executable/1" do
    assert System.find_executable("erl")
    assert is_binary System.find_executable("erl")
    assert !System.find_executable("does-not-really-exist-from-elixir")
  end

  test "monotonic_time/0" do
    assert is_integer(System.monotonic_time())
  end

  test "monotonic_time/1" do
    assert is_integer(System.monotonic_time(:nanoseconds))
    assert abs(System.monotonic_time(:microseconds)) < abs(System.monotonic_time(:nanoseconds))
  end

  test "system_time/0" do
    assert is_integer(System.system_time())
  end

  test "system_time/1" do
    assert is_integer(System.system_time(:nanoseconds))
    assert abs(System.system_time(:microseconds)) < abs(System.system_time(:nanoseconds))
  end

  test "time_offset/0 and time_offset/1" do
    assert is_integer(System.time_offset())
    assert is_integer(System.time_offset(:seconds))
  end

  test "os_time/0" do
    assert is_integer(System.os_time())
  end

  test "os_time/1" do
    assert is_integer(System.os_time(:nanoseconds))
    assert abs(System.os_time(:microseconds)) < abs(System.os_time(:nanoseconds))
  end

  test "unique_integer/0 and unique_integer/1" do
    assert is_integer(System.unique_integer())
    assert System.unique_integer([:positive]) > 0
    assert System.unique_integer([:positive, :monotonic]) < System.unique_integer([:positive, :monotonic])
  end

  test "convert_time_unit/3" do
    time = System.monotonic_time(:nanoseconds)
    assert abs(System.convert_time_unit(time, :nanoseconds, :microseconds)) < abs(time)
  end
end
