Code.require_file("test_helper.exs", __DIR__)

defmodule SystemTest do
  use ExUnit.Case
  import PathHelpers

  test "build_info/0" do
    build_info = System.build_info()
    assert is_map(build_info)
    assert is_binary(build_info[:build])
    assert is_binary(build_info[:date])
    assert is_binary(build_info[:revision])
    assert is_binary(build_info[:version])
    assert is_binary(build_info[:otp_release])

    if build_info[:revision] != "" do
      assert String.length(build_info[:revision]) >= 7
    end

    version_file = Path.join([__DIR__, "../../../..", "VERSION"]) |> Path.expand()
    {:ok, version} = File.read(version_file)
    assert build_info[:version] == String.trim(version)
    assert build_info[:build] =~ "compiled with Erlang/OTP"
  end

  test "user_home/0" do
    assert is_binary(System.user_home())
    assert is_binary(System.user_home!())
  end

  test "tmp_dir/0" do
    assert is_binary(System.tmp_dir())
    assert is_binary(System.tmp_dir!())
  end

  test "endianness/0" do
    assert System.endianness() in [:little, :big]
    assert System.endianness() == System.compiled_endianness()
  end

  test "argv/0" do
    list = elixir('-e "IO.inspect System.argv" -- -o opt arg1 arg2 --long-opt 10')
    {args, _} = Code.eval_string(list, [])
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

    assert_raise ArgumentError, ~r[cannot execute System.put_env/2 for key with \"=\"], fn ->
      System.put_env("FOO=BAR", "BAZ")
    end
  end

  test "cmd/2 raises for null bytes" do
    assert_raise ArgumentError, ~r"cannot execute System.cmd/3 for program with null byte", fn ->
      System.cmd("null\0byte", [])
    end
  end

  test "cmd/3 raises with non-binary arguments" do
    assert_raise ArgumentError, ~r"all arguments for System.cmd/3 must be binaries", fn ->
      System.cmd("ls", ['/usr'])
    end
  end

  describe "Windows" do
    @describetag :windows

    test "cmd/2" do
      assert {"hello\r\n", 0} = System.cmd("cmd", ~w[/c echo hello])
    end

    test "cmd/3 (with options)" do
      assert {["hello\r\n"], 0} =
               System.cmd(
                 "cmd",
                 ~w[/c echo hello],
                 into: [],
                 cd: File.cwd!(),
                 env: %{"foo" => "bar", "baz" => nil},
                 arg0: "echo",
                 stderr_to_stdout: true,
                 parallelism: true
               )
    end

    @echo "echo-elixir-test"

    test "cmd/2 with absolute and relative paths" do
      echo = tmp_path(@echo)
      File.mkdir_p!(Path.dirname(echo))
      File.cp!(System.find_executable("cmd"), echo)

      File.cd!(Path.dirname(echo), fn ->
        # There is a bug in OTP where find_executable is finding
        # entries on the current directory. If this is the case,
        # we should avoid the assertion below.
        unless System.find_executable(@echo) do
          assert :enoent = catch_error(System.cmd(@echo, ~w[/c echo hello]))
        end

        assert {"hello\r\n", 0} =
                 System.cmd(Path.join(File.cwd!(), @echo), ~w[/c echo hello], [{:arg0, "echo"}])
      end)
    after
      File.rm_rf!(tmp_path(@echo))
    end
  end

  describe "Unix" do
    @describetag :unix

    test "cmd/2" do
      assert {"hello\n", 0} = System.cmd("echo", ["hello"])
    end

    test "cmd/3 (with options)" do
      opts = [
        into: [],
        cd: File.cwd!(),
        env: %{"foo" => "bar", "baz" => nil},
        arg0: "echo",
        stderr_to_stdout: true,
        parallelism: true
      ]

      assert {["hello\n"], 0} = System.cmd("echo", ["hello"], opts)
    end

    @echo "echo-elixir-test"

    test "cmd/2 with absolute and relative paths" do
      echo = tmp_path(@echo)
      File.mkdir_p!(Path.dirname(echo))
      File.cp!(System.find_executable("echo"), echo)

      File.cd!(Path.dirname(echo), fn ->
        # There is a bug in OTP where find_executable is finding
        # entries on the current directory. If this is the case,
        # we should avoid the assertion below.
        unless System.find_executable(@echo) do
          assert :enoent = catch_error(System.cmd(@echo, ["hello"]))
        end

        assert {"hello\n", 0} =
                 System.cmd(Path.join(File.cwd!(), @echo), ["hello"], [{:arg0, "echo"}])
      end)
    after
      File.rm_rf!(tmp_path(@echo))
    end
  end

  test "find_executable/1" do
    assert System.find_executable("erl")
    assert is_binary(System.find_executable("erl"))
    assert !System.find_executable("does-not-really-exist-from-elixir")

    message = ~r"cannot execute System.find_executable/1 for program with null byte"

    assert_raise ArgumentError, message, fn ->
      System.find_executable("null\0byte")
    end
  end

  test "monotonic_time/0" do
    assert is_integer(System.monotonic_time())
  end

  test "monotonic_time/1" do
    assert is_integer(System.monotonic_time(:nanosecond))
    assert abs(System.monotonic_time(:microsecond)) < abs(System.monotonic_time(:nanosecond))
  end

  test "system_time/0" do
    assert is_integer(System.system_time())
  end

  test "system_time/1" do
    assert is_integer(System.system_time(:nanosecond))
    assert abs(System.system_time(:microsecond)) < abs(System.system_time(:nanosecond))
  end

  test "time_offset/0 and time_offset/1" do
    assert is_integer(System.time_offset())
    assert is_integer(System.time_offset(:second))
  end

  test "os_time/0" do
    assert is_integer(System.os_time())
  end

  test "os_time/1" do
    assert is_integer(System.os_time(:nanosecond))
    assert abs(System.os_time(:microsecond)) < abs(System.os_time(:nanosecond))
  end

  test "unique_integer/0 and unique_integer/1" do
    assert is_integer(System.unique_integer())
    assert System.unique_integer([:positive]) > 0

    assert System.unique_integer([:positive, :monotonic]) <
             System.unique_integer([:positive, :monotonic])
  end

  test "convert_time_unit/3" do
    time = System.monotonic_time(:nanosecond)
    assert abs(System.convert_time_unit(time, :nanosecond, :microsecond)) < abs(time)
  end

  test "schedulers/0" do
    assert System.schedulers() >= 1
  end

  test "schedulers_online/0" do
    assert System.schedulers_online() >= 1
  end

  test "otp_release/0" do
    otp_release = System.otp_release()
    assert is_binary(System.otp_release())
    version = otp_release |> Version.parse!()

    assert version.major == :erlang.system_info(:otp_release) |> List.to_integer()
    assert version.major >= 20
  end
end
