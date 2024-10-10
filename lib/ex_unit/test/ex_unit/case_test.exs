Code.require_file("../test_helper.exs", __DIR__)

defmodule ExUnit.CaseTest do
  use ExUnit.Case, async: true

  ExUnit.Case.register_attribute(__MODULE__, :foo)
  ExUnit.Case.register_attribute(__MODULE__, :bar, accumulate: true)
  ExUnit.Case.register_attribute(__MODULE__, :baz)
  ExUnit.Case.register_describe_attribute(__MODULE__, :describe_foo)
  ExUnit.Case.register_describe_attribute(__MODULE__, :describe_bar, accumulate: true)
  ExUnit.Case.register_describe_attribute(__MODULE__, :describe_baz)
  ExUnit.Case.register_module_attribute(__MODULE__, :module_foo)
  ExUnit.Case.register_module_attribute(__MODULE__, :module_bar, accumulate: true)
  ExUnit.Case.register_module_attribute(__MODULE__, :module_baz)

  @moduletag :moduletag
  @module_foo :hello
  @module_bar :world

  test "defines __ex_unit__" do
    assert %ExUnit.TestModule{name: __MODULE__, tests: tests} = __ex_unit__()
    assert length(tests) > 0
  end

  @tag hello: false
  @tag :hello
  @tag world: :bad
  @tag world: :good
  test "tags", context do
    line = __ENV__.line - 1
    assert context[:module] == __MODULE__
    assert context[:test] == __ENV__.function |> elem(0)
    assert context[:line] == line
    assert context[:async] == true
    assert context[:hello] == true
    assert context[:world] == :good
  end

  test "reset tags", context do
    assert is_nil(context[:hello])
    assert is_nil(context[:world])
  end

  # tags are passed to setup_all
  setup_all context do
    assert context.async
    assert context.module == __MODULE__
    %{moduletag_from_setup_all: context[:moduletag]}
  end

  test "module tags", context do
    assert context[:moduletag] == true
    assert context[:moduletag_from_setup_all] == true
  end

  @tag moduletag: :overridden
  test "module tags can be overridden", context do
    assert context[:moduletag] == :overridden
  end

  @foo :hello
  @bar :world
  test "registered attributes are in context", context do
    assert context.registered.foo == :hello
    assert context.registered.bar == [:world]
    assert context.registered.baz == nil
  end

  test "registered attributes are set per test", context do
    assert context.registered.foo == nil
    assert context.registered.bar == []
  end

  describe "with attributes" do
    @describe_foo :hello
    @describe_bar :world

    test "registered subscribe attributes are in context", context do
      assert context.registered.describe_foo == :hello
      assert context.registered.describe_bar == [:world]
      assert context.registered.describe_baz == nil
    end
  end

  describe "without attributes" do
    test "registered subscribe attributes are set per subscribe", context do
      assert context.registered.describe_foo == nil
      assert context.registered.describe_bar == []
    end
  end

  test "registered module attributes are in context", context do
    assert context.registered.module_foo == :hello
    assert context.registered.module_bar == [:world]
    assert context.registered.module_baz == nil
  end

  test "registered module attributes stay in context", context do
    assert context.registered.module_foo == :hello
    assert context.registered.module_bar == [:world]
  end

  test "raises when same name is registered twice" do
    assert_raise ArgumentError, "cannot register attribute :foo multiple times", fn ->
      defmodule AcrossLevelDoubleRegisterTest do
        use ExUnit.Case
        ExUnit.Case.register_attribute(__MODULE__, :foo)
        ExUnit.Case.register_module_attribute(__MODULE__, :foo)
      end
    end
  end

  test "raises when attribute is set before being registered" do
    assert_raise RuntimeError, "you must set @foo after it has been registered", fn ->
      defmodule SetBeforeRegisterTest do
        use ExUnit.Case
        @foo true
        ExUnit.Case.register_attribute(__MODULE__, :foo, accumulate: true)
      end
    end
  end

  test "raises when name is longer than 255 characters" do
    assert_raise SystemLimitError,
                 ~r/must be shorter than 255 characters, got: "test a{256}"/,
                 fn ->
                   defmodule LongNameTest do
                     use ExUnit.Case

                     test String.duplicate("a", 256)
                   end
                 end

    assert_raise SystemLimitError,
                 ~r/must be shorter than 255 characters, got: "test a{100} a{156}"/,
                 fn ->
                   defmodule LongDescribeNameTest do
                     use ExUnit.Case

                     describe String.duplicate("a", 100) do
                       test String.duplicate("a", 156)
                     end
                   end
                 end
  end

  test "warns when using @tag outside of describe" do
    stderr =
      ExUnit.CaptureIO.capture_io(:stderr, fn ->
        defmodule TagOutsideOfDescribe do
          use ExUnit.Case, register: false

          @tag :foo
          describe "bar" do
            test "baz" do
            end
          end
        end
      end)

    assert stderr =~ "found unused @tag before \"describe\", did you mean to use @describetag?"
  end
end

defmodule ExUnit.DoubleCaseTestAsyncFirst do
  use ExUnit.Case, async: true
  use ExUnit.Case

  test "async must be true", context do
    assert context.async
  end
end

defmodule ExUnit.DoubleCaseTestAsyncLast do
  use ExUnit.Case
  use ExUnit.Case, async: true

  test "async must be true", context do
    assert context.async
  end
end

defmodule ExUnit.CaseTest.TmpDir do
  use ExUnit.Case

  @moduletag :tmp_dir

  defp ends_with_short_hash?(string) do
    string
    |> binary_slice(-9..-1)
    |> String.starts_with?("-")
  end

  defp ends_with_short_hash_and_extra_path?(string, extra_path) do
    extra_path = "/" <> extra_path
    extra_path_length = String.length(extra_path)

    case String.split_at(string, -extra_path_length) do
      {tmp_dir_base, extra_path_new} when extra_path_new == extra_path ->
        ends_with_short_hash?(tmp_dir_base)

      _ ->
        false
    end
  end

  defp starts_with_path?(tmp_dir, path) do
    String.starts_with?(tmp_dir, Path.expand(path))
  end

  test "default path", context do
    assert starts_with_path?(context.tmp_dir, "tmp/ExUnit.CaseTest.TmpDir/test-default-path-")
    assert ends_with_short_hash?(context.tmp_dir)
    assert File.ls!(context.tmp_dir) == []
  end

  test "escapes foo?/0", context do
    assert starts_with_path?(context.tmp_dir, "tmp/ExUnit.CaseTest.TmpDir/test-escapes-foo--0-")
    assert ends_with_short_hash?(context.tmp_dir)
  end

  @tag tmp_dir: "foo/bar"
  test "custom path", context do
    assert starts_with_path?(context.tmp_dir, "tmp/ExUnit.CaseTest.TmpDir/test-custom-path-")
    assert ends_with_short_hash_and_extra_path?(context.tmp_dir, "foo/bar")
  end

  @tag tmp_dir: false
  test "disabled", context do
    refute context[:tmp_dir]
  end

  describe "colliding test names" do
    test "foo-bar", context do
      assert starts_with_path?(
               context.tmp_dir,
               "tmp/ExUnit.CaseTest.TmpDir/test-colliding-test-names-foo-bar-"
             )

      assert String.ends_with?(context.tmp_dir, "-2489e2ce")
    end

    test "foo+bar", context do
      assert starts_with_path?(
               context.tmp_dir,
               "tmp/ExUnit.CaseTest.TmpDir/test-colliding-test-names-foo-bar-"
             )

      assert String.ends_with?(context.tmp_dir, "-9633ed5f")
    end
  end
end

defmodule ExUnit.BadOptsCase do
  use ExUnit.Case, async: true

  test "raises if passed something other than options" do
    assert_raise ArgumentError, ~r/must be a list of options, got: "not a list of options"/, fn ->
      defmodule MyBadCase do
        use ExUnit.Case, "not a list of options"
      end
    end
  end
end

defmodule ExUnit.CaseTest.GetLastRegisteredTestHelper do
  defmacro escaped_get_last_registered_test do
    Macro.escape(ExUnit.Case.get_last_registered_test(__CALLER__))
  end
end

defmodule ExUnit.CaseTest.GetLastRegisteredTestTest do
  use ExUnit.Case, async: true
  import ExUnit.CaseTest.GetLastRegisteredTestHelper

  last = ExUnit.Case.get_last_registered_test(__MODULE__)

  test "returns nil if called before any test has been registered" do
    assert unquote(last) == nil
  end

  test "returns the current test if call is within test body", %{test: name} do
    assert %ExUnit.Test{
             name: ^name,
             module: __MODULE__,
             state: nil,
             time: 0
           } = escaped_get_last_registered_test()
  end

  last = ExUnit.Case.get_last_registered_test(__MODULE__)

  test "returns the previous test if call is outside test body" do
    assert %ExUnit.Test{name: :"test returns the current test if call is within test body"} =
             unquote(Macro.escape(last))
  end

  test "raises if given module is already compiled" do
    assert_raise ArgumentError, ~r/could not call Module\.get_last_attribute\/2/, fn ->
      ExUnit.Case.get_last_registered_test(__MODULE__)
    end
  end

  @moduletag tag1: :foo
  describe "tags" do
    @describetag tag2: :bar

    @tag tag3: :baz
    test "includes data available in test context", context do
      assert %ExUnit.Test{tags: %{tag1: :foo, tag2: :bar, tag3: :baz} = tags} =
               escaped_get_last_registered_test()

      assert tags == Map.take(context, Map.keys(tags))
    end
  end
end
