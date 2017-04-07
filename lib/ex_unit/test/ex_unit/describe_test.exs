Code.require_file "../test_helper.exs", __DIR__

defmodule ExUnit.DescribeTest do
  use ExUnit.Case, async: true

  @moduletag [attribute_tag: :from_module]

  setup _ do
    [setup_tag: :from_module]
  end

  describe "tags" do
    @describetag attribute_tag: :from_describe

    test "from describe have higher precedence", context do
      assert context.attribute_tag == :from_describe
    end

    @tag attribute_tag: :from_test
    test "from test have higher precedence", context do
      assert context.attribute_tag == :from_test
    end
  end

  describe "setup" do
    setup _ do
      [setup_tag: :from_describe]
    end

    test "from describe has higher precedence", context do
      assert context.setup_tag == :from_describe
    end
  end

  describe "failures" do
    test "when using setup_all inside describe" do
      assert_raise RuntimeError, ~r"cannot invoke setup_all/2 inside describe", fn ->
        defmodule Sample do
          use ExUnit.Case

          describe "hello" do
            setup_all do
              [hello: "world"]
            end
          end
        end
      end
    end

    test "when using describe inside describe" do
      assert_raise RuntimeError, ~r"cannot call describe/2 inside another describe", fn ->
        defmodule Sample do
          use ExUnit.Case

          describe "hello" do
            describe "another" do
            end
          end
        end
      end
    end

    test "when using non-string describe name" do
      assert_raise ArgumentError, ~r"describe name must be a string, got: :not_allowed", fn ->
        defmodule Sample do
          use ExUnit.Case

          describe :not_allowed do
          end
        end
      end
    end
  end

  describe "test names" do
    test "merge describe information", context do
      assert context.test == :"test test names merge describe information"
    end
  end

  test "attributes from outside describe", context do
    assert context.attribute_tag == :from_module
    assert context.setup_tag == :from_module
    assert context.test == :"test attributes from outside describe"
  end
end
