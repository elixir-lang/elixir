Code.require_file "../test_helper.exs", __DIR__

defmodule ExUnit.BundleTest do
  use ExUnit.Case, async: true

  @moduletag [attribute_tag: :from_module]

  setup _ do
    [setup_tag: :from_module]
  end

  bundle "tags" do
    @bundletag attribute_tag: :from_bundle

    test "from bundle have higher precedence", context do
      assert context.attribute_tag == :from_bundle
    end

    @tag attribute_tag: :from_test
    test "from test have higher precedence", context do
      assert context.attribute_tag == :from_test
    end
  end

  bundle "setup" do
    setup _ do
      [setup_tag: :from_bundle]
    end

    test "from bundle has higher precedence", context do
      assert context.setup_tag == :from_bundle
    end
  end

  bundle "failures" do
    test "when using setup_all inside bundle" do
      assert_raise RuntimeError, "cannot invoke setup_all/2 inside bundle", fn ->
        defmodule Sample do
          use ExUnit.Case

          bundle "hello" do
            setup_all do
              [hello: "world"]
            end
          end
        end
      end
    end

    test "when using bundle inside bundle" do
      assert_raise RuntimeError, "cannot call bundle/2 inside another bundle", fn ->
        defmodule Sample do
          use ExUnit.Case

          bundle "hello" do
            bundle "another" do
            end
          end
        end
      end
    end

    test "when using non-string bundle name" do
      assert_raise ArgumentError, "bundle name must be a string, got: :not_allowed", fn ->
        defmodule Sample do
          use ExUnit.Case

          bundle :not_allowed do
          end
        end
      end
    end
  end

  bundle "test names" do
    test "come from bundle", context do
      assert context.test == :"test test names come from bundle"
    end
  end

  test "from outside bundle", context do
    assert context.attribute_tag == :from_module
    assert context.setup_tag == :from_module
    assert context.test == :"test from outside bundle"
  end
end
