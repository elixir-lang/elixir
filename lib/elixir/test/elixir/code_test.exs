Code.require_file "test_helper.exs", __DIR__

defmodule CodeTest do
  use ExUnit.Case, async: true
  import PathHelpers

  def one, do: 1

  contents = quote do
    defmodule CodeTest.Sample do
      def eval_quoted_info, do: { __MODULE__, __FILE__, __ENV__.line }
    end
  end

  Code.eval_quoted contents, [], file: "sample.ex", line: 13

  test :eval do
    assert Code.eval("1 + 2") == { 3, [] }
    assert { 3, _ } = Code.eval("a + b", [a: 1, b: 2], __ENV__.location)
  end

  test :eval_with_scope do
    assert Code.eval("one", [], delegate_locals_to: __MODULE__) == { 1, [] }
  end

  test :eval_with_requires do
    assert Code.eval("Kernel.if true, do: :ok", [], requires: [Z, Kernel]) == { :ok, [] }
  end

  test :eval_quoted do
    assert Code.eval_quoted(quote(do: 1 + 2)) == { 3, [] }
    assert CodeTest.Sample.eval_quoted_info() == { CodeTest.Sample, "sample.ex", 13 }
  end

  test :eval_quoted_with_env do
    alias :lists, as: MyList
    assert Code.eval_quoted(quote(do: MyList.flatten [[1,2,3]]), [], __ENV__) == { [1,2,3],[] }
  end

  test :require do
    Code.require_file fixture_path("code_sample.exs")
    assert fixture_path("code_sample.exs") in Code.loaded_files
    assert Code.require_file(fixture_path("code_sample.exs")) == nil

    Code.unload_files [fixture_path("code_sample.exs")]
    refute fixture_path("code_sample.exs") in Code.loaded_files
    assert Code.require_file(fixture_path("code_sample.exs")) != nil
  end

  test :path_manipulation do
    path = Path.join(__DIR__, "binary")
    Code.prepend_path path
    assert binary_to_list(path) in :code.get_path

    Code.delete_path path
    refute binary_to_list(path) in :code.get_path
  end

  test :file do
    assert :filename.absname(__FILE__) == __FILE__
  end

  test :string_to_ast do
    assert Code.string_to_ast("1 + 2") == { :ok, quote hygiene: [imports: false], line: 1, do: 1 + 2 }
    assert Code.string_to_ast("1 + 2; 3 + 4") == { :ok, quote hygiene: [imports: false], line: 1, do: (1 + 2; 3 + 4) }
    assert { :error, _ } = Code.string_to_ast("a.1")
  end

  test :string_to_ast_existing_atoms_only do
    assert :badarg = catch_error(Code.string_to_ast(":thereisnosuchatom", existing_atoms_only: true))
    assert :badarg = catch_error(Code.string_to_ast!(":thereisnosuchatom", existing_atoms_only: true))
  end

  test :string_to_ast! do
    assert Code.string_to_ast!("1 + 2") == quote hygiene: [imports: false], line: 1, do: 1 + 2

    assert_raise SyntaxError, fn ->
      Code.string_to_ast!("a.1")
    end

    assert_raise TokenMissingError, fn ->
      Code.string_to_ast!("1 +")
    end
  end

  test :compile_source do
    compile = __MODULE__.__info__(:compile)

    # Erlang has a bug that does not allow us to set the source
    # when compiling forms. In such cases, the source will be
    # under compile option. This is fixed and future Erlang
    # version will return the proper source always (source2).
    options = :proplists.get_value(:options, compile, [])
    source1 = :proplists.get_value(:source, options, nil)
    source2 = :proplists.get_value(:source, compile, nil)

    assert (source1 || source2) == binary_to_list(__FILE__)
  end

  test :compile_info_returned_with_source_accessible_through_keyword_module do
    compile = __MODULE__.__info__(:compile)
    assert Keyword.get(compile, :source) != nil
  end

  test :ensure_loaded? do
    assert Code.ensure_loaded?(__MODULE__)
    refute Code.ensure_loaded?(Unknown.Module)
  end

  test :ensure_compiled? do
    assert Code.ensure_compiled?(__MODULE__)
    refute Code.ensure_compiled?(Unknown.Module)
  end
end
