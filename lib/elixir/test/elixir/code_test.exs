Code.require_file("test_helper.exs", __DIR__)

defmodule CodeTest do
  use ExUnit.Case, async: true

  doctest Code
  import PathHelpers

  def genmodule(name) do
    defmodule name do
      Kernel.LexicalTracker.remote_references(__ENV__.lexical_tracker)
    end
  end

  contents =
    quote do
      defmodule CodeTest.Sample do
        def eval_quoted_info, do: {__MODULE__, __ENV__.file, __ENV__.line}
      end
    end

  Code.eval_quoted(contents, [], file: "sample.ex", line: 13)

  describe "eval_string/1-3" do
    test "correctly evaluates a string of code" do
      assert Code.eval_string("1 + 2") == {3, []}
      assert Code.eval_string("two = 1 + 1") == {2, [two: 2]}
    end

    test "supports a %Macro.Env{} struct as the third argument" do
      assert {3, _} = Code.eval_string("a + b", [a: 1, b: 2], __ENV__)
    end

    test "returns bindings from a different context" do
      assert Code.eval_string("var!(a, Sample) = 1") == {1, [{{:a, Sample}, 1}]}
    end

    test "supports unnamed scopes" do
      assert {%RuntimeError{}, [a: %RuntimeError{}]} =
               Code.eval_string("a = (try do (raise \"hello\") rescue e -> e end)")
    end

    test "supports the :requires option" do
      assert Code.eval_string("Kernel.if true, do: :ok", [], requires: [Z, Kernel]) == {:ok, []}
    end

    test "with many options" do
      options = [
        functions: [{Kernel, [is_atom: 1]}],
        macros: [{Kernel, [and: 2]}],
        aliases: [{K, Kernel}],
        requires: [Kernel]
      ]

      code = "is_atom(:foo) and K.is_list([])"
      assert Code.eval_string(code, [], options) == {true, []}
    end

    test "yields the correct stacktrace" do
      try do
        Code.eval_string("<<a::size(b)>>", a: :a, b: :b)
      rescue
        _ ->
          assert Enum.any?(__STACKTRACE__, &(elem(&1, 0) == __MODULE__))
      end
    end

    test "raises streamlined argument errors" do
      assert_raise ArgumentError,
                   ~r"argument error while evaluating at line 1",
                   fn -> Code.eval_string("a <> b", [a: :a, b: :b]) end

      assert_raise ArgumentError,
                   ~r"argument error while evaluating example.ex at line 1",
                   fn -> Code.eval_string("a <> b", [a: :a, b: :b], file: "example.ex") end

      assert_raise ArgumentError,
                   ~r"argument error while evaluating example.ex between lines 1 and 2",
                   fn -> Code.eval_string("a <>\nb", [a: :a, b: :b], file: "example.ex") end
    end
  end

  test "eval_quoted/1" do
    assert Code.eval_quoted(quote(do: 1 + 2)) == {3, []}
    assert CodeTest.Sample.eval_quoted_info() == {CodeTest.Sample, "sample.ex", 13}
  end

  test "eval_quoted/2 with a %Macro.Env{} struct as the second argument" do
    alias :lists, as: MyList

    assert Code.eval_quoted(quote(do: MyList.flatten([[1, 2, 3]])), [], __ENV__) ==
             {[1, 2, 3], []}
  end

  test "eval_file/1" do
    assert Code.eval_file(fixture_path("code_sample.exs")) == {3, [var: 3]}

    assert_raise Code.LoadError, fn ->
      Code.eval_file("non_existent.exs")
    end
  end

  test "compile_file/1" do
    assert Code.compile_file(fixture_path("code_sample.exs")) == []
    refute fixture_path("code_sample.exs") in Code.required_files()
  end

  test "require_file/1" do
    assert Code.require_file(fixture_path("code_sample.exs")) == []
    assert fixture_path("code_sample.exs") in Code.required_files()
    assert Code.require_file(fixture_path("code_sample.exs")) == nil

    Code.unrequire_files([fixture_path("code_sample.exs")])
    refute fixture_path("code_sample.exs") in Code.required_files()
    assert Code.require_file(fixture_path("code_sample.exs")) != nil
  after
    Code.unrequire_files([fixture_path("code_sample.exs")])
  end

  describe "string_to_quoted/2" do
    test "converts strings to quoted expressions" do
      assert Code.string_to_quoted("1 + 2") == {:ok, {:+, [line: 1], [1, 2]}}
      assert Code.string_to_quoted("a.1") == {:error, {1, "syntax error before: ", "\"1\""}}
    end

    test "converts strings to quoted with column information" do
      string_to_quoted = &Code.string_to_quoted(&1, columns: true)
      assert string_to_quoted.("1 + 2") == {:ok, {:+, [line: 1, column: 3], [1, 2]}}

      foo = {:foo, [line: 1, column: 1], nil}
      bar = {:bar, [line: 1, column: 7], nil}
      assert string_to_quoted.("foo + bar") == {:ok, {:+, [line: 1, column: 5], [foo, bar]}}
    end

    test "returns an error tuple on hex errors" do
      assert Code.string_to_quoted(~S["\x"]) ==
               {:error, {1, "missing hex sequence after \\x, expected \\xHH", "\""}}

      assert Code.string_to_quoted(~S[:"\x"]) ==
               {:error, {1, "missing hex sequence after \\x, expected \\xHH", ":\""}}

      assert Code.string_to_quoted(~S["\x": 123]) ==
               {:error, {1, "missing hex sequence after \\x, expected \\xHH", "\""}}

      assert Code.string_to_quoted(~s["""\n\\x\n"""]) ==
               {:error, {1, "missing hex sequence after \\x, expected \\xHH", "\"\"\""}}
    end

    test "returns an error tuple on interpolation in calls" do
      msg =
        "interpolation is not allowed when calling function/macro. Found interpolation in a call starting with: "

      assert Code.string_to_quoted(".\"\#{}\"") == {:error, {1, msg, "\""}}
      assert Code.string_to_quoted(".\"a\#{:b}\"c") == {:error, {1, msg, "\""}}
    end

    test "returns an error tuple on long atoms" do
      atom =
        "@GR{+z]`_XrNla!d<GTZ]iw[s'l2N<5hGD0(.xh&}>0ptDp(amr.oS&<q(FA)5T3=},^{=JnwIOE*DPOslKV KF-kb7NF&Y#Lp3D7l/!s],^hnz1iB |E8~Y'-Rp&*E(O}|zoB#xsE.S/~~'=%H'2HOZu0PCfz6j=eHq5:yk{7&|}zeRONM+KWBCAUKWFw(tv9vkHTu#Ek$&]Q:~>,UbT}v$L|rHHXGV{;W!>avHbD[T-G5xrzR6m?rQPot-37B@"

      assert Code.string_to_quoted(~s[:"#{atom}"]) ==
               {:error, {1, "atom length must be less than system limit: ", atom}}
    end

    test "returns an error tuple when no atom is found with :existing_atoms_only" do
      assert Code.string_to_quoted(":there_is_no_such_atom", existing_atoms_only: true) ==
               {:error, {1, "unsafe atom does not exist: ", "there_is_no_such_atom"}}
    end

    test "supports static_atoms_encoder" do
      ref = make_ref()

      encoder = fn atom, meta ->
        assert atom == "there_is_no_such_atom"
        assert meta[:line] == 1
        assert meta[:column] == 1
        assert meta[:file] == "nofile"
        {:ok, {:my, "atom", ref}}
      end

      assert {:ok, {:my, "atom", ^ref}} =
               Code.string_to_quoted(":there_is_no_such_atom", static_atoms_encoder: encoder)
    end

    test "static_atoms_encoder, error case" do
      encoder = fn _atom, _meta ->
        {:error, "Invalid atom name"}
      end

      assert {:error, {1, "Invalid atom name: ", "there_is_no_such_atom"}} =
               Code.string_to_quoted(":there_is_no_such_atom", static_atoms_encoder: encoder)
    end

    test "returns an error tuple on long atoms, even when using static_atoms_encoder" do
      atom = String.duplicate("a", 256)

      encoder = fn atom, _meta -> {:ok, atom} end

      assert Code.string_to_quoted(atom, static_atoms_encoder: encoder) ==
               {:error, {1, "atom length must be less than system limit: ", atom}}
    end

    test "extended static_atoms_encoder" do
      encoder = fn string, _metadata ->
        try do
          {:ok, String.to_existing_atom(string)}
        rescue
          ArgumentError ->
            {:ok, {:user_atom, string}}
        end
      end

      assert {:ok, {:try, _, [[do: {:test, _, [{{:user_atom, "atom_does_not_exist"}, _, []}]}]]}} =
               Code.string_to_quoted("try do: test(atom_does_not_exist())",
                 static_atoms_encoder: encoder
               )
    end

    test "raises on errors when string_to_quoted!/2 is used" do
      assert Code.string_to_quoted!("1 + 2") == {:+, [line: 1], [1, 2]}

      assert_raise SyntaxError, fn ->
        Code.string_to_quoted!("a.1")
      end

      assert_raise TokenMissingError, fn ->
        Code.string_to_quoted!("1 +")
      end
    end
  end

  describe "string_to_quoted/2 with :token_metadata" do
    test "adds end_of_expression information to blocks" do
      file = """
      one();two()
      three()

      four()


      five()
      """

      args = [
        {:one,
         [
           end_of_expression: [newlines: 0, line: 1, column: 6],
           closing: [line: 1, column: 5],
           line: 1,
           column: 1
         ], []},
        {:two,
         [
           end_of_expression: [newlines: 1, line: 1, column: 12],
           closing: [line: 1, column: 11],
           line: 1,
           column: 7
         ], []},
        {:three,
         [
           end_of_expression: [newlines: 2, line: 2, column: 8],
           closing: [line: 2, column: 7],
           line: 2,
           column: 1
         ], []},
        {:four,
         [
           end_of_expression: [newlines: 3, line: 4, column: 7],
           closing: [line: 4, column: 6],
           line: 4,
           column: 1
         ], []},
        {:five, [closing: [line: 7, column: 6], line: 7, column: 1], []}
      ]

      assert Code.string_to_quoted!(file, token_metadata: true, columns: true) ==
               {:__block__, [], args}
    end

    test "adds pairing information" do
      string_to_quoted = &Code.string_to_quoted!(&1, token_metadata: true)

      assert string_to_quoted.("foo") == {:foo, [line: 1], nil}
      assert string_to_quoted.("foo()") == {:foo, [closing: [line: 1], line: 1], []}

      assert string_to_quoted.("foo(\n)") ==
               {:foo, [newlines: 1, closing: [line: 2], line: 1], []}

      assert string_to_quoted.("%{\n}") == {:%{}, [newlines: 1, closing: [line: 2], line: 1], []}

      assert string_to_quoted.("foo(\n) do\nend") ==
               {:foo, [do: [line: 2], end: [line: 3], newlines: 1, closing: [line: 2], line: 1],
                [[do: {:__block__, [], []}]]}
    end

    test "adds delimiter information to sigils" do
      string_to_quoted = &Code.string_to_quoted!(&1, token_metadata: true)

      assert string_to_quoted.("~r/foo/") ==
               {:sigil_r, [delimiter: "/", line: 1], [{:<<>>, [line: 1], ["foo"]}, []]}

      assert string_to_quoted.("~r[foo]") ==
               {:sigil_r, [delimiter: "[", line: 1], [{:<<>>, [line: 1], ["foo"]}, []]}

      assert string_to_quoted.("~r\"foo\"") ==
               {:sigil_r, [delimiter: "\"", line: 1], [{:<<>>, [line: 1], ["foo"]}, []]}

      meta = [delimiter: "\"\"\"", line: 1]
      args = {:sigil_S, meta, [{:<<>>, [line: 1], ["sigil heredoc\n"]}, []]}
      assert string_to_quoted.("~S\"\"\"\nsigil heredoc\n\"\"\"") == args

      meta = [delimiter: "'''", line: 1]
      args = {:sigil_S, meta, [{:<<>>, [line: 1], ["sigil heredoc\n"]}, []]}
      assert string_to_quoted.("~S'''\nsigil heredoc\n'''") == args
    end
  end

  describe "string_to_quoted/2 with :literal_encoder" do
    test "wraps literals in blocks" do
      opts = [literal_encoder: &{:ok, {:__block__, &2, [&1]}}, token_metadata: true]
      string_to_quoted = &Code.string_to_quoted!(&1, opts)

      assert string_to_quoted.(~s("one")) == {:__block__, [delimiter: "\"", line: 1], ["one"]}
      assert string_to_quoted.("'one'") == {:__block__, [delimiter: "'", line: 1], ['one']}
      assert string_to_quoted.("?é") == {:__block__, [token: "?é", line: 1], [233]}
      assert string_to_quoted.("0b10") == {:__block__, [token: "0b10", line: 1], [2]}
      assert string_to_quoted.("12") == {:__block__, [token: "12", line: 1], [12]}
      assert string_to_quoted.("0o123") == {:__block__, [token: "0o123", line: 1], [83]}
      assert string_to_quoted.("0xEF") == {:__block__, [token: "0xEF", line: 1], [239]}
      assert string_to_quoted.("12.3") == {:__block__, [token: "12.3", line: 1], [12.3]}
      assert string_to_quoted.("nil") == {:__block__, [line: 1], [nil]}
      assert string_to_quoted.(":one") == {:__block__, [delimiter: ":", line: 1], [:one]}

      args = [[{:__block__, [token: "1", line: 1], [1]}]]

      assert string_to_quoted.("[1]") ==
               {:__block__, [closing: [line: 1], line: 1], args}

      args = [
        {{:__block__, [delimiter: ":", line: 1], [:ok]},
         {:__block__, [delimiter: ":", line: 1], [:test]}}
      ]

      assert string_to_quoted.("{:ok, :test}") ==
               {:__block__, [closing: [line: 1], line: 1], args}

      assert string_to_quoted.(~s("""\nhello\n""")) ==
               {:__block__, [delimiter: ~s["""], line: 1], ["hello\n"]}

      assert string_to_quoted.("'''\nhello\n'''") ==
               {:__block__, [delimiter: ~s['''], line: 1], ['hello\n']}

      args = [
        {:->, [line: 1],
         [
           [{:__block__, [token: "1", line: 1, closing: [line: 1], line: 1], [1]}],
           {:__block__, [delimiter: "\"", line: 1], ["hello"]}
         ]}
      ]

      assert string_to_quoted.(~s[fn (1) -> "hello" end]) ==
               {:fn, [closing: [line: 1], line: 1], args}
    end

    test "raises on bad literal" do
      assert_raise SyntaxError, "nofile:1: oops: literal", fn ->
        Code.string_to_quoted!(":one", literal_encoder: fn _, _ -> {:error, "oops"} end)
      end
    end
  end

  test "compile source" do
    assert __MODULE__.__info__(:compile)[:source] == String.to_charlist(__ENV__.file)
  end

  test "compile info returned with source accessible through keyword module" do
    compile = __MODULE__.__info__(:compile)
    assert Keyword.get(compile, :source) != nil
  end

  describe "compile_string/1" do
    test "compiles the given string" do
      assert [{CompileStringSample, _}] =
               Code.compile_string("defmodule CompileStringSample, do: :ok")
    after
      :code.purge(CompileSimpleSample)
      :code.delete(CompileSimpleSample)
    end

    test "works across lexical scopes" do
      assert [{CompileCrossSample, _}] =
               Code.compile_string("CodeTest.genmodule CompileCrossSample")
    after
      :code.purge(CompileCrossSample)
      :code.delete(CompileCrossSample)
    end
  end

  test "ensure_loaded?/1" do
    assert Code.ensure_loaded?(__MODULE__)
    refute Code.ensure_loaded?(Code.NoFile)
  end

  test "ensure_compiled?/1" do
    assert Code.ensure_compiled?(__MODULE__)
    refute Code.ensure_compiled?(Code.NoFile)
  end

  test "compiler_options/1 validates options" do
    message = "unknown compiler option: :not_a_valid_option"

    assert_raise RuntimeError, message, fn ->
      Code.compiler_options(not_a_valid_option: :foo)
    end

    message = "compiler option :debug_info should be a boolean, got: :not_a_boolean"

    assert_raise RuntimeError, message, fn ->
      Code.compiler_options(debug_info: :not_a_boolean)
    end
  end
end

defmodule Code.SyncTest do
  use ExUnit.Case

  test "path manipulation" do
    path = Path.join(__DIR__, "fixtures")
    Code.prepend_path(path)
    assert to_charlist(path) in :code.get_path()

    Code.delete_path(path)
    refute to_charlist(path) in :code.get_path()
  end

  test "purges compiler modules" do
    quoted = quote(do: Agent.start_link(fn -> :ok end))
    Code.compile_quoted(quoted)

    {:ok, claimed} = Code.purge_compiler_modules()
    assert claimed > 0

    {:ok, claimed} = Code.purge_compiler_modules()
    assert claimed == 0
  end
end
