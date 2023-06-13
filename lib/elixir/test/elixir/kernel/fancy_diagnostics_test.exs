Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.FancyDiagnosticsTest do
  use ExUnit.Case, async: false

  setup do 
    Code.put_compiler_option(:fancy_diagnostics, true)
    on_exit(fn -> Code.put_compiler_option(:fancy_diagnostics, false) end)
  end

  describe "compile-time exceptions" do 
    test "SyntaxError (snippet)" do 
      assert false
    end

    test "SyntaxError (line only)" do 
      assert false
    end

    test "SyntaxError (line+column)" do 
      assert false
    end

    test "MissingTokenError (snippet)" do 
      assert false
    end

    test "MissingTokenError (line only)" do 
      assert false
    end

    test "MissingTokenError (line+column)" do 
      assert false
    end

    test "respects disabled ansi" do 
      assert false
    end
  end

  describe "diagnostics" do 
    test "grouped warnings" do 
      assert false
    end

    test "warning (line only)" do 
      assert false
    end

    test "warning (line+column)" do 
      assert false
    end

    test "warning (long message)" do 
      assert false
    end

    test "error (line only)" do 
      assert false
    end

    test "error (line+column)" do 
      assert false
    end

    test "error (long message)" do 
      assert false
    end

    test "respects disabled ansi" do 
      assert false
    end
  end
end
