Code.require_file "../../test_helper", __FILE__

#defrecord Kernel.ReadMacroTest.Helper, ignore: [], html: [] do
defmodule Kernel.ReadMacroTest.Helper do
  defmacro __using__(_) do
    target = __CALLER__.module
    Module.register_attribute target, :__ignore__
    Module.register_attribute target, :__html__
    quote do
      import Kernel.ReadMacroTest.Helper
    end
  end

  defmacro __ignore__({ :<<>>, line, body }, []) do
    Module.add_attribute __CALLER__.module, :__ignore__, {line, body}
  end

  defmacro __html__({ :<<>>, line, body }, []) do
    Module.add_attribute __CALLER__.module, :__html__, {line, body}
  end
end

defmodule Kernel.ReadMacroTest do
  use ExUnit.Case, async: true

  test :with_read_macro do
    path = File.expand_path("../../fixtures/with_read_macro.ex", __FILE__)

    try do
      Code.load_file path

      attributes = Keyword.from_enum(WithReadMacro.__info__(:attributes))
      expected_ignore = {11, "  def bar(var // 0)\n  def bar(_), do: 2\n"}
      expected_html = {16, "<html>\n     <head>\n          <title>\n               Example\n          </title>\n     </head>\n     <body>\n          <h1>\n               Hi!\n          </h1>\n     </body>\n</html>\n"}
      assert [expected_ignore] == attributes[:__ignore__]
      assert [expected_html] == attributes[:__html__]
    after
      :code.purge WithReadMacro
      :code.delete WithReadMacro
    end
  end


end
