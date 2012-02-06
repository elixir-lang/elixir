Code.require_file "../../test_helper", __FILE__

defmodule ExUnit::EscaperTest do
  use ExUnit::Case

  def test_escape do
    escaped  = ExUnit::Escaper.escape(%{ :%{}, 1, [2, %{ :_, 2, false }] })
    "%{2, _}" = inspect elem Code.eval_quoted(escaped, [], __FILE__, __LINE__), 1
  end
end