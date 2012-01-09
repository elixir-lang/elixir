Code.require_file "../../test_helper", __FILE__

defmodule ExUnit::EscaperTest do
  use ExUnit::Case

  def test_escape do
    escaped    = ExUnit::Escaper.escape({ :{}, 1, [2, { :_, 2, false }] })
    { res, _ } = Code.eval_quoted escaped, [], __FILE__, __LINE__
    "{2, _}"   = inspect res
  end
end