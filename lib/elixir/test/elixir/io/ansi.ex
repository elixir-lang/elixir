Code.require_file "../../test_helper.exs", __FILE__

defmodule IO.ANSITest do
  use ExUnit.Case, async: true

  test :boolean do
    assert String.length(IO.ANSI.red(true)) > 0
    assert IO.ANSI.red(false) == ""
  end

end
