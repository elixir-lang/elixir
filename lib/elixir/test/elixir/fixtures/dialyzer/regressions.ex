defmodule Dialyzer.Regressions do
  def io_inspect_opts do
    IO.inspect(123, label: "foo", limit: :infinity)
  end

  def format_opts do
    Code.format_string!("",
      line_length: 120,
      force_do_end_blocks: true,
      locals_without_parens: true,
      migrate: true
    )
  end
end
