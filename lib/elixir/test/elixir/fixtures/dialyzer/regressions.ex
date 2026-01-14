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

  def eex_eval_opts do
    EEx.eval_string("foo <%= bar %>", [bar: "baz"], trim: true)
  end

  @spec inlined_map_set :: MapSet.t(integer())
  def inlined_map_set, do: MapSet.new([1, 2])

  @spec inlined_uri :: URI.t()
  def inlined_uri, do: URI.new!("example.com")
end
