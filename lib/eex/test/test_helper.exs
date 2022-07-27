{line_exclude, line_include} =
  if line = System.get_env("LINE"), do: {[:test], [line: line]}, else: {[], []}

ExUnit.start(
  trace: !!System.get_env("TRACE"),
  include: line_include,
  exclude: line_exclude
)
