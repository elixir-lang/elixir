defmodule ExUnit.PatternFormat do
  @moduledoc "Formats the output of PatternDiff"

  alias ExUnit.{ContainerDiff, PatternDiff, WhenDiff}

  def format(%PatternDiff{type: :value, diff_result: :eq} = diff) do
    [eq: inspect(diff.rh)]
  end

  def format(%PatternDiff{type: :value, diff_result: :neq} = diff) do
    [del: inspect(diff.lh.val), ins: inspect(diff.rh)]
  end

  def format(%PatternDiff{} = unhandled) do
    IO.inspect(unhandled, label: "pattern_diff_unhandled")
  end

end
