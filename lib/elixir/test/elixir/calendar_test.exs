Code.require_file "test_helper.exs", __DIR__

defmodule DateTest do
  use ExUnit.Case, async: true
  doctest Date
end

defmodule TimeTest do
  use ExUnit.Case, async: true
  doctest Time
end

defmodule NaiveDateTimeTest do
  use ExUnit.Case, async: true
  doctest NaiveDateTime
end
