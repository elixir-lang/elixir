ExUnit.start()

defmodule ChangelogTest do
  use ExUnit.Case, async: true
  import ExUnit.DocTest
  doctest_file(Path.expand("../../../../CHANGELOG.md", __DIR__))
end
