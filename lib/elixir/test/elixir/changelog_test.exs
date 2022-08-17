root = Path.expand("../../../..", __DIR__)

{:module, mod, bytecode, _} =
  defmodule Changelog do
    @moduledoc File.read!("#{root}/CHANGELOG.md")
  end

dir = "#{root}/tmp/changelog_test"
File.mkdir_p!(dir)
File.write!("#{dir}/#{mod}.beam", bytecode)
Code.prepend_path(dir)

defmodule ChangelogTest do
  use ExUnit.Case, async: true
  doctest Changelog
end
