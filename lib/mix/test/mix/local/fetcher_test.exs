Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.Local.FetcherTest do
  use MixTest.Case

  test "fetch" do
    dep_spec = {:"git repo", git: fixture_path("git_repo")}

    config =
      Mix.Local.Fetcher.fetch "git repo", dep_spec, fn _mixfile ->
        assert Mix.env() == :prod
        Mix.Project.config()
      end

    assert Mix.env() == :dev

    assert config[:app] == :git_repo
    assert config[:deps_path] =~ ~r/mix-local-builder-.*\/deps/
    assert config[:lockfile] =~ ~r/mix-local-builder-.*\/mix.lock/
  end
end
