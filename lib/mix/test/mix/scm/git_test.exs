Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.SCM.GitTest do
  use MixTest.Case, async: true

  test "formats the lock" do
    assert Mix.SCM.Git.format_lock(lock()) == "abcdef0"
    assert Mix.SCM.Git.format_lock(lock(branch: "master")) == "abcdef0 (branch: master)"
    assert Mix.SCM.Git.format_lock(lock(tag: "v0.12.0"))   == "abcdef0 (tag: v0.12.0)"
    assert Mix.SCM.Git.format_lock(lock(ref: "abcdef0"))   == "abcdef0 (ref)"
  end

  test "considers two dep equals if the have the same git and the same opts" do
    assert Mix.SCM.Git.equal?([git: "foo"], [git: "foo"])
    refute Mix.SCM.Git.equal?([git: "foo"], [git: "bar"])

    assert Mix.SCM.Git.equal?([git: "foo", branch: "master"], [git: "foo", branch: "master"])
    refute Mix.SCM.Git.equal?([git: "foo", branch: "master"], [git: "foo", branch: "other"])
  end

  test "considers two GitHub dep equals if they are the same repo and opts match, just different protocols" do
    git_url = "git://github.com/elixir-lang/elixir.git"
    https_url = "https://github.com/elixir-lang/elixir.git"

    assert Mix.SCM.Git.equal?([git: git_url], [git: https_url])
    refute Mix.SCM.Git.equal?([git: "git@github.com:elixir-lang/ecto.git"], [git: https_url])

    assert Mix.SCM.Git.equal?([git: git_url, branch: "master"], [git: https_url, branch: "master"])
    refute Mix.SCM.Git.equal?([git: git_url, branch: "master"], [git: https_url, branch: "other"])
  end

  test "lock should not be taken into account when considering deps equal as the lock is shared" do
    assert Mix.SCM.Git.equal?([git: "foo", lock: 1], [git: "foo", lock: 2])
  end

  test "warns about conflicting git checkout options" do
    msg = "warning: you should specify only one of branch, ref or tag, and only once. " <>
          "Error on git dependency: /repo"

    Mix.SCM.Git.accepts_options(nil, [git: "/repo", branch: "master", tag: "0.1.0"])
    assert_received {:mix_shell, :error, [^msg]}

    Mix.SCM.Git.accepts_options(nil, [git: "/repo", branch: "master", branch: "develop"])
    assert_received {:mix_shell, :error, [^msg]}

    Mix.SCM.Git.accepts_options(nil, [git: "/repo", ref: "abcdef01234", branch: "develop"])
    assert_received {:mix_shell, :error, [^msg]}
  end

  defp lock(opts \\ []) do
    [lock: {:git, "/repo", "abcdef0123456789", opts}]
  end
end
