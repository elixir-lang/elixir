Code.require_file("../../test_helper.exs", __DIR__)

defmodule Mix.SCM.GitTest do
  use MixTest.Case, async: true

  test "formats the lock" do
    assert Mix.SCM.Git.format_lock(lock()) == "abcdef0"
    assert Mix.SCM.Git.format_lock(lock(branch: "main")) == "abcdef0 (branch: main)"
    assert Mix.SCM.Git.format_lock(lock(tag: "v0.12.0")) == "abcdef0 (tag: v0.12.0)"
    assert Mix.SCM.Git.format_lock(lock(ref: "abcdef0")) == "abcdef0 (ref)"
  end

  test "considers two dep equals if the have the same Git and the same opts" do
    assert Mix.SCM.Git.equal?([git: "foo"], git: "foo")
    refute Mix.SCM.Git.equal?([git: "foo"], git: "bar")

    assert Mix.SCM.Git.equal?([git: "foo", branch: "main"], git: "foo", branch: "main")
    refute Mix.SCM.Git.equal?([git: "foo", branch: "main"], git: "foo", branch: "other")
  end

  test "lock should not be taken into account when considering deps equal as the lock is shared" do
    assert Mix.SCM.Git.equal?([git: "foo", lock: 1], git: "foo", lock: 2)
  end

  test "get and update should display git checkout options along the url" do
    opts = [git: "https://github.com/elixir-lang/some_dep.git"]

    assert Mix.SCM.Git.format(opts) ==
             "https://github.com/elixir-lang/some_dep.git"

    assert Mix.SCM.Git.format(Keyword.put(opts, :tag, "v1")) ==
             "https://github.com/elixir-lang/some_dep.git - v1"

    assert Mix.SCM.Git.format(Keyword.put(opts, :branch, "b")) ==
             "https://github.com/elixir-lang/some_dep.git - origin/b"

    assert Mix.SCM.Git.format(Keyword.put(opts, :ref, "abcdef")) ==
             "https://github.com/elixir-lang/some_dep.git - abcdef"
  end

  test "redacts username password from urls" do
    url = "https://username:password@github.com/elixir-lang/some_dep.git"
    opts = [git: url]

    assert Mix.SCM.Git.format(opts) ==
             "https://****:****@github.com/elixir-lang/some_dep.git"

    assert_raise Mix.Error, ~r/[*]{4}:[*]{4}/, fn ->
      Mix.SCM.Git.accepts_options(nil, git: url, branch: "main", branch: "develop")
    end
  end

  test "raises about conflicting Git checkout options" do
    assert_raise Mix.Error, ~r/You should specify only one of branch, ref or tag/, fn ->
      Mix.SCM.Git.accepts_options(nil, git: "/repo", branch: "main", tag: "0.1.0")
    end

    assert_raise Mix.Error, ~r/You should specify only one of branch, ref or tag/, fn ->
      Mix.SCM.Git.accepts_options(nil, git: "/repo", branch: "main", branch: "develop")
    end
  end

  defp lock(opts \\ []) do
    [lock: {:git, "/repo", "abcdef0123456789", opts}]
  end
end
