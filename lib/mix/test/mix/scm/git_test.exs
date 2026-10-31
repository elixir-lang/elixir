# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

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
             "https://github.com/elixir-lang/some_dep.git - b"

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

  test "raises about conflicting Git refspec options" do
    assert_raise Mix.Error, ~r/You should specify only one of branch, ref or tag/, fn ->
      Mix.SCM.Git.accepts_options(nil, git: "/repo", branch: "main", tag: "0.1.0")
    end

    assert_raise Mix.Error, ~r/You should specify only one of branch, ref or tag/, fn ->
      Mix.SCM.Git.accepts_options(nil, git: "/repo", branch: "main", branch: "develop")
    end
  end

  test "raises about non-binary Git refspec options" do
    assert_raise Mix.Error, ~r/A dependency's branch must be a string/, fn ->
      Mix.SCM.Git.accepts_options(nil, git: "/repo", branch: :main)
    end

    assert_raise Mix.Error, ~r/A dependency's tag must be a string/, fn ->
      Mix.SCM.Git.accepts_options(nil, git: "/repo", tag: :stable)
    end

    assert_raise Mix.Error, ~r/A dependency's ref must be a string/, fn ->
      Mix.SCM.Git.accepts_options(nil, git: "/repo", ref: :abcdef0123456789)
    end
  end

  test "raises about Git refspec options starting with dash" do
    assert_raise Mix.Error, ~r/A dependency's branch must not start with -/, fn ->
      Mix.SCM.Git.accepts_options(nil, git: "/repo", branch: "-main")
    end

    assert_raise Mix.Error, ~r/A dependency's tag must not start with -/, fn ->
      Mix.SCM.Git.accepts_options(nil, git: "/repo", tag: "--upload-pack=echo")
    end

    assert_raise Mix.Error, ~r/A dependency's ref must not start with -/, fn ->
      Mix.SCM.Git.accepts_options(nil, git: "/repo", ref: "-abcdef0123456789")
    end
  end

  describe "lock_status/1" do
    @rev String.duplicate("a1", 20)
    @url "https://example.com/elixir-lang/some_dep.git"

    test "reads a detached HEAD and the origin url from the checkout" do
      checkout = git_checkout("plain", "[remote \"origin\"]\n\turl = #{@url}\n", @rev)

      assert lock_status(checkout, @url, @rev) == :ok

      assert lock_status(checkout, "other", @rev) ==
               :mismatch

      other_rev = String.duplicate("b2", 20)

      assert lock_status(checkout, @url, other_rev) ==
               :mismatch
    end

    test "agrees with git on unusual configs" do
      configs = [
        quoted: "[remote \"origin\"]\n\turl = \"#{@url}\"\n",
        comment: "[remote \"origin\"]\n\turl = #{@url} # comment\n",
        several: "[remote \"origin\"]\n\turl = other\n\turl = #{@url}\n",
        deprecated_syntax: "[remote.origin]\n\turl = #{@url}\n",
        key_on_header: "[remote \"origin\"] url = #{@url}\n",
        crlf: "[remote \"origin\"]\r\n\turl = #{@url}\r\n",
        continuation: "[remote \"origin\"]\n\tpushurl = x \\\n\turl = other\n\turl = #{@url}\n",
        bare_key: "[remote \"origin\"]\n\turl = other\n\turl\n",
        include: "[include]\n\tpath = origin.config\n"
      ]

      for {name, config} <- configs do
        checkout = git_checkout(name, config, @rev)

        File.write!(
          Path.join(checkout, ".git/origin.config"),
          "[remote \"origin\"]\nurl = #{@url}\n"
        )

        {origin, 0} =
          System.cmd("git", ["--git-dir=.git", "config", "remote.origin.url"], cd: checkout)

        assert lock_status(checkout, String.trim(origin), @rev) == :ok, "#{name}"
      end
    end

    test "falls back to git for a symbolic HEAD" do
      checkout = git_checkout("symbolic", "[remote \"origin\"]\n\turl = #{@url}\n", nil)
      git = &System.cmd("git", &1, cd: checkout, stderr_to_stdout: true)
      {_, 0} = git.(~w[-c user.name=mix -c user.email=mix@example.com commit --allow-empty -qm x])
      {rev, 0} = git.(~w[rev-parse HEAD])
      assert "ref: " <> _ = File.read!(Path.join(checkout, ".git/HEAD"))

      assert lock_status(checkout, @url, String.trim(rev)) == :ok
    end

    defp lock_status(checkout, url, rev) do
      Mix.SCM.Git.lock_status(checkout: checkout, git: url, lock: {:git, url, rev, []})
    end

    defp git_checkout(name, config, head) do
      checkout = tmp_path("git_lock_status/#{name}")
      File.rm_rf!(checkout)
      File.mkdir_p!(checkout)
      {_, 0} = System.cmd("git", ~w[init --quiet], cd: checkout)
      File.write!(Path.join(checkout, ".git/config"), config, [:append])
      if head, do: File.write!(Path.join(checkout, ".git/HEAD"), head <> "\n")
      checkout
    end
  end

  defp lock(opts \\ []) do
    [lock: {:git, "/repo", "abcdef0123456789", opts}]
  end
end
