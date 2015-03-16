Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.SCM.MercurialTest do
  use MixTest.Case, async: true

  test "formats the lock" do
    assert Mix.SCM.Mercurial.format_lock(lock()) == "abcdef0"
    assert Mix.SCM.Mercurial.format_lock(lock(branch: "default")) == "abcdef0 (branch: default)"
    assert Mix.SCM.Mercurial.format_lock(lock(tag: "v0.12.0"))   == "abcdef0 (tag: v0.12.0)"
    assert Mix.SCM.Mercurial.format_lock(lock(ref: "abcdef0"))   == "abcdef0 (ref)"
  end

  test "considers to dep equals if the have the same hg and the same opts" do
    assert Mix.SCM.Mercurial.equal?([hg: "foo"], [hg: "foo"])
    refute Mix.SCM.Mercurial.equal?([hg: "foo"], [hg: "bar"])

    assert Mix.SCM.Mercurial.equal?([hg: "foo", branch: "master"], [hg: "foo", branch: "master"])
    refute Mix.SCM.Mercurial.equal?([hg: "foo", branch: "master"], [hg: "foo", branch: "other"])
  end

  test "lock should not be taken into account when considering deps equal as the lock is shared" do
    assert Mix.SCM.Mercurial.equal?([hg: "foo", lock: 1], [hg: "foo", lock: 2])
  end

  defp lock(opts \\ []) do
    [lock: {:hg, "/repo", "abcdef0123456789", opts}]
  end
end
