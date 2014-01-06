Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.SCM.GitTest do
  use MixTest.Case

  test "formats the lock" do
    assert Mix.SCM.Git.format_lock(lock()) == "abcdef0"
    assert Mix.SCM.Git.format_lock(lock(branch: "master")) == "abcdef0 (branch: master)"
    assert Mix.SCM.Git.format_lock(lock(tag: "v0.12.0"))   == "abcdef0 (tag: v0.12.0)"
    assert Mix.SCM.Git.format_lock(lock(ref: "abcdef0"))   == "abcdef0 (ref)"
  end

  defp lock(opts // []) do
    [lock: { :git, "/repo", "abcdef0123456789", opts }]
  end
end
