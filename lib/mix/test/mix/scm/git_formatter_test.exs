Code.require_file "../../test_helper.exs", __DIR__

defmodule Mix.SCM.Git.FormatterTest do
  use MixTest.Case
  alias Mix.SCM.Git.Formatter, as: F

  test "ref aggregation" do
    assert F.refs_to_string([]) == ""

    assert F.refs_to_string([
      "refs/heads/master",
    ]) == "(master)"

    assert F.refs_to_string([
      "refs/heads/master",
      "refs/remotes/origin/master",
    ]) == "(origin/master)"

    assert F.refs_to_string([
      "refs/heads/develop",
      "refs/remotes/origin/master",
    ]) == "(origin/master, develop)"

    assert F.refs_to_string([
      "refs/heads/master",
      "refs/remotes/origin/master",
      "refs/tags/1.0",
    ]) == "(tag: 1.0)"

    assert F.refs_to_string([
      "refs/heads/master",
      "refs/remotes/origin/master",
      "refs/tags/1.0",
      "refs/tags/2.0",
    ]) == "(tags: 2.0, 1.0)"
  end

  test "git show-ref output parsing" do
    output = """
    abcdefg refs/heads/develop
    ABCDEFG refs/remotes/origin/master
    1234567 refs/tags/1.0
    1234567 refs/tags/2.0
    """

    assert F.format_refs(output, "1234567") == "(tags: 2.0, 1.0)"
    assert F.format_refs(output, "abcdefg") == "(develop)"
  end
end
