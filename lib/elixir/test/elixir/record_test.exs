Code.require_file "test_helper.exs", __DIR__

defmodule RecordTest do
  use ExUnit.Case, async: true

  require Record

  test "extract/2 extracts information from an Erlang file" do
    assert Record.extract(:file_info, from_lib: "kernel/include/file.hrl") ==
           [size: :undefined, type: :undefined, access: :undefined, atime: :undefined,
            mtime: :undefined, ctime: :undefined, mode: :undefined, links: :undefined,
            major_device: :undefined, minor_device: :undefined, inode: :undefined,
            uid: :undefined, gid: :undefined]
  end

  test "extract/2 handles nested records too" do
    namespace = Record.extract(:xmlElement, from_lib: "xmerl/include/xmerl.hrl")[:namespace]
    assert is_tuple(namespace)
    assert elem(namespace, 0) == :xmlNamespace
  end

  # We need indirection to avoid warnings
  defp record?(data, kind) do
    Record.record?(data, kind)
  end

  test "record?/2" do
    assert record?({ User, "jose", 27 }, User)
    refute record?({ User, "jose", 27 }, Author)
    refute record?(13, Author)
  end

  # We need indirection to avoid warnings
  defp record?(data) do
    Record.record?(data)
  end

  test "record?/1" do
    assert record?({ User, "jose", 27 })
    refute record?({ "jose", 27 })
    refute record?(13)
  end
end
