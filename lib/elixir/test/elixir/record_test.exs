Code.require_file "test_helper.exs", __DIR__

defmodule RecordTest do
  use ExUnit.Case, async: true

  doctest Record

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

  test "extract/2 with defstruct" do
    defmodule StructExtract do
      defstruct Record.extract(:file_info, from_lib: "kernel/include/file.hrl")
    end

    assert %{__struct__: StructExtract, size: :undefined} =
           StructExtract.__struct__
  end

  test "extract_all/1 extracts all records information from an Erlang file" do
    all_extract = Record.extract_all(from_lib: "kernel/include/file.hrl")
    assert length(all_extract) == 2 # has been stable over the very long time
    assert all_extract[:file_info]
    assert all_extract[:file_descriptor]
  end

  # We need indirection to avoid warnings
  defp record?(data, kind) do
    Record.is_record(data, kind)
  end

  test "is_record/2" do
    assert record?({User, "meg", 27}, User)
    refute record?({User, "meg", 27}, Author)
    refute record?(13, Author)
  end

  # We need indirection to avoid warnings
  defp record?(data) do
    Record.is_record(data)
  end

  test "is_record/1" do
    assert record?({User, "john", 27})
    refute record?({"john", 27})
    refute record?(13)
  end

  Record.defrecord  :timestamp, [:date, :time]
  Record.defrecord  :user, __MODULE__, name: "john", age: 25
  Record.defrecordp :file_info,
    Record.extract(:file_info, from_lib: "kernel/include/file.hrl")
  Record.defrecordp :certificate, :OTPCertificate,
    Record.extract(:OTPCertificate, from_lib: "public_key/include/public_key.hrl")

  test "records generates macros that generates tuples" do
    record = user()
    assert user(record, :name) == "john"
    assert user(record, :age)  == 25

    record = user(record, name: "meg")
    assert user(record, :name) == "meg"

    assert elem(record, user(:name)) == "meg"
    assert elem(record, 0) == RecordTest

    user(name: name) = record
    assert name == "meg"

    assert user(:name) == 1
  end

  test "records with no tag" do
    assert elem(file_info(), 0) == :file_info
  end

  test "records with dynamic arguments" do
    record = file_info()
    assert file_info(record, :size) == :undefined

    record = user()
    assert user(record) == [name: "john", age: 25]
    assert user(user()) == [name: "john", age: 25]

    msg = "expected argument to be a literal atom, literal keyword or a :file_info record, " <>
          "got runtime: {RecordTest, \"john\", 25}"
    assert_raise ArgumentError, msg, fn ->
      file_info(record)
    end
  end

  test "records visibility" do
    assert macro_exported?(__MODULE__, :user, 0)
    refute macro_exported?(__MODULE__, :file_info, 1)
  end

  test "records with no defaults" do
    record = timestamp()
    assert timestamp(record, :date) == nil
    assert timestamp(record, :time) == nil

    record = timestamp(date: :foo, time: :bar)
    assert timestamp(record, :date) == :foo
    assert timestamp(record, :time) == :bar
  end
end
