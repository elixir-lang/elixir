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

  test "extract/2 with defstruct" do
    defmodule StructExtract do
      defstruct Record.extract(:file_info, from_lib: "kernel/include/file.hrl")
    end

    assert %{__struct__: StructExtract, size: :undefined} =
           StructExtract.__struct__
  end

  # We need indirection to avoid warnings
  defp record?(data, kind) do
    Record.record?(data, kind)
  end

  test "record?/2" do
    assert record?({User, "jose", 27}, User)
    refute record?({User, "jose", 27}, Author)
    refute record?(13, Author)
  end

  # We need indirection to avoid warnings
  defp record?(data) do
    Record.record?(data)
  end

  test "record?/1" do
    assert record?({User, "jose", 27})
    refute record?({"jose", 27})
    refute record?(13)
  end

  Record.defrecord  :timestamp, [:date, :time]
  Record.defrecord  :user, __MODULE__, name: "José", age: 25
  Record.defrecordp :file_info, Record.extract(:file_info, from_lib: "kernel/include/file.hrl")

  test "records generates macros that generates tuples" do
    record = user()
    assert user(record, :name) == "José"
    assert user(record, :age)  == 25

    record = user(record, name: "Eric")
    assert user(record, :name) == "Eric"

    assert elem(record, user(:name)) == "Eric"
    assert elem(record, 0) == RecordTest

    user(name: name) = record
    assert name == "Eric"
  end

  test "create with keywords list variable generates record tuple" do
    list = [name: "Eric", age: 23]
    record = user(list)
    assert user(record, :name) == "Eric"
    assert user(record, :age) == 23
  end

  test "create with keywords list variable with invalid key raises" do
    assert_raise ArgumentError, "record #{inspect __MODULE__} does not have the key: :foo", fn ->
      list = [name: "Eric", foo: 42]
      user(list)
    end
  end

  test "create with keywords list variable generates record tuple with defaults" do
    list = [name: "Eric"]
    record = user(list)
    assert user(record, :age) == 25
  end

  test "create with invalid keywords list variable raises" do
    assert_raise ArgumentError, "expected argument to be an atom or a keywords list, got: [1, 2, 3]", fn ->
      list = [1, 2, 3]
      user(list)
    end
  end

  test "update with keywords list variable updates record tuple" do
    record = user()
    list = [name: "Eric", age: 23]
    record = user(record, list)
    assert user(record, :name) == "Eric"
    assert user(record, :age) == 23
  end

  test "update with keywords list variable with invalid key raises" do
    assert_raise ArgumentError, "record #{inspect __MODULE__} does not have the key: :foo", fn ->
      record = user()
      list = [name: "Eric", foo: 42]
      user(record, list)
    end
  end

  test "update with invalid keywords list variable raises" do
    assert_raise ArgumentError, "expected argument to be an atom or a keywords list, got: [1, 2, 3]", fn ->
      record = user()
      list = [1, 2, 3]
      user(record, list)
    end
  end

  test "index with a variable returns the field index" do
    name_field = :name
    age_field = :age
    assert user(name_field) == 1
    assert user(age_field) == 2
  end

  test "index with a variable and invalid field key raises" do
    assert_raise ArgumentError, "record #{inspect __MODULE__} does not have the key: :foo", fn ->
      field = :foo
      user(field)
    end
  end

  test "index with a variable with invalid value raises" do
    assert_raise ArgumentError, "expected argument to be an atom or a keywords list, got: \"foo\"", fn ->
      field = "foo"
      user(field)
    end
  end

  test "get with a variable returns the field value" do
    name_field = :name
    age_field = :age
    record = user()
    assert user(record, name_field) == "José"
    assert user(record, age_field) == 25
  end

  test "get with a variable and invalid field key raises" do
    assert_raise ArgumentError, "record #{inspect __MODULE__} does not have the key: :foo", fn ->
      field = :foo
      record = user()
      user(record, field)
    end
  end

  test "get with a variable with invalid value raises" do
    assert_raise ArgumentError, "expected argument to be an atom or a keywords list, got: \"foo\"", fn ->
      field = "foo"
      record = user()
      user(record, field)
    end
  end

  test "records with no tag" do
    assert elem(file_info(), 0) == :file_info
  end

  test "records with dynamic arguments" do
    record = file_info()
    assert file_info(record, :size) == :undefined
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
