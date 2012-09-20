Code.require_file "../test_helper.exs", __FILE__

defrecord RecordTest.FileInfo,
  Record.extract(:file_info, from_lib: "kernel/include/file.hrl")

name = RecordTest.DynamicName
defrecord name, a: 0, b: 1 do
  def get_a(RecordTest.DynamicName[a: a]) do
    a
  end
end

defrecord RecordTest.WithNoField, []

defmodule RecordTest.Macros do
  Record.defmacros __ENV__, :_user, [:name, :age]

  def new(name, age) do
    _user(name: name, age: age)
  end

  def name(_user(name: name)) do
    name
  end

  def add_bar_to_name(_user(name: name) = user) do
    _user(user, name: name <> " bar")
  end

  def age(user) do
    _user(user, :age)
  end

  def to_keywords(user) do
    _user(user)
  end

  def name_and_age(user) do
    _user(user, [:name, :age])
  end

  def age_and_name(user) do
    _user(user, [:age, :name])
  end
end

defmodule RecordTest do
  use ExUnit.Case, async: true

  test :record_access_with_nil_keyword do
    record = RecordTest.DynamicName.new(a: nil)
    record_access = RecordTest.DynamicName[a: nil]
    assert record == record_access
  end

  test :record_constructor_with_dict do
    record   = RecordTest.FileInfo.new(type: :regular)
    assert record.type == :regular
    assert record.access == nil
  end

  test :record_accessors do
    record = RecordTest.FileInfo.new(file_info)
    assert record.type == :regular
    assert record.access == :read_write

    new_record = record.access :read
    assert new_record.access == :read
  end

  test :dynamic_record_name do
    record = RecordTest.DynamicName.new
    assert record.a == 0
    assert record.b == 1
  end

  test :dynamic_update do
    record = RecordTest.DynamicName.new
    assert record.update_a(10 + &1).a == 10
  end

  test :is_record do
    assert is_record(RecordTest.FileInfo.new, RecordTest.FileInfo)
    refute is_record(a_list, RecordTest.FileInfo)
    refute is_record(RecordTest.FileInfo.new, List)
  end

  test :__index__ do
    record = RecordTest.DynamicName.new(a: "a", b: "b")
    assert elem(record, record.__index__(:a)) == "a"
    assert elem(record, record.__index__(:b)) == "b"
    assert record.__index__(:c) == nil
    record = RecordTest.FileInfo.new
    assert RecordTest.FileInfo.__index__(:atime) == record.__index__(:atime)
  end

  test :to_keywords do
    record = RecordTest.DynamicName.new(a: "a", b: "b")
    assert record.to_keywords[:a] == "a"
    assert record.to_keywords[:b] == "b"
  end

  test :underscore_record_syntax do
    record = RecordTest.DynamicName[_: "a"]
    assert RecordTest.DynamicName[a: "a", b: "a"] == record
    assert RecordTest.DynamicName[_: _] = RecordTest.DynamicName[_: "x"]
    assert { :badmatch, RecordTest.DynamicName[a: "y", b: "y"] } =
      catch_error(RecordTest.DynamicName[_: "x"] = RecordTest.DynamicName[_: "y"])
  end

  test :access_protocol_on_being_defined_record do
    assert RecordTest.DynamicName.new(a: "a").get_a == "a"
  end

  test :record_macros do
    record = RecordTest.Macros.new("Foo", 25)
    assert record.name == "Foo"

    record = record.add_bar_to_name
    assert record.name == "Foo bar"

    assert record.age == 25
    assert record.to_keywords == [name: record.name, age: record.age]

    assert record.name_and_age == [record.name, record.age]
    assert record.age_and_name == [record.age, record.name]
  end

  defp file_info do
    { :ok, file_info } = Erlang.file.read_file_info(__FILE__)
    file_info
  end

  defp a_list do
    [:a, :b, :c]
  end
end
