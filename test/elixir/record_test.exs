Code.require_file "../test_helper", __FILE__

defrecord RecordTest::FileInfo,
  Record.extract(:file_info, from_lib: "kernel/include/file.hrl")

name = RecordTest::DynamicName
defrecord name, a: 0, b: 1

defmodule RecordTest do
  use ExUnit::Case

  def test_record_constructor_with_dict do
    record   = RecordTest::FileInfo.new(type: :regular)
    assert_equal :regular, record.type
    assert_equal nil, record.access
  end

  def test_record_accessors do
    record = RecordTest::FileInfo.new(file_info)
    assert_equal :regular, record.type
    assert_equal :read_write, record.access

    new_record = record.access :read
    assert_equal :read, new_record.access
  end

  def test_dynamic_record_name do
    record = RecordTest::DynamicName.new
    assert_equal 0, record.a
    assert_equal 1, record.b
  end

  defp file_info do
    { :ok, file_info } = Erlang.file.read_file_info(__FILE__)
    file_info
  end
end
