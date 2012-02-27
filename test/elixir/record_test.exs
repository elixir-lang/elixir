Code.require_file "../test_helper", __FILE__

defrecord RecordTest::FileInfo,
  Record.extract(:file_info, from_lib: "kernel/include/file.hrl")

name = RecordTest::DynamicName
defrecord name, a: 0, b: 1

defmodule RecordTest do
  use ExUnit::Case

  test :record_constructor_with_dict do
    record   = RecordTest::FileInfo.new(type: :regular)
    assert_equal :regular, record.type
    assert_equal nil, record.access
  end

  test :record_accessors do
    record = RecordTest::FileInfo.new(file_info)
    assert_equal :regular, record.type
    assert_equal :read_write, record.access

    new_record = record.access :read
    assert_equal :read, new_record.access
  end

  test :dynamic_record_name do
    record = RecordTest::DynamicName.new
    assert_equal 0, record.a
    assert_equal 1, record.b
  end

  test :dynamic_update do
    record = RecordTest::DynamicName.new
    assert_equal 10, record.update_a(10 + &1).a
  end

  test :is_record do
    assert is_record(FileInfo.new, FileInfo)
    refute is_record(a_list, FileInfo)
    refute is_record(FileInfo.new, List)
  end

  defp file_info do
    { :ok, file_info } = Erlang.file.read_file_info(__FILE__)
    file_info
  end

  defp a_list do
    [:a, :b, :c]
  end
end
