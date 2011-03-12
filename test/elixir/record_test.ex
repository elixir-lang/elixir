Code.require File.expand_path("../test_helper", __FILE__)
Code.require "record"

object RecordTest
  proto ExUnit::Case

  object FileInfo
    proto Record
    record 'file_info, 'from_lib: "kernel/include/file.hrl"
  end

  object ElixirScope
    proto Record
    record 'elixir_scope, 'from: File.expand_path("../../../include/elixir.hrl", __FILE__)
  end
  
  def record_accessors_test
    record = RecordTest::FileInfo.new(file_info)
    'regular    = record.type
    'read_write = record.access

    new_record = record.access 'read
    'read = new_record.access
  end

  def record_update_test
    record = RecordTest::FileInfo.new(file_info).update('type: 'device, 'access: 'read)
    'device = record.type
    'read   = record.access
  end

  def record_defaults_test
    record = RecordTest::ElixirScope.new
    {[],[]} = record.scope
  end

  def record_reflection_test
    record = RecordTest::ElixirScope.new
    'elixir_scope = record.record_name
    ['assign,'method,'scope,'vars,'counter,'filename] = record.record_keys
    [false,[],{[], []},[],0,$"nofile"] = record.record_defaults
    6 = record.record_size
  end

  private
  
  def file_info
    { 'ok, file_info } = Erlang.file.read_file_info(__FILE__.to_bin)
    file_info
  end
end