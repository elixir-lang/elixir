Code.require_file "../test_helper", __FILE__

module RecordTest
  mixin ExUnit::Case

  object FileInfo
    proto Record
    record 'file_info, 'from_lib: "kernel/include/file.hrl"
  end

  object ElixirScope
    proto Record
    record 'elixir_scope, 'from: File.expand_path("../../../include/elixir.hrl", __FILE__)
  end

  def record_constructor_with_dict_test
    record = RecordTest::FileInfo.new('type: 'regular)
    'regular = record.type
    nil      = record.access
  end

  def record_constructor_with_invalid_object_test
    invalid = [1,2,3]
    self.assert_error {'badarg, invalid}, -> RecordTest::FileInfo.new(invalid)
  end

  def record_constructor_with_invalid_tuple_name_test
    invalid = {'elixir_another,1,2,3,4,5,6}
    self.assert_error {'badrecord, invalid}, -> RecordTest::ElixirScope.new(invalid)
  end

  def record_constructor_with_invalid_tuple_size_test
    invalid = {'elixir_scope,1,2,3,4,5}
    self.assert_error {'badrecord, invalid}, -> RecordTest::ElixirScope.new(invalid)
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
    ['assign,'guard,'noname,'method,'scope,'vars,'temp_vars,
      'clause_vars,'counter,'filename,'assigned_vars] = record.record_keys
    [false,false,false,[],{[], []},_,[],_,0,$"nofile",_] = record.record_defaults
    11 = record.record_size
  end

  private

  def file_info
    { 'ok, file_info } = Erlang.file.read_file_info(__FILE__.to_bin)
    file_info
  end
end