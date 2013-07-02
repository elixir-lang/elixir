Code.require_file "test_helper.exs", __DIR__

defrecord RecordTest.FileInfo,
  Record.extract(:file_info, from_lib: "kernel/include/file.hrl")

defrecord RecordTest.SomeRecord, a: 0, b: 1
defrecord RecordTest.WithNoField, []

## Record import

defmodule RecordTest.FileInfo.Helper do
  Record.import RecordTest.FileInfo, as: :file_info

  def new do
    file_info
  end

  def size(file_info(size: size)), do: size
end

## Dynamic names and overridable

name = RecordTest.DynamicName
defrecord name, a: 0, b: 1 do
  defoverridable [update_b: 2]

  def update_b(_, _) do
    :not_optimizable
  end

  Record.import __MODULE__, as: :self
end

defmodule RecordTest.DynamicOpts do
  @a [foo: 1..30]
  defrecord State, (lc {name, _interval} inlist @a, do: {name, nil})
end

## With types

defrecord RecordTest.WithTypeOverriden, a: 0, b: 1 do
  @type t :: __MODULE__[a: integer, b: any]
end

defrecord RecordTest.WithRecordType, a: 0, b: 1 do
  record_type a: non_pos_integer
  record_type a: integer
end

defrecord RecordTest.WithInlineType, a: nil :: atom, b: 1 :: integer

defmodule RecordTest.Macros do
  defmacro gen do
    quote do
      alias RecordTest.Macros.Nested

      def this_works, do: RecordTest.Macros.Nested[]
      def this_should_too, do: Nested[]
    end
  end

  defrecord Nested do
    def nested_record_alias?(Nested[]) do
      true
    end

    defrecord NestedInNested, it_compiles: true
  end

  # Ensure there is no conflict in a nested module
  # named record.
  defrecord Record, [a: 1, b: 2]
end

defmodule RecordTest do
  use ExUnit.Case, async: true

  # Check the access from the generated macro works
  # as expected. If it compiles, we are good to go.
  require RecordTest.Macros
  RecordTest.Macros.gen

  test :record_constructor_with_dict do
    record   = RecordTest.FileInfo.new(type: :regular)
    assert record.type == :regular
    assert record.access == :undefined
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
    assert is_record(RecordTest.WithNoField.new)
    refute is_record(empty_tuple)
    refute is_record(a_list)
    refute is_record(empty_tuple, RecordTest.FileInfo)
    refute is_record(a_tuple, RecordTest.FileInfo)
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

  test :record_update do
    record = RecordTest.SomeRecord.new
    assert RecordTest.SomeRecord.a(record.update(a: 2, b: 3)) == 2
    assert RecordTest.SomeRecord.b(record.update(a: 2, b: 3)) == 3
    assert RecordTest.SomeRecord.a(record.update(a: 2)) == 2
    assert RecordTest.SomeRecord.b(record.update(b: 2)) == 2
  end

  test :optimizable do
    assert { :b, 1 } in RecordTest.SomeRecord.__record__(:optimizable)
    assert { :b, 2 } in RecordTest.SomeRecord.__record__(:optimizable)
    assert { :update_b, 2 } in RecordTest.SomeRecord.__record__(:optimizable)
    refute { :update_b, 2 } in RecordTest.DynamicName.__record__(:optimizable)
  end

  test :result do
    assert { :module, _, _, "result"} = (defrecord WithResult, foo: :bar do
      "result"
    end)
  end

  test :import do
    assert RecordTest.FileInfo.Helper.new == RecordTest.FileInfo.new
    assert RecordTest.FileInfo.Helper.size(RecordTest.FileInfo.new(size: 100)) == 100
  end

  test :extract_with_nested_records do
    namespace = Record.extract(:xmlElement, from_lib: "xmerl/include/xmerl.hrl")[:namespace]
    assert is_record(namespace, :xmlNamespace)
  end

  defp file_info do
    { :ok, file_info } = :file.read_file_info(__FILE__)
    file_info
  end

  defp empty_tuple, do: {}
  defp a_tuple, do: { :foo, :bar, :baz }
  defp a_list,  do: [ :foo, :bar, :baz ]
end
