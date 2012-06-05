Code.require_file "../test_helper", __FILE__

defrecord RecordTest.FileInfo,
  Record.extract(:file_info, from_lib: "kernel/include/file.hrl")

name = RecordTest.DynamicName
defrecord name, a: 0, b: 1

defrecord RecordTest.WithNoField, []

defrecord RecordTest.Override, a: 0 do

  def new, do: new([a: 100])

  def new(opts) do
      opts = Keyword.put opts, :a, opts[:a] * 5
      super(opts)
  end

  def a(value, r) do
    super({value, :erlang.now}, r)
  end
  def a(r) do
    {v, _} = super(r)
    v
  end
  def update_a(fun, r) do
    r.a(fun.(r.a))
  end
  def prepend_a(_, r) do
    r.a(r.a)
  end
  def merge_a(_, r) do
    r.a(r.a)
  end
  def increment_a(_, r) do
      r
  end

  def raw_a(r), do: elem(r, 2)
end

defmodule RecordTest do
  use ExUnit.Case

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
  
  test :to_keywords do
    record = RecordTest.DynamicName.new(a: "a", b: "b")
    assert record.to_keywords[:a] == "a"
    assert record.to_keywords[:b] == "b"
  end

  test :overridable do
    record = RecordTest.Override.new
    assert record.raw_a == 500
    record = record.a(1)
    assert {1, _} = record.raw_a
    assert record.a == 1
    record = record.update_a(fn(_) -> 2 end)
    assert {2, _} = record.raw_a
    assert record.a == 2
    record = record.prepend_a(100)
    assert {2, _} = record.raw_a
    assert record.a == 2
    record = record.merge_a(100)
    assert {2, _} = record.raw_a
    assert record.a == 2
    record = record.increment_a(1)
    assert {2, _} = record.raw_a
    assert record.a == 2
  end

  defp file_info do
    { :ok, file_info } = Erlang.file.read_file_info(__FILE__)
    file_info
  end

  defp a_list do
    [:a, :b, :c]
  end
end
