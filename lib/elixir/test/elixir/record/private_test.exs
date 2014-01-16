Code.require_file "../test_helper.exs", __DIR__

defmodule Record.PrivateTest do
  use ExUnit.Case, async: true

  defmodule Macros do
    defrecordp :_user, __MODULE__, name: "José", age: 25
    defrecordp :_my_user, :my_user, name: "José", age: 25

    defrecordp :_file_info, Record.extract(:file_info, from_lib: "kernel/include/file.hrl")
    name = :_dynamic
    defrecordp name, [:field]

    Record.defmacros(:_macro, [c: 2, d: 3], __ENV__)

    def new() do
      _user()
    end

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

    def name_ix() do
      _user(:name)
    end

    def age_ix() do
      _user(:age)
    end

    def my_new() do
      _my_user()
    end

    def my_new(name, age) do
      _my_user(name: name, age: age)
    end

    def my_name(_my_user(name: name)) do
      name
    end

    def my_add_bar_to_name(_my_user(name: name) = user) do
      _my_user(user, name: name <> " bar")
    end

    def my_age(user) do
      _my_user(user, :age)
    end

    def my_to_keywords(user) do
      _my_user(user)
    end

    def my_name_and_age(user) do
      _my_user(user, [:name, :age])
    end

    def my_age_and_name(user) do
      _my_user(user, [:age, :name])
    end

    def my_name_ix() do
      _my_user(:name)
    end

    def my_age_ix() do
      _my_user(:age)
    end

    def file_info() do
      _file_info()
    end

    def dynamic() do
      _dynamic()
    end

    def macro() do
      _macro()
    end
  end

  test "defrecordp generates macros that generates tuples" do
    record = Macros.my_new
    assert Macros.my_name(record) == "José"

    record = Macros.my_new("Foo", 25)
    assert Macros.my_name(record) == "Foo"

    record = Macros.my_add_bar_to_name(record)
    assert Macros.my_name(record) == "Foo bar"

    assert Macros.my_age(record) == 25
    assert Macros.my_to_keywords(record) == [name: Macros.my_name(record), age: Macros.my_age(record)]

    assert Macros.my_name_and_age(record) == [Macros.my_name(record), Macros.my_age(record)]
    assert Macros.my_age_and_name(record) == [Macros.my_age(record), Macros.my_name(record)]

    assert elem(record, 0) == :my_user
  end

  test "defrecordp generates tuples with custom first element" do
    record = Macros.new
    assert record.name == "José"

    record = Macros.new("Foo", 25)
    assert record.name == "Foo"

    record = record.add_bar_to_name
    assert record.name == "Foo bar"

    assert record.age == 25
    assert record.to_keywords == [name: record.name, age: record.age]

    assert record.name_and_age == [record.name, record.age]
    assert record.age_and_name == [record.age, record.name]

    assert elem(record, 0) == Macros
  end

  test "defrecordp access index" do
    assert Macros.name_ix == 1
    assert Macros.age_ix == 2
    assert Macros.my_name_ix == 1
    assert Macros.my_age_ix == 2
  end

  test "defrecordp with dynamic arguments" do
    assert [:_file_info|_] = Macros.file_info() |> tuple_to_list
    assert { :_dynamic, nil } = Macros.dynamic()
  end

  test "defmacros" do
    assert { :_macro, 2, 3 } = Macros.macro()
  end

  defrecordp :match, [:name]

  test "defrecordp generates macros that are matchable" do
    assert match(name: 42) = match(name: 42)
  end
end
