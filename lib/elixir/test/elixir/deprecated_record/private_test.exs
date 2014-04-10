Code.require_file "../test_helper.exs", __DIR__

defmodule Record.PrivateTest do
  use ExUnit.Case, async: true

  require Record

  defmodule Macros do
    defrecordp :_user, __MODULE__, name: "José", age: 25
    defrecordp :_my_user, :my_user, name: "José", age: 25

    name = :_file_info
    defrecordp name, Record.extract(:file_info, from_lib: "kernel/include/file.hrl")

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

    def age(user) do
      _user(user, :age)
    end

    def add_bar_to_name(_user(name: name) = user) do
      _user(user, name: name <> " bar")
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

    def my_age(user) do
      _my_user(user, :age)
    end

    def my_add_bar_to_name(_my_user(name: name) = user) do
      _my_user(user, name: name <> " bar")
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

    def macro() do
      _macro()
    end
  end

  test "defrecordp generates macros that generates tuples" do
    record = Macros.my_new
    assert Macros.my_name(record) == "José"
    assert Macros.my_age(record) == 25

    record = Macros.my_new("Foo", 25)
    assert Macros.my_name(record) == "Foo"

    record = Macros.my_add_bar_to_name(record)
    assert Macros.my_name(record) == "Foo bar"

    assert elem(record, 0) == :my_user
  end

  test "defrecordp access index" do
    assert Macros.name_ix == 1
    assert Macros.age_ix == 2
    assert Macros.my_name_ix == 1
    assert Macros.my_age_ix == 2
  end

  test "defrecordp with dynamic arguments" do
    assert [:_file_info|_] = Macros.file_info() |> tuple_to_list
  end

  test "defmacros" do
    assert { :_macro, 2, 3 } = Macros.macro()
  end

  defrecordp :match, [:name]

  test "defrecordp generates macros that are matchable" do
    assert match(name: 42) = match(name: 42)
  end
end
