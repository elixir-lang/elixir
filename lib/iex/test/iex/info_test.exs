Code.require_file("../test_helper.exs", __DIR__)

defmodule IEx.InfoTest do
  use ExUnit.Case

  alias IEx.Info

  defmodule Foo do
    defstruct [:foo]
  end

  test "tuples" do
    assert Info.info({:ok, "good!"}) == [{"Data type", "Tuple"}, {"Reference modules", "Tuple"}]
  end

  describe "atoms" do
    test "loaded module (without docs)" do
      info = Info.info(Foo)
      assert get_key(info, "Data type") == "Atom"
      assert get_key(info, "Source") == Path.relative_to_cwd(__ENV__.file)

      assert get_key(info, "Description") ==
               "Call IEx.InfoTest.Foo.module_info() to access metadata."

      assert get_key(info, "Raw representation") == ~s(:"Elixir.IEx.InfoTest.Foo")
    end

    test "loaded module (with docs)" do
      info = Info.info(List)

      description = """
      Use h(List) to access its documentation.
      Call List.module_info() to access metadata.\
      """

      assert get_key(info, "Description") == description
    end

    test "module that is also a protocol" do
      info = Info.info(String.Chars)
      protocol_info = get_key(info, "Protocol")
      assert protocol_info =~ "This module is a protocol"
      assert protocol_info =~ "Atom"
      assert protocol_info =~ "BitString"
    end

    test "module-like atom (Foo)" do
      info = Info.info(NonexistentModuleAtom)
      assert get_key(info, "Raw representation") == ~s(:"Elixir.NonexistentModuleAtom")
    end

    test "regular atom" do
      assert Info.info(:foo) == [{"Data type", "Atom"}, {"Reference modules", "Atom"}]
    end
  end

  describe "lists" do
    test "charlists" do
      info = Info.info('foo')
      assert get_key(info, "Description") =~ "This is a list of integers that is printed"
      assert get_key(info, "Raw representation") == "[102, 111, 111]"
    end

    test "keyword lists" do
      info = Info.info(a: 1, b: 2)
      assert get_key(info, "Description") =~ "This is what is referred to as a \"keyword list\""
    end

    test "regular lists" do
      assert get_key(Info.info([]), "Reference modules") == "List"
      assert get_key(Info.info([:foo, :bar, :baz]), "Reference modules") == "List"
    end
  end

  describe "bitstring" do
    test "strings" do
      info = Info.info("føø")
      assert get_key(info, "Byte size") == 5
      assert get_key(info, "Description") =~ "This is a string: a UTF-8 encoded binary"
      assert get_key(info, "Raw representation") == "<<102, 195, 184, 195, 184>>"
    end

    test "non-printable string" do
      info = Info.info(<<"foo", 0, "bar">>)
      description = get_key(info, "Description")

      assert get_key(info, "Byte size") == 7
      assert description =~ "This is a string"
      assert description =~ "It's printed with the `<<>>`"
      assert description =~ "the first non-printable code point being\n`<<0>>`"
    end

    test "binary" do
      info = Info.info(<<255, 255>>)
      assert get_key(info, "Description") =~ "This is a binary: a collection of bytes"
    end

    test "bitstring" do
      info = Info.info(<<1::1>>)
      assert get_key(info, "Bits size") == 1
      assert get_key(info, "Description") =~ "This is a bitstring"
    end
  end

  test "integers" do
    assert Info.info(99) == [{"Data type", "Integer"}, {"Reference modules", "Integer"}]
  end

  test "float" do
    assert Info.info(3.14) == [{"Data type", "Float"}, {"Reference modules", "Float"}]
  end

  describe "functions" do
    test "named function" do
      info = Info.info(&String.length/1)
      assert get_key(info, "Type") == "external"
      assert get_key(info, "Arity") == 1
    end

    test "anonymous function" do
      info = Info.info(fn -> :ok end)
      assert get_key(info, "Type") == "local"
      assert get_key(info, "Arity") == 0
      assert get_key(info, "Description") == "This is an anonymous function."
    end
  end

  test "PIDs" do
    pid = spawn_link(fn -> Process.sleep(1000) end)

    info = Info.info(pid)
    assert get_key(info, "Alive") == true
    assert get_key(info, "Name") == "not registered"
    assert get_key(info, "Links") == inspect(self())
    assert get_key(info, "Message queue length") == 0

    Process.register(pid, :iex_info_registered_pid)
    Process.unlink(pid)
    send(pid, :oops)
    info = Info.info(pid)
    assert get_key(info, "Name") == ":iex_info_registered_pid"
    assert get_key(info, "Links") == "none"
    assert get_key(info, "Message queue length") == 1

    Process.exit(pid, :kill)
    assert get_key(Info.info(pid), "Alive") == false
  end

  test "ports" do
    {:ok, port} = :gen_udp.open(0)
    assert get_key(Info.info(port), "Open") == true
    :ok = :gen_udp.close(port)
    assert get_key(Info.info(port), "Open") == false
  end

  test "references" do
    assert Info.info(make_ref()) == [{"Data type", "Reference"}]
  end

  describe "calendar types" do
    test "date" do
      {:ok, date} = Date.new(2017, 1, 1)
      info = Info.info(date)
      assert get_key(info, "Data type") == "Date"

      assert get_key(info, "Raw representation") ==
               "%Date{calendar: Calendar.ISO, day: 1, month: 1, year: 2017}"

      assert get_key(info, "Reference modules") == "Date, Calendar, Map"
      assert get_key(info, "Description") =~ "a date"
      assert get_key(info, "Description") =~ "`~D`"
    end

    test "time" do
      {:ok, time} = Time.new(23, 59, 59)
      info = Info.info(time)
      assert get_key(info, "Data type") == "Time"

      assert get_key(info, "Raw representation") ==
               "%Time{calendar: Calendar.ISO, hour: 23, microsecond: {0, 0}, minute: 59, second: 59}"

      assert get_key(info, "Reference modules") == "Time, Calendar, Map"
      assert get_key(info, "Description") =~ "a time"
      assert get_key(info, "Description") =~ "`~T`"
    end

    test "naive datetime" do
      {:ok, time} = NaiveDateTime.new(2017, 1, 1, 23, 59, 59)
      info = Info.info(time)
      assert get_key(info, "Data type") == "NaiveDateTime"

      assert get_key(info, "Raw representation") ==
               "%NaiveDateTime{calendar: Calendar.ISO, day: 1, hour: 23, microsecond: {0, 0}, minute: 59, month: 1, second: 59, year: 2017}"

      assert get_key(info, "Reference modules") == "NaiveDateTime, Calendar, Map"

      assert get_key(info, "Description") =~
               ~S{a "naive" datetime (that is, a datetime without a time zone)}

      assert get_key(info, "Description") =~ "`~N`"
    end
  end

  test "structs" do
    info = Info.info(%Foo{})
    assert get_key(info, "Data type") == "IEx.InfoTest.Foo"

    assert get_key(info, "Description") ==
             "This is a struct. Structs are maps with a __struct__ key."

    assert get_key(info, "Reference modules") == "IEx.InfoTest.Foo, Map"
  end

  defp get_key(info, key) do
    {^key, value} = List.keyfind(info, key, 0)
    value
  end
end
