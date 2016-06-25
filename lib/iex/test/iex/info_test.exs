Code.require_file "../test_helper.exs", __DIR__

defmodule IEx.InfoTest do
  use ExUnit.Case

  alias IEx.Info

  defmodule Foo do
    defstruct [:foo]
  end

  test "tuples" do
    assert Info.info({:ok, "good!"}) == ["Data type": "Tuple",
                                         "Reference modules": "Tuple"]
  end

  test "atoms: loaded module (without docs)" do
    info = Info.info(Foo)
    assert info[:"Data type"] == "Atom"
    assert info[:"Source"] == Path.relative_to_cwd(__ENV__.file)
    assert info[:"Description"] == "Call IEx.InfoTest.Foo.module_info() to access metadata."
    assert info[:"Raw representation"] == ~s(:"Elixir.IEx.InfoTest.Foo")
  end

  test "atoms: loaded module (with docs)" do
    info = Info.info(List)
    description = "Use h(List) to access its documentation.\n"
                  <> "Call List.module_info() to access metadata."
    assert info[:"Description"] == description
  end

  test "atoms: regular atom" do
    assert Info.info(:foo) == ["Data type": "Atom",
                               "Reference modules": "Atom"]
  end

  test "lists: charlists" do
    info = Info.info('foo')
    assert info[:"Description"] =~ "This is a list of integers that is printed"
    assert info[:"Raw representation"] == "[102, 111, 111]"
  end

  test "lists: keyword lists" do
    info = Info.info(a: 1, b: 2)
    assert info[:"Description"] =~ "This is what is referred to as a \"keyword list\""
  end

  test "lists: regular lists" do
    assert Info.info([:foo, :bar, :baz])[:"Reference modules"] == "List"
  end

  test "bitstring: strings" do
    info = Info.info("føø")
    assert info[:"Byte size"] == 5
    assert info[:"Description"] =~ "This is a string: a UTF-8 encoded binary"
    assert info[:"Raw representation"] == "<<102, 195, 184, 195, 184>>"
  end

  test "bitstring: non-printable string" do
    info = Info.info(<<"foo", 0, "bar">>)
    assert info[:"Byte size"] == 7
    assert info[:"Description"] =~ "This is a string"
    assert info[:"Description"] =~ "It's printed with the `<<>>`"
    assert info[:"Description"] =~ "the first non-printable codepoint being `<<0>>`"
  end

  test "bitstring: binary" do
    info = Info.info(<<255, 255>>)
    assert info[:"Description"] =~ "This is a binary: a collection of bytes"
  end

  test "bitstring: bitstring" do
    info = Info.info(<<1 :: 1>>)
    assert info[:"Bits size"] == 1
    assert info[:"Description"] =~ "This is a bitstring"
  end

  test "integers" do
    assert Info.info(99) == ["Data type": "Integer",
                             "Reference modules": "Integer"]
  end

  test "float" do
    assert Info.info(3.14) == ["Data type": "Float",
                               "Reference modules": "Float"]
  end

  test "functions: named function" do
    info = Info.info(&String.length/1)
    assert info[:"Type"] == "external"
    assert info[:"Arity"] == 1
  end

  test "functions: anonymous function" do
    info = Info.info(fn -> :ok end)
    assert info[:"Type"] == "local"
    assert info[:"Arity"] == 0
    assert info[:"Description"] == "This is an anonymous function."
  end

  test "PIDs" do
    pid = spawn_link(fn -> :timer.sleep(1000) end)

    info = Info.info(pid)
    assert info[:"Alive"] == true
    assert info[:"Name"] == "not registered"
    assert info[:"Links"] == inspect(self())
    assert info[:"Message queue length"] == 0

    Process.register(pid, :iex_info_registered_pid)
    Process.unlink(pid)
    send pid, :oops
    info = Info.info(pid)
    assert info[:"Name"] == ":iex_info_registered_pid"
    assert info[:"Links"] == "none"
    assert info[:"Message queue length"] == 1

    Process.exit(pid, :kill)
    assert Info.info(pid)[:"Alive"] == false
  end

  test "ports" do
    {:ok, port} = :gen_udp.open(0)
    assert Info.info(port)[:"Open"] == true
    :ok = :gen_udp.close(port)
    assert Info.info(port)[:"Open"] == false
  end

  test "references" do
    assert Info.info(make_ref()) == ["Data type": "Reference"]
  end

  test "structs" do
    info = Info.info(%Foo{})
    assert info[:"Data type"] == "IEx.InfoTest.Foo"
    assert info[:"Description"] == "This is a struct. Structs are maps with a __struct__ key."
    assert info[:"Reference modules"] == "IEx.InfoTest.Foo, Map"
  end
end
