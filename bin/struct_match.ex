defmodule Foo do
  defstruct name: "I am Foo"

  def some_function(foo_struct=%Foo{}) do
    IO.puts foo_struct.name
  end 
end

defmodule Bar do
  defstruct name: "I am Bar"

  def some_function(bar_struct=%Bar{}) do
    IO.puts bar_struct.name
  end 
end


defmodule StructMatch do
  
  defmacro struct_match(b=%Foo{}) do
    quote do
      Foo.some_function(unquote(b))
    end
  end

  defmacro struct_match(b=%Bar{}) do
    quote do
      Bar.some_function(unquote(b))
    end
  end

  defmacro struct_match(b) do
    quote do
      :erlang.||(
        :erlang.&&( 
          is_map(unquote(b)),
          (
            :erlang.||(
              :erlang.&&(unquote(b)[:__struct__] == Foo, "foo"),
              :erlang.||(
                :erlang.&&(unquote(b)[:__struct__] == Bar, "bar"),
                "unmatched"
              )
            )
          )
        ),
        "not a map"
      )
    end
  end

  def tester(b) when struct_match(b) == "foo" do
    IO.puts "FOO"
  end

  def tester(b) when struct_match(b) == "bar" do
    IO.puts "BAR"
  end

  def tester(b) when struct_match(b) == "unmatched" do
    IO.puts "UNMATCHED"
  end
end