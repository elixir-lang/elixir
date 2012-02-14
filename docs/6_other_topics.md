# 6 Other topics

# 6.1 Partial application

Elixir also supports partial application. Let's suppose we have a list of strings and we want to calculate the size for each them. We could do it as follow:

    iex> list = ["foo", "bar", "baz"]
    ["foo","bar","baz"]
    iex> Enum.map list, fn(x) -> size(x) end
    [3,3,3]

However, with partial application, we could also do:

    iex> Enum.map list, size(&1)
    [3,3,3]

In the example above, we have invoked the function `size` passing `&1` as argument asking Elixir to generate a function that expects one argument and that argument will be passed to `size`.

Since operators are also function calls they can also be partially applied:

    iex> Enum.map [1,2,3], &1 * 2
    [2,4,6]

All functions can be partially applied, except [Elixir's special forms](https://github.com/josevalim/elixir/tree/master/lib/elixir/special_forms.ex).

# 6.2 Comprehensions

Elixir also provide list and bit comprehensions. List comprehensions allow you to quickly build a list from another list:

    iex> lc n in [1,2,3,4], do: n * 2
    [2,4,6,8]

Or, using key-value blocks:

    lc n in [1,2,3,4] do
      n * 2
    end

A comprehension accepts several expressions. Those expressions can be generators, as in `x in [1,2,3,4]`, or filters:

    # A comprehension with a generator and a filter
    iex> lc n in [1,2,3,4,5,6], rem(n, 2) == 0, do: n
    [2,4,6]

    # A comprehension with two generators
    iex> lc x in [1,2], y in [2,3], do: x*y
    [2,3,4,6]

Elixir provides generators for both lists and bitstrings (a bitstring is a sequence of bits. An Elixir binary, for example, is a bitstring where the number of bits is multiple of 8):

    # A list generator:
    iex> lc n in [1,2,3,4], do: n * 2
    [2,4,6,8]

    # A bit string generator:
    iex> lc <<n>> in <<1,2,3,4>>, do: n * 2
    [2,4,6,8]

Bit string generators are quite useful when you need to organize bit string streams:

    iex> pixels = <<213,45,132,64,76,32,76,0,0,234,32,15>>
    iex> lc <<r:8,g:8,b:8>> in pixels, do: {r,g,b}
    [{213,45,132},{64,76,32},{76,0,0},{234,32,15}]

Remember, as strings are binaries and a binary is a bitstring, we can also use strings on comprehensions. For instance, the example below removes all white space characters from a string via bit comprehensions:

    iex> bc <<c>> in " hello world ", c != ?\s, do: <<c>>
    "helloworld"

Elixir does its best to hide the differences between list and bit string generators. However, there is a special case due to Erlang limitation that we need to explicitly tell Erlang that a list is being given as argument:

    # This will fail because when Elixir sees that the left side
    # of the in expression is a bit string, it expects the right side
    # to be a bit string as well:
    iex> lc <<n>> in [<<1>>,<<2>>,<<3>>], do: n*2
    ** (ErlangError) erlang error {:bad_generator,[<<1>>,<<2>>,<<3>>]}

    # You need to be explicit and use inlist:
    iex> lc inlist(<<n>>, [<<1>>,<<2>>,<<3>>]), do: n*2
    [2,4,6]

    # For consistency, inbin is also available:
    iex> lc inbin(<<n>>, <<1,2,3>>), do: n*2
    [2,4,6]

# 6.3 Native compilation

Elixir can compile to native code using the Hipe compiler. All you need to do is to export the following before running your code:

    export ERL_COMPILER_OPTIONS=native

[Chapter 5: Macros](https://github.com/josevalim/elixir/blob/master/docs/5_macros.md) | [Index](https://github.com/josevalim/elixir/blob/master/docs/0_index.md)
