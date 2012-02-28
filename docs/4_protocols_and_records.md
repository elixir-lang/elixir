# 4 Records & Protocols

Elixir provides both protocols and records. This section will outline the main features on both and provide some examples. More specifically, we will learn how to use `defrecord`, `defprotocol` and `defimpl`.

## 4.1 Records

Records are simple structures that holds values. For example, we can define a `FileInfo` record that is supposed to store information about files as follows:

    defrecord FileInfo, atime: nil, mtime: nil, accesses: 0

The line above will define a module named `FileInfo` which contain a function named `new` that returns a new record and other functions to read and set the values in the record. Therefore, we can do:

    file_info = FileInfo.new(atime: now())
    file_info.atime         #=> Returns the value of atime
    file_info.atime(now())  #=> Updates the value of atime

Elixir will also define a `update_#{field}` function that accepts a function as argument that will receive the old value and update the current value with the result of the function:

    file_info = FileInfo.new(accesses: 10)
    file_info = file_info.update_accesses(fn(x) -> x + 1 end)
    file_info.accesses #=> 11

Internally, a record is simply a tuple where the first element is always the record module name. This can be noticed if we create and print the record in Interactive Elixir (`bin/iex`):

    iex> defrecord FileInfo, atime: nil, mtime: nil
    iex> FileInfo.new
    {::FileInfo, nil, nil}

### 4.1.1 Default based functions

Depending on the default value, Elixir will define helpers to interact with the record. For example, the test framework that ships with Elixir, called ExUnit, defines a record which keeps track of how many tests were executed and the failures that happened. The record definition is similar to:

    iex> defrecord Config, counter: 0, failures: []

Since `counter` is an integer, Elixir automatically defines a helper named `increment_counter` that will increase the counter value:

    iex> new_config = Config.new.increment_counter
    {::Config, 1, []}
    iex> new_config.counter
    1

`increment_counter` also accepts a number to increment as argument:

    iex> new_config = Config.new.increment_counter 10
    {::Config, 10, []}
    iex> new_config.counter
    10

On the other hand, if the default value is a list Elixir will define the two following helpers:

* `prepend_field` - Receives another list and prepend its values
* `merge_field` - Receives an orddict (which is a list of tuples) and merge it into the current value;

## 4.2 Protocols

Protocols allows us to define contracts. Dispatching a protocol is available to any data type as long as it implements the prototype. Let's consider a practical example.

In Elixir, only `false` and `nil` are considered falsy values. Everything else evaluates to true. Depending on the application, it may be important to specify a `blank?` protocol that returns a boolean for other data types that should be considered blank. For instance, an empty list or an empty binary could be considered blanks.

We could implement this protocol as follows:

    defprotocol Blank, [ blank?(data) ]

The protocol expects a function called `blank?` expecting one argument to be implemented. We can implement this protocol for some Elixir data types as follows:

    # Numbers are never blank
    defimpl Blank, for: Number do
      def blank?(number), do: false
    end

    # Just empty list is blank
    defimpl Blank, for: List do
      def blank?([]), do: true
      def blank?(_),  do: false
    end

    # Just the atoms false and nil are blank
    defimpl Blank, for: Atom do
      def blank?(false), do: true
      def blank?(nil),   do: true
      def blank?(_),     do: false
    end

And we would do so for all native data types. The types available are:

* Record
* Tuple
* Atom
* List
* BitString
* Number
* Function
* PID
* Port
* Reference
* Any

### 4.2.1 Selecting implementations

Implementing the protocol for all 9 types above can be cumbersome. Even more if you consider that Number, Function, PID, Port and Reference are never going to be blank. For this reason, Elixir allows us to declare that we are going to implement the protocol just for some types, as follows:

    defprotocol Blank, [blank?(data)], only: [Atom, Tuple, List, BitString, Any]

Since we also specified `Any` as a data type, if the data type is not any of Atom, Tuple, List or BitString, it will automatically fallback to Any:

    defimpl Blank, for: Any do
      def blank?(_), do: false
    end

Now all data types that we have not specified will be automatically considered non blank.

### 4.2.2 Using protocols with records

The real benefit of protocols comes when mixed with records. For instance, one may implement a custom dictionary as a Red-Black tree and this dictionary should also be considered blank in case it has no items. That said, the developer just needs to implement the protocol for this dictionary:

    defimpl Blank, for: RedBlack::Dict do
      def blank?(dict), do: RedBlack.empty?(dict)
    end

In the example above, we have implemented `blank?` for the custom dictionary that simply delegates to `RedBlack.empty?`. Finally, since records are simply tuples, the default implementation for records can be given in the tuple implementation.

### 4.2.3 Built-in protocols

Elixir ships with three built-in protocols, they are:

* Enum::Iterator - specifies an iteration contract for any data structure
* String::Chars - specifies how to convert a data structure with characters to binary
* List::Chars - specifies how to convert a data structure with characters to lists
* String::Inspect - specifies how to convert any data structure to a string for inspection

You can check the source code of those files for more information about how the protocol is used and how to implement your own. With this, we have finally finished this section which has described `defrecord`, `defprotocol` and `defimpl`. Next, we are going to discuss macros with `defmacro`!

[Chapter 3: Modules](https://github.com/elixir-lang/elixir/blob/master/docs/3_modules.md) | [Index](https://github.com/elixir-lang/elixir/blob/master/docs/0_index.md) |
[Chapter 5: Macros](https://github.com/elixir-lang/elixir/blob/master/docs/5_macros.md)