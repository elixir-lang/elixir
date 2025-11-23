<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

# IO and the file system

This chapter introduces the input/output mechanisms, file-system-related tasks, and related modules such as `IO`, `File`, and `Path`. The IO system provides a great opportunity to shed some light on some philosophies and curiosities of Elixir and the Erlang VM.

## The `IO` module

The `IO` module is the main mechanism in Elixir for reading and writing to standard input/output (`:stdio`), standard error (`:stderr`), files, and other IO devices. Usage of the module is pretty straightforward:

```elixir
iex> IO.puts("hello world")
hello world
:ok
iex> IO.gets("yes or no? ")
yes or no? yes
"yes\n"
```

By default, functions in the `IO` module read from the standard input and write to the standard output. We can change that by passing, for example, `:stderr` as an argument (in order to write to the standard error device):

```elixir
iex> IO.puts(:stderr, "hello world")
hello world
:ok
```

## The `File` module

The `File` module contains functions that allow us to open files as IO devices. By default, files are opened in binary mode, which requires developers to use the specific `IO.binread/2` and `IO.binwrite/2` functions from the `IO` module:

> ### Potential data loss warning {: .warning}
>
> The following code opens a file for writing. If an existing file is available at the given path, its contents will be deleted.

```elixir
iex> {:ok, file} = File.open("path/to/file/hello", [:write])
{:ok, #PID<0.47.0>}
iex> IO.binwrite(file, "world")
:ok
iex> File.close(file)
:ok
iex> File.read("path/to/file/hello")
{:ok, "world"}
```

The file could be opened with the `:append` option, instead of `:write`, to preserve its contents. You may also pass the `:utf8` option, which tells the `File` module to interpret the bytes read from the file as UTF-8-encoded bytes.

Besides functions for opening, reading and writing files, the `File` module has many functions to work with the file system. Those functions are named after their UNIX equivalents. For example, `File.rm/1` can be used to remove files, `File.mkdir/1` to create directories, `File.mkdir_p/1` to create directories and all their parent chain. There are even `File.cp_r/2` and `File.rm_rf/1` to respectively copy and remove files and directories recursively (i.e., copying and removing the contents of the directories too).

You will also notice that functions in the `File` module have two variants: one "regular" variant and another variant with a trailing bang (`!`). For example, when we read the `"hello"` file in the example above, we use `File.read/1`. Alternatively, we can use `File.read!/1`:

```elixir
iex> File.read("path/to/file/hello")
{:ok, "world"}
iex> File.read!("path/to/file/hello")
"world"
iex> File.read("path/to/file/unknown")
{:error, :enoent}
iex> File.read!("path/to/file/unknown")
** (File.Error) could not read file "path/to/file/unknown": no such file or directory
```

Notice that the version with `!` returns the contents of the file instead of a tuple, and if anything goes wrong the function raises an error.

The version without `!` is preferred when you want to handle different outcomes using pattern matching:

```elixir
case File.read("path/to/file/hello") do
  {:ok, body} -> # do something with the `body`
  {:error, reason} -> # handle the error caused by `reason`
end
```

However, if you expect the file to be there, the bang variation is more useful as it raises a meaningful error message. Avoid writing:

```elixir
{:ok, body} = File.read("path/to/file/unknown")
```

as, in case of an error, `File.read/1` will return `{:error, reason}` and the pattern matching will fail. You will still get the desired result (a raised error), but the message will be about the pattern which doesn't match (thus being cryptic in respect to what the error actually is about).

Therefore, if you don't want to handle the error outcomes, prefer to use the functions ending with an exclamation mark, such as `File.read!/1`.

## The `Path` module

The majority of the functions in the `File` module expect paths as arguments. Most commonly, those paths will be regular binaries. The `Path` module provides facilities for working with such paths:

```elixir
iex> Path.join("foo", "bar")
"foo/bar"
iex> Path.expand("~/hello")
"/Users/jose/hello"
```

Using functions from the `Path` module as opposed to directly manipulating strings is preferred since the `Path` module takes care of different operating systems transparently. Finally, keep in mind that Elixir will automatically convert slashes (`/`) into backslashes (`\`) on Windows when performing file operations.

With this, we have covered the main modules that Elixir provides for dealing with IO and interacting with the file system. In the next section, we will peek a bit under the covers and learn how the IO system is implemented in the VM.

## Processes

You may have noticed that `File.open/2` returns a tuple like `{:ok, pid}`:

```elixir
iex> {:ok, file} = File.open("hello")
{:ok, #PID<0.47.0>}
```

This happens because the `IO` module actually works with processes (see [the previous chapter](processes.md)). Given a file is a process, when you write to a file that has been closed, you are actually sending a message to a process which has been terminated:

```elixir
iex> File.close(file)
:ok
iex> IO.write(file, "is anybody out there")
** (ErlangError) Erlang error: :terminated:

  * 1st argument: the device has terminated

    (stdlib 5.0) io.erl:94: :io.put_chars(#PID<0.114.0>, "is anybody out there")
    iex:4: (file)
```

Let's see in more detail what happens when you request `IO.write(pid, binary)`. The `IO` module sends a message to the process identified by `pid` with the desired operation. A small ad-hoc process can help us see it:

```elixir
iex> pid = spawn(fn ->
...>   receive do
...>     msg -> IO.inspect(msg)
...>   end
...> end)
#PID<0.57.0>
iex> IO.write(pid, "hello")
{:io_request, #PID<0.41.0>, #Reference<0.0.8.91>,
 {:put_chars, :unicode, "hello"}}
** (ErlangError) erlang error: :terminated
```

After `IO.write/2`, we can see the request sent by the `IO` module printed out (a four-elements tuple). Soon after that, we see that it fails since the `IO` module expected some kind of result, which we did not supply.

By modeling IO devices with processes, the Erlang VM allows us to even read and write to files across nodes. Neat!

## `iodata` and `chardata`

In all of the examples above, we used binaries when writing to files. However, most of the IO functions in Elixir also accept either "iodata" or "chardata".

One of the main reasons for using "iodata" and "chardata" is for performance. For example,
imagine you need to greet someone in your application:

```elixir
name = "Mary"
IO.puts("Hello " <> name <> "!")
```

Given strings in Elixir are immutable, as most data structures, the example above will copy the string "Mary" into the new "Hello Mary!" string. While this is unlikely to matter for the short string as above, copying can be quite expensive for large strings! For this reason, the IO functions in Elixir allow you to pass instead a list of strings:

```elixir
name = "Mary"
IO.puts(["Hello ", name, "!"])
```

In the example above, there is no copying. Instead we create a list that contains the original name. We call such lists either "iodata" or "chardata" and we will learn the precise difference between them soon.

Those lists are very useful because it can actually simplify the processing strings in several scenarios. For example, imagine you have a list of values, such as `["apple", "banana", "lemon"]` that you want to write to disk separated by commas. How can you achieve this?

One option is to use `Enum.join/2` and convert the values to a string:

```elixir
iex> Enum.join(["apple", "banana", "lemon"], ",")
"apple,banana,lemon"
```

The above returns a new string by copying each value into the new string. However, with the knowledge in this section, we know that we can pass a list of strings to the IO/File functions. So instead we can do:

```elixir
iex> Enum.intersperse(["apple", "banana", "lemon"], ",")
["apple", ",", "banana", ",", "lemon"]
```

"iodata" and "chardata" do not only contain strings, but they may contain arbitrary nested lists of strings too:

```elixir
iex> IO.puts(["apple", [",", "banana", [",", "lemon"]]])
```

"iodata" and "chardata" may also contain integers. For example, we could print our comma separated list of values by using `?,` as separator, which is the integer representing a comma (`44`):

```elixir
iex> IO.puts(["apple", ?,, "banana", ?,, "lemon"])
```

The difference between "iodata" and "chardata" is precisely what said integer represents. For iodata, the integers represent bytes. For chardata, the integers represent Unicode codepoints. For ASCII characters, the byte representation is the same as the codepoint representation, so it fits both classifications. However, the default IO device works with chardata, which means we can do:

```elixir
iex> IO.puts([?O, ?l, ?á, ?\s, "Mary", ?!])
```

Charlists, such as `~c"hello world"`, are lists of integers, and therefore are chardata.

We packed a lot into this small section, so let's break it down:

  * iodata and chardata are lists of binaries and integers. Those binaries and integers can be arbitrarily nested inside lists. Their goal is to give flexibility and performance when working with IO devices and files;

  * the choice between iodata and chardata depends on the encoding of the IO device. If the file is opened without encoding, the file expects iodata, and the functions in the `IO` module starting with `bin*` must be used. The default IO device (`:stdio`) and files opened with `:utf8` encoding expect chardata and work with the remaining functions in the `IO` module;

This finishes our tour of IO devices and IO related functionality. We have learned about three Elixir modules - `IO`, `File`, and `Path` - as well as how the VM uses processes for the underlying IO mechanisms and how to use `chardata` and `iodata` for IO operations.
