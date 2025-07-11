<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

# Doctests, patterns, and with

In this chapter, we will implement the code that parses the commands we described in the first chapter:

```text
CREATE shopping
OK

PUT shopping milk 1
OK

PUT shopping eggs 3
OK

GET shopping milk
1
OK

DELETE shopping eggs
OK
```

After the parsing is done, we will update our server to dispatch the parsed commands to the relevant buckets.

## Doctests

On the language homepage, we mention that Elixir makes documentation a first-class citizen in the language. We have explored this concept many times throughout this guide, be it via `mix help` or by typing `h Enum` or another module in an IEx console.

In this section, we will implement the parsing functionality, document it and make sure our documentation is up to date with doctests. This helps us provide documentation with accurate code samples.

Let's create our command parser at `lib/kv/command.ex` and start with the doctest:

```elixir
defmodule KV.Command do
  @doc ~S"""
  Parses the given `line` into a command.

  ## Examples

      iex> KV.Command.parse("CREATE shopping\r\n")
      {:ok, {:create, "shopping"}}

  """
  def parse(_line) do
    :not_implemented
  end
end
```

Doctests are specified by an indentation of four spaces followed by the `iex>` prompt in a documentation string. If a command spans multiple lines, you can use `...>`, as in IEx. The expected result should start at the next line after `iex>` or `...>` line(s) and is terminated either by a newline or a new `iex>` prefix.

Also, note that we started the documentation string using `@doc ~S"""`. The `~S` prevents the `\r\n` characters from being converted to a carriage return and line feed until they are evaluated in the test.

To run our doctests, we'll create a file at `test/kv/command_test.exs` and call `doctest KV.Command` in the test case:

```elixir
defmodule KV.CommandTest do
  use ExUnit.Case, async: true
  doctest KV.Command
end
```

Run the test suite and the doctest should fail:

```text
  1) doctest KV.Command.parse/1 (1) (KV.CommandTest)
     test/kv/command_test.exs:3
     Doctest failed
     doctest:
       iex> KV.Command.parse("CREATE shopping\r\n")
       {:ok, {:create, "shopping"}}
     code: KV.Command.parse "CREATE shopping\r\n" === {:ok, {:create, "shopping"}}
     left:  :not_implemented
     right: {:ok, {:create, "shopping"}}
     stacktrace:
       lib/kv/command.ex:7: KV.Command (module)
```

Excellent!

Now let's make the doctest pass. Let's implement the `parse/1` function:

```elixir
def parse(line) do
  case String.split(line) do
    ["CREATE", bucket] -> {:ok, {:create, bucket}}
  end
end
```

Our implementation splits the line on whitespace and then matches the command against a list. Using `String.split/1` means our commands will be whitespace-insensitive. Leading and trailing whitespace won't matter, nor will consecutive spaces between words. Let's add some new doctests to test this behavior along with the other commands:

```elixir
  @doc ~S"""
  Parses the given `line` into a command.

  ## Examples

      iex> KV.Command.parse "CREATE shopping\r\n"
      {:ok, {:create, "shopping"}}

      iex> KV.Command.parse "CREATE  shopping  \r\n"
      {:ok, {:create, "shopping"}}

      iex> KV.Command.parse "PUT shopping milk 1\r\n"
      {:ok, {:put, "shopping", "milk", "1"}}

      iex> KV.Command.parse "GET shopping milk\r\n"
      {:ok, {:get, "shopping", "milk"}}

      iex> KV.Command.parse "DELETE shopping eggs\r\n"
      {:ok, {:delete, "shopping", "eggs"}}

  Unknown commands or commands with the wrong number of
  arguments return an error:

      iex> KV.Command.parse "UNKNOWN shopping eggs\r\n"
      {:error, :unknown_command}

      iex> KV.Command.parse "GET shopping\r\n"
      {:error, :unknown_command}

  """
```

With doctests at hand, it is your turn to make tests pass! Once you're ready, you can compare your work with our solution below:

```elixir
  def parse(line) do
    case String.split(line) do
      ["CREATE", bucket] -> {:ok, {:create, bucket}}
      ["GET", bucket, key] -> {:ok, {:get, bucket, key}}
      ["PUT", bucket, key, value] -> {:ok, {:put, bucket, key, value}}
      ["DELETE", bucket, key] -> {:ok, {:delete, bucket, key}}
      _ -> {:error, :unknown_command}
    end
  end
```

Notice how we were able to elegantly parse the commands without adding a bunch of `if/else` clauses that check the command name and number of arguments!

Finally, you may have observed that each doctest corresponds to a different test in our suite, which now reports a total of 7 doctests. That is because ExUnit considers the following to define two different doctests:

```elixir
iex> KV.Command.parse("UNKNOWN shopping eggs\r\n")
{:error, :unknown_command}

iex> KV.Command.parse("GET shopping\r\n")
{:error, :unknown_command}
```

Without new lines, as seen below, ExUnit compiles it into a single doctest:

```elixir
iex> KV.Command.parse("UNKNOWN shopping eggs\r\n")
{:error, :unknown_command}
iex> KV.Command.parse("GET shopping\r\n")
{:error, :unknown_command}
```

As the name says, doctest is documentation first and a test later. Their goal is not to replace tests but to provide up-to-date documentation. You can read more about doctests in the `ExUnit.DocTest` documentation.

## Using `with`

As we are now able to parse commands, we can finally start implementing the logic that runs the commands. Let's add a stub definition for this function for now:

```elixir
defmodule KV.Command do
  @doc """
  Runs the given command.
  """
  def run(command, socket) do
    :gen_tcp.send(socket, "OK\r\n")
    :ok
  end
end
```

Before we implement this function, let's change our server to start using our new `parse/1` and `run/1` functions. Remember, our `read_line/1` function was also crashing when the client closed the socket, so let's take the opportunity to fix it, too. Open up `lib/kv/server.ex` and replace the existing server definition:

```elixir
  defp serve(socket) do
    socket
    |> read_line()
    |> write_line(socket)

    serve(socket)
  end

  defp read_line(socket) do
    {:ok, data} = :gen_tcp.recv(socket, 0)
    data
  end

  defp write_line(line, socket) do
    :gen_tcp.send(socket, line)
  end
```

by the following:

```elixir
  defp serve(socket) do
    msg =
      case read_line(socket) do
        {:ok, data} ->
          case KV.Command.parse(data) do
            {:ok, command} ->
              KV.Command.run(command, socket)

            {:error, _} = err ->
              err
          end

        {:error, _} = err ->
          err
      end

    write_line(socket, msg)
    serve(socket)
  end

  defp read_line(socket) do
    :gen_tcp.recv(socket, 0)
  end

  defp write_line(_socket, :ok) do
    :ok
  end

  defp write_line(socket, {:error, :unknown_command}) do
    # Known error; write to the client
    :gen_tcp.send(socket, "UNKNOWN COMMAND\r\n")
  end

  defp write_line(_socket, {:error, :closed}) do
    # The connection was closed, exit politely
    exit(:shutdown)
  end

  defp write_line(socket, {:error, error}) do
    # Unknown error; write to the client and exit
    :gen_tcp.send(socket, "ERROR\r\n")
    exit(error)
  end
```

If we start our server, we can now send commands to it. For now, we will get two different responses: "OK" when the command is known and "UNKNOWN COMMAND" otherwise:

```console
$ telnet 127.0.0.1 4040
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
CREATE shopping
OK
HELLO
UNKNOWN COMMAND
```

This means our implementation is going in the correct direction, but it doesn't look very elegant, does it?

The previous implementation used pipelines which made the logic straightforward to follow. However, now that we need to handle different error codes along the way, our server logic is nested inside many `case` calls.

Thankfully, Elixir has the `with` construct, which allows you to simplify code like the above, replacing nested `case` calls with a chain of matching clauses. Let's rewrite the `serve/1` function to use `with`:

```elixir
  defp serve(socket) do
    msg =
      with {:ok, data} <- read_line(socket),
          {:ok, command} <- KV.Command.parse(data),
          do: KV.Command.run(command, socket)

    write_line(socket, msg)
    serve(socket)
  end
```

Much better! `with` will retrieve the value returned by the right-side of `<-` and match it against the pattern on the left side. If the value matches the pattern, `with` moves on to the next expression. In case there is no match, the non-matching value is returned.

In other words, we converted each expression given to `case/2` as a step in `with`. As soon as any of the steps return something that does not match `{:ok, x}`, `with` aborts, and returns the non-matching value.

You can read more about `with/1` in our documentation.

## Running commands

The last step is to implement `KV.Command.run/1` to run the parsed commands on top of buckets. Its implementation is shown below:

```elixir
  @doc """
  Runs the given command.
  """
  def run(command, socket)

  def run({:create, bucket}, socket) do
    KV.create_bucket(bucket)
    :gen_tcp.send(socket, "OK\r\n")
    :ok
  end

  def run({:get, bucket, key}, socket) do
    lookup(bucket, fn pid ->
      value = KV.Bucket.get(pid, key)
      :gen_tcp.send(socket, "#{value}\r\nOK\r\n")
      :ok
    end)
  end

  def run({:put, bucket, key, value}, socket) do
    lookup(bucket, fn pid ->
      KV.Bucket.put(pid, key, value)
      :gen_tcp.send(socket, "OK\r\n")
      :ok
    end)
  end

  def run({:delete, bucket, key}, socket) do
    lookup(bucket, fn pid ->
      KV.Bucket.delete(pid, key)
      :gen_tcp.send(socket, "OK\r\n")
      :ok
    end)
  end

  defp lookup(bucket, callback) do
    if bucket = KV.lookup_bucket(bucket) do
      callback.(bucket)
    else
      {:error, :not_found}
    end
  end
```

Each function clause dispatches the appropriate command to the appropriate bucket.

You might have noticed we have a function head, `def run(command, socket)`, without a body. In the [Modules and Functions](../getting-started/modules-and-functions.md#default-arguments) chapter, we learned that a bodiless function can be used to declare default arguments for a multi-clause function. Here is another use case where we use a function without a body to document what the arguments are.

We have also defined a private function named `lookup/2` to help with the common functionality of looking up a bucket and returning its `pid` if it exists, `{:error, :not_found}` otherwise.

By the way, since we are now returning `{:error, :not_found}`, we should amend the `write_line/2` function in `KV.Server` to print such error as well:

```elixir
defp write_line(socket, {:error, :not_found}) do
  :gen_tcp.send(socket, "NOT FOUND\r\n")
end
```

Our server functionality is almost complete. Only tests are missing.

## Integration tests

`KV.Command.run/1`'s implementation is sending commands directly to the `KV` module, which is using a local registry to name processes. This means if we have two tests sending messages to the same bucket, our tests will conflict with each other (and likely fail). One might think this would be a reason to use mocks and other strategies to keep our tests isolated, but such techniques often make our testing environment too distant from how our code actually runs in production, and you may end-up with bugs lurking.

Luckily, there is a technique that we have been using throughout this guide that would be equally applicable here: it is ok to rely on the local registry as long as each test uses unique names. Using a combination of the test module and test name is more than enough to guarantee that.

So let's write integration tests that rely on unique names to exercise the whole stack from the TCP server to the bucket.

Create a new file at `test/kv/server_test.exs` as shown below:

```elixir
defmodule KV.ServerTest do
  use ExUnit.Case, async: true

  @socket_options [:binary, packet: :line, active: false]

  setup config do
    {:ok, socket} = :gen_tcp.connect(~c"localhost", 4040, @socket_options)
    test_name = config.test |> Atom.to_string() |> String.replace(" ", "-")
    %{socket: socket, name: "#{config.module}-#{test_name}"}
  end

  test "server interaction", %{socket: socket, name: name} do
    # CREATE
    assert send_and_recv(socket, "CREATE #{name}\r\n") == "OK\r\n"

    # PUT
    assert send_and_recv(socket, "PUT #{name} eggs 3\r\n") == "OK\r\n"

    # GET
    assert send_and_recv(socket, "GET #{name} eggs\r\n") == "3\r\n"
    assert send_and_recv(socket, "") == "OK\r\n"

    # DELETE
    assert send_and_recv(socket, "DELETE #{name} eggs\r\n") == "OK\r\n"

    # GET
    assert send_and_recv(socket, "GET #{name} eggs\r\n") == "\r\n"
    assert send_and_recv(socket, "") == "OK\r\n"
  end

  test "unknown command", %{socket: socket} do
    assert send_and_recv(socket, "WHATEVER\r\n") ==
             "UNKNOWN COMMAND\r\n"
  end

  test "unknown bucket", %{socket: socket} do
    assert send_and_recv(socket, "GET whatever eggs\r\n") ==
             "NOT FOUND\r\n"
  end

  defp send_and_recv(socket, command) do
    :ok = :gen_tcp.send(socket, command)
    {:ok, data} = :gen_tcp.recv(socket, 0, 1000)
    data
  end
end
```

Run `mix test` and the tests should all pass. However, make sure to terminate any `iex -S mix` session you may have running, as currently tests and development environment are running on the same port (4040). We will address it in the next chapter.

We added three tests, the first one tests most bucket actions, while the other two deal with error cases. Given there is a lot of shared setup across these tests, we used the `setup/2` macro to deal with common boilerplate. The macro receives the same *test context* as tests and starts a client TCP connection per test. It also defines a unique bucket name using the module name and the test name, making sure any space in the test name is replaced by `-` as to not interfere with our command parsing logic.

Then, in each test, we pattern matched on the *test context*, extracting the socket or name as necessary. This is similar to the code we wrote in `test/kv/bucket_test.exs`:

```elixir
  test "stores values by key on a named process", config do
```

Except back then we matched on all config and, this time around, we matched only on the data we needed.

Let's move to the next chapter. We will finally make our system distributed by adding a tiny bit of configuration and, *spoiler alert*, changing one line of code.
