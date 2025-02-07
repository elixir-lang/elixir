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

After the parsing is done, we will update our server to dispatch the parsed commands to the `:kv` application we built previously.

## Doctests

On the language homepage, we mention that Elixir makes documentation a first-class citizen in the language. We have explored this concept many times throughout this guide, be it via `mix help` or by typing `h Enum` or another module in an IEx console.

In this section, we will implement the parsing functionality, document it and make sure our documentation is up to date with doctests. This helps us provide documentation with accurate code samples.

Let's create our command parser at `lib/kv_server/command.ex` and start with the doctest:

```elixir
defmodule KVServer.Command do
  @doc ~S"""
  Parses the given `line` into a command.

  ## Examples

      iex> KVServer.Command.parse("CREATE shopping\r\n")
      {:ok, {:create, "shopping"}}

  """
  def parse(_line) do
    :not_implemented
  end
end
```

Doctests are specified by an indentation of four spaces followed by the `iex>` prompt in a documentation string. If a command spans multiple lines, you can use `...>`, as in IEx. The expected result should start at the next line after `iex>` or `...>` line(s) and is terminated either by a newline or a new `iex>` prefix.

Also, note that we started the documentation string using `@doc ~S"""`. The `~S` prevents the `\r\n` characters from being converted to a carriage return and line feed until they are evaluated in the test.

To run our doctests, we'll create a file at `test/kv_server/command_test.exs` and call `doctest KVServer.Command` in the test case:

```elixir
defmodule KVServer.CommandTest do
  use ExUnit.Case, async: true
  doctest KVServer.Command
end
```

Run the test suite and the doctest should fail:

```text
  1) doctest KVServer.Command.parse/1 (1) (KVServer.CommandTest)
     test/kv_server/command_test.exs:3
     Doctest failed
     doctest:
       iex> KVServer.Command.parse("CREATE shopping\r\n")
       {:ok, {:create, "shopping"}}
     code: KVServer.Command.parse "CREATE shopping\r\n" === {:ok, {:create, "shopping"}}
     left:  :not_implemented
     right: {:ok, {:create, "shopping"}}
     stacktrace:
       lib/kv_server/command.ex:7: KVServer.Command (module)
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

    iex> KVServer.Command.parse "CREATE shopping\r\n"
    {:ok, {:create, "shopping"}}

    iex> KVServer.Command.parse "CREATE  shopping  \r\n"
    {:ok, {:create, "shopping"}}

    iex> KVServer.Command.parse "PUT shopping milk 1\r\n"
    {:ok, {:put, "shopping", "milk", "1"}}

    iex> KVServer.Command.parse "GET shopping milk\r\n"
    {:ok, {:get, "shopping", "milk"}}

    iex> KVServer.Command.parse "DELETE shopping eggs\r\n"
    {:ok, {:delete, "shopping", "eggs"}}

Unknown commands or commands with the wrong number of
arguments return an error:

    iex> KVServer.Command.parse "UNKNOWN shopping eggs\r\n"
    {:error, :unknown_command}

    iex> KVServer.Command.parse "GET shopping\r\n"
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
iex> KVServer.Command.parse("UNKNOWN shopping eggs\r\n")
{:error, :unknown_command}

iex> KVServer.Command.parse("GET shopping\r\n")
{:error, :unknown_command}
```

Without new lines, as seen below, ExUnit compiles it into a single doctest:

```elixir
iex> KVServer.Command.parse("UNKNOWN shopping eggs\r\n")
{:error, :unknown_command}
iex> KVServer.Command.parse("GET shopping\r\n")
{:error, :unknown_command}
```

As the name says, doctest is documentation first and a test later. Their goal is not to replace tests but to provide up-to-date documentation. You can read more about doctests in the `ExUnit.DocTest` documentation.

## `with`

As we are now able to parse commands, we can finally start implementing the logic that runs the commands. Let's add a stub definition for this function for now:

```elixir
defmodule KVServer.Command do
  @doc """
  Runs the given command.
  """
  def run(command) do
    {:ok, "OK\r\n"}
  end
end
```

Before we implement this function, let's change our server to start using our new `parse/1` and `run/1` functions. Remember, our `read_line/1` function was also crashing when the client closed the socket, so let's take the opportunity to fix it, too. Open up `lib/kv_server.ex` and replace the existing server definition:

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
        case KVServer.Command.parse(data) do
          {:ok, command} ->
            KVServer.Command.run(command)
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

defp write_line(socket, {:ok, text}) do
  :gen_tcp.send(socket, text)
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

Thankfully, Elixir v1.2 introduced the `with` construct, which allows you to simplify code like the above, replacing nested `case` calls with a chain of matching clauses. Let's rewrite the `serve/1` function to use `with`:

```elixir
defp serve(socket) do
  msg =
    with {:ok, data} <- read_line(socket),
         {:ok, command} <- KVServer.Command.parse(data),
         do: KVServer.Command.run(command)

  write_line(socket, msg)
  serve(socket)
end
```

Much better! `with` will retrieve the value returned by the right-side of `<-` and match it against the pattern on the left side. If the value matches the pattern, `with` moves on to the next expression. In case there is no match, the non-matching value is returned.

In other words, we converted each expression given to `case/2` as a step in `with`. As soon as any of the steps return something that does not match `{:ok, x}`, `with` aborts, and returns the non-matching value.

You can read more about `with/1` in our documentation.

## Running commands

The last step is to implement `KVServer.Command.run/1`, to run the parsed commands against the `:kv` application. Its implementation is shown below:

```elixir
@doc """
Runs the given command.
"""
def run(command)

def run({:create, bucket}) do
  KV.Registry.create(KV.Registry, bucket)
  {:ok, "OK\r\n"}
end

def run({:get, bucket, key}) do
  lookup(bucket, fn pid ->
    value = KV.Bucket.get(pid, key)
    {:ok, "#{value}\r\nOK\r\n"}
  end)
end

def run({:put, bucket, key, value}) do
  lookup(bucket, fn pid ->
    KV.Bucket.put(pid, key, value)
    {:ok, "OK\r\n"}
  end)
end

def run({:delete, bucket, key}) do
  lookup(bucket, fn pid ->
    KV.Bucket.delete(pid, key)
    {:ok, "OK\r\n"}
  end)
end

defp lookup(bucket, callback) do
  case KV.Registry.lookup(KV.Registry, bucket) do
    {:ok, pid} -> callback.(pid)
    :error -> {:error, :not_found}
  end
end
```

Every function clause dispatches the appropriate command to the `KV.Registry` server that we registered during the `:kv` application startup. Since our `:kv_server` depends on the `:kv` application, it is completely fine to depend on the services it provides.

You might have noticed we have a function head, `def run(command)`, without a body. In the [Modules and Functions](../getting-started/modules-and-functions.md#default-arguments) chapter, we learned that a bodiless function can be used to declare default arguments for a multi-clause function. Here is another use case where we use a function without a body to document what the arguments are.

Note that we have also defined a private function named `lookup/2` to help with the common functionality of looking up a bucket and returning its `pid` if it exists, `{:error, :not_found}` otherwise.

By the way, since we are now returning `{:error, :not_found}`, we should amend the `write_line/2` function in `KVServer` to print such error as well:

```elixir
defp write_line(socket, {:error, :not_found}) do
  :gen_tcp.send(socket, "NOT FOUND\r\n")
end
```

Our server functionality is almost complete. Only tests are missing. This time, we have left tests for last because there are some important considerations to be made.

`KVServer.Command.run/1`'s implementation is sending commands directly to the server named `KV.Registry`, which is registered by the `:kv` application. This means this server is global and if we have two tests sending messages to it at the same time, our tests will conflict with each other (and likely fail). We need to decide between having unit tests that are isolated and can run asynchronously, or writing integration tests that work on top of the global state, but exercise our application's full stack as it is meant to be exercised in production.

So far we have only written unit tests, typically testing a single module directly. However, in order to make `KVServer.Command.run/1` testable as a unit we would need to change its implementation to not send commands directly to the `KV.Registry` process but instead pass a server as an argument. For example, we would need to change `run`'s signature to `def run(command, pid)` and then change all clauses accordingly:

```elixir
def run({:create, bucket}, pid) do
  KV.Registry.create(pid, bucket)
  {:ok, "OK\r\n"}
end

# ... other run clauses ...
```

Feel free to go ahead and do the changes above and write some unit tests. The idea is that your tests will start an instance of the `KV.Registry` and pass it as an argument to `run/2` instead of relying on the global `KV.Registry`. This has the advantage of keeping our tests asynchronous as there is no shared state.

But let's also try something different. Let's write integration tests that rely on the global server names to exercise the whole stack from the TCP server to the bucket. Our integration tests will rely on global state and must be synchronous. With integration tests, we get coverage on how the components in our application work together at the cost of test performance. They are typically used to test the main flows in your application. For example, we should avoid using integration tests to test an edge case in our command parsing implementation.

Our integration test will use a TCP client that sends commands to our server and assert we are getting the desired responses.

Let's implement the integration test in `test/kv_server_test.exs` as shown below:

```elixir
defmodule KVServerTest do
  use ExUnit.Case

  setup do
    Application.stop(:kv)
    :ok = Application.start(:kv)
  end

  setup do
    opts = [:binary, packet: :line, active: false]
    {:ok, socket} = :gen_tcp.connect(~c"localhost", 4040, opts)
    %{socket: socket}
  end

  test "server interaction", %{socket: socket} do
    assert send_and_recv(socket, "UNKNOWN shopping\r\n") ==
           "UNKNOWN COMMAND\r\n"

    assert send_and_recv(socket, "GET shopping eggs\r\n") ==
           "NOT FOUND\r\n"

    assert send_and_recv(socket, "CREATE shopping\r\n") ==
           "OK\r\n"

    assert send_and_recv(socket, "PUT shopping eggs 3\r\n") ==
           "OK\r\n"

    # GET returns two lines
    assert send_and_recv(socket, "GET shopping eggs\r\n") == "3\r\n"
    assert send_and_recv(socket, "") == "OK\r\n"

    assert send_and_recv(socket, "DELETE shopping eggs\r\n") ==
           "OK\r\n"

    # GET returns two lines
    assert send_and_recv(socket, "GET shopping eggs\r\n") == "\r\n"
    assert send_and_recv(socket, "") == "OK\r\n"
  end

  defp send_and_recv(socket, command) do
    :ok = :gen_tcp.send(socket, command)
    {:ok, data} = :gen_tcp.recv(socket, 0, 1000)
    data
  end
end
```

Our integration test checks all server interaction, including unknown commands and not found errors. It is worth noting that, as with ETS tables and linked processes, there is no need to close the socket. Once the test process exits, the socket is automatically closed.

This time, since our test relies on global data, we have not given `async: true` to `use ExUnit.Case`. Furthermore, in order to guarantee our test is always in a clean state, we stop and start the `:kv` application before each test. In fact, stopping the `:kv` application even prints a warning on the terminal:

```text
18:12:10.698 [info] Application kv exited: :stopped
```

To avoid printing log messages during tests, ExUnit provides a neat feature called `:capture_log`. By setting `@tag :capture_log` before each test or `@moduletag :capture_log` for the whole test module, ExUnit will automatically capture anything that is logged while the test runs. In case our test fails, the captured logs will be printed alongside the ExUnit report.

Between `use ExUnit.Case` and `setup`, add the following call:

```elixir
@moduletag :capture_log
```

In case the test crashes, you will see a report as follows:

```text
  1) test server interaction (KVServerTest)
     test/kv_server_test.exs:17
     ** (RuntimeError) oops
     stacktrace:
       test/kv_server_test.exs:29

     The following output was logged:

     13:44:10.035 [notice] Application kv exited: :stopped
```

With this simple integration test, we start to see why integration tests may be slow. Not only can this test not run asynchronously, but it also requires the expensive setup of stopping and starting the `:kv` application.

At the end of the day, it is up to you and your team to figure out the best testing strategy for your applications. You need to balance code quality, confidence, and test suite runtime. For example, we may start with testing the server only with integration tests, but if the server continues to grow in future releases, or it becomes a part of the application with frequent bugs, it is important to consider breaking it apart and writing more intensive unit tests that don't have the weight of an integration test.

Let's move to the next chapter. We will finally make our system distributed by adding a bucket routing mechanism. We will use this opportunity to also improve our testing chops.
