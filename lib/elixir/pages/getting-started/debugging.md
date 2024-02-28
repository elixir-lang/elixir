# Debugging

There are a number of ways to debug code in Elixir. In this chapter we will cover some of the more common ways of doing so.

## IO.inspect/2

What makes `IO.inspect(item, opts \\ [])` really useful in debugging is that it returns the `item` argument passed to it without affecting the behavior of the original code. Let's see an example.

```elixir
(1..10)
|> IO.inspect()
|> Enum.map(fn x -> x * 2 end)
|> IO.inspect()
|> Enum.sum()
|> IO.inspect()
```

Prints:

```elixir
1..10
[2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
110
```

As you can see `IO.inspect/2` makes it possible to "spy" on values almost anywhere in your code without altering the result, making it very helpful inside of a pipeline like in the above case.

`IO.inspect/2` also provides the ability to decorate the output with a `label` option. The label will be printed before the inspected `item`:

```elixir
[1, 2, 3]
|> IO.inspect(label: "before")
|> Enum.map(&(&1 * 2))
|> IO.inspect(label: "after")
|> Enum.sum
```

Prints:

```elixir
before: [1, 2, 3]
after: [2, 4, 6]
```

It is also very common to use `IO.inspect/2` with `binding/0`, which returns all variable names and their values:

```elixir
def some_fun(a, b, c) do
  IO.inspect binding()
  ...
end
```

When `some_fun/3` is invoked with `:foo`, `"bar"`, `:baz` it prints:

```elixir
[a: :foo, b: "bar", c: :baz]
```

See `IO.inspect/2` and `Inspect.Opts` respectively to learn more about the function and read about all supported options.

## dbg/2

Elixir v1.14 introduced `dbg/2`. `dbg` is similar to `IO.inspect/2` but specifically tailored for debugging. It prints the value passed to it and returns it (just like `IO.inspect/2`), but it also prints the code and location.

```elixir
# In my_file.exs
feature = %{name: :dbg, inspiration: "Rust"}
dbg(feature)
dbg(Map.put(feature, :in_version, "1.14.0"))
```

The code above prints this:

```shell
[my_file.exs:2: (file)]
feature #=> %{inspiration: "Rust", name: :dbg}
[my_file.exs:3: (file)]
Map.put(feature, :in_version, "1.14.0") #=> %{in_version: "1.14.0", inspiration: "Rust", name: :dbg}
```

When talking about `IO.inspect/2`, we mentioned its usefulness when placed between steps of `|>` pipelines. `dbg` does it better: it understands Elixir code, so it will print values at *every step of the pipeline*.

```elixir
# In dbg_pipes.exs
__ENV__.file
|> String.split("/", trim: true)
|> List.last()
|> File.exists?()
|> dbg()
```

This code prints:

```shell
[dbg_pipes.exs:5: (file)]
__ENV__.file #=> "/home/myuser/dbg_pipes.exs"
|> String.split("/", trim: true) #=> ["home", "myuser", "dbg_pipes.exs"]
|> List.last() #=> "dbg_pipes.exs"
|> File.exists?() #=> true
```

While `dbg` provides conveniences around Elixir constructs, you will need `IEx` if you want to execute code and set breakpoints while debugging.

## Breakpoints

When using `IEx`, you may pass `--dbg pry` as an option to "stop" the code execution where the `dbg` call is:

```console
$ iex --dbg pry
```

Or to debug inside a of a project:

```console
$ iex --dbg pry -S mix
```

Or during tests (the `--trace` flag on `mix test` prevents tests from timing out):

```console
$ iex --dbg pry -S mix test --trace
$ iex --dbg pry -S mix test path/to/file:line --trace
```

Now a call to `dbg` will ask if you want to pry the existing code. If you accept, you'll be able to access all variables, as well as imports and aliases from the code, directly from IEx. This is called "prying". While the pry session is running, the code execution stops, until `continue` or `next` are called. Remember you can always run `iex` in the context of a project with `iex -S mix TASK`.

<script id="asciicast-509509" src="https://asciinema.org/a/509509.js" async></script>

`dbg` calls require us to change the code we intend to debug and has limited stepping functionality. Luckily IEx also provides a `IEx.break!/2` function which allows you to set and manage breakpoints on any Elixir code without modifying its source:

<script type="text/javascript" src="https://asciinema.org/a/0h3po0AmTcBAorc5GBNU97nrs.js" id="asciicast-0h3po0AmTcBAorc5GBNU97nrs" async></script><noscript><p><a href="https://asciinema.org/a/0h3po0AmTcBAorc5GBNU97nrs">See the example in asciinema</a></p></noscript>

Similar to `dbg`, once a breakpoint is reached code execution stops until `continue` or `next` are invoked. However, `break!/2` does not have access to aliases and imports from the debugged code as it works on the compiled artifact rather than on source code.

## Observer

For debugging complex systems, jumping at the code is not enough. It is necessary to have an understanding of the whole virtual machine, processes, applications, as well as set up tracing mechanisms. Luckily this can be achieved in Erlang with `:observer`. In your application:

```elixir
$ iex
iex> :observer.start()
```

> #### Missing dependencies {: .warning}
>
> When running `iex` inside a project with `iex -S mix`, `observer` won't be available as a dependency. To do so, you will need to call the following functions before:
>
> ```elixir
> iex> Mix.ensure_application!(:wx)
> iex> Mix.ensure_application!(:runtime_tools)
> iex> Mix.ensure_application!(:observer)
> iex> :observer.start()
> ```
>
> If any of the calls above fail, here is what may have happened: some package managers default to installing a minimized Erlang without WX bindings for GUI support. In some package managers, you may be able to replace the headless Erlang with a more complete package (look for packages named `erlang` vs `erlang-nox` on Debian/Ubuntu/Arch). In others managers, you may need to install a separate `erlang-wx` (or similarly named) package.
>
> There are conversations to improve this experience in future releases.

The above will open another Graphical User Interface that provides many panes to fully understand and navigate the runtime and your project.

We explore the Observer in the context of an actual project [in the Dynamic Supervisor chapter of the Mix & OTP guide](../mix-and-otp/dynamic-supervisor.md). This is one of the debugging techniques [the Phoenix framework used to achieve 2 million connections on a single machine](https://phoenixframework.org/blog/the-road-to-2-million-websocket-connections).

If you are using the Phoenix web framework, it ships with the [Phoenix LiveDashboard](https://github.com/phoenixframework/phoenix_live_dashboard), a web dashboard for production nodes which provides similar features to Observer.

Finally, remember you can also get a mini-overview of the runtime info by calling `runtime_info/0` directly in IEx.

## Other tools and community

We have just scratched the surface of what the Erlang VM has to offer, for example:

  * Alongside the observer application, Erlang also includes a [`:crashdump_viewer`](https://www.erlang.org/doc/man/crashdump_viewer.html) to view crash dumps

  * Integration with OS level tracers, such as [Linux Trace Toolkit,](https://www.erlang.org/doc/apps/runtime_tools/lttng) [DTRACE,](https://www.erlang.org/doc/apps/runtime_tools/dtrace) and [SystemTap](https://www.erlang.org/doc/apps/runtime_tools/systemtap)

  * [Microstate accounting](http://www.erlang.org/doc/man/msacc.html) measures how much time the runtime spends in several low-level tasks in a short time interval

  * Mix ships with many tasks under the `profile` namespace, such as `mix profile.cprof` and `mix profile.fprof`

  * For more advanced use cases, we recommend the excellent [Erlang in Anger](https://www.erlang-in-anger.com/), which is available as a free ebook

Happy debugging!
