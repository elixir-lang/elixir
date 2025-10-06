# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

Code.require_file("type_helper.exs", __DIR__)

defmodule Module.Types.IntegrationTest do
  use ExUnit.Case

  import ExUnit.CaptureIO
  import Module.Types.Descr

  defp builtin_protocols do
    [
      Collectable,
      Enumerable,
      IEx.Info,
      Inspect,
      JSON.Encoder,
      List.Chars,
      String.Chars
    ]
  end

  test "built-in protocols" do
    builtin_protocols =
      for app <- ~w[eex elixir ex_unit iex logger mix]a,
          Application.ensure_loaded(app),
          module <- Application.spec(app, :modules),
          Code.ensure_loaded(module),
          function_exported?(module, :__protocol__, 1),
          do: module

    # If this test fails, update:
    # * lib/elixir/scripts/elixir_docs.ex
    assert Enum.sort(builtin_protocols) == builtin_protocols()
  end

  setup_all do
    Application.put_env(:elixir, :ansi_enabled, false)

    on_exit(fn ->
      Application.put_env(:elixir, :ansi_enabled, true)
    end)
  end

  describe "ExCk chunk" do
    test "writes exports" do
      files = %{
        "a.ex" => """
        defmodule A do
          defp a, do: :ok
          defmacrop b, do: a()
          def c, do: b()
          defmacro d, do: b()
          @deprecated "oops"
          def e, do: :ok
        end
        """,
        "b.ex" => """
        defmodule B do
          @callback f() :: :ok
        end
        """,
        "c.ex" => """
        defmodule C do
          @macrocallback g() :: :ok
        end
        """
      }

      modules = compile_modules(files)

      assert [
               {{:c, 0}, %{}},
               {{:e, 0}, %{deprecated: "oops", sig: {:infer, _, _}}}
             ] = read_chunk(modules[A]).exports

      assert read_chunk(modules[B]).exports == [
               {{:behaviour_info, 1}, %{sig: :none}}
             ]

      assert read_chunk(modules[C]).exports == [
               {{:behaviour_info, 1}, %{sig: :none}}
             ]
    end

    test "writes exports with inferred map types" do
      files = %{
        "a.ex" => """
        defmodule A do
          defstruct [:x, :y, :z]

          def struct_create_with_atom_keys(x) do
            infer(y = %A{x: x})
            {x, y}
          end

          def map_create_with_atom_keys(x) do
            infer(%{__struct__: A, x: x, y: nil, z: nil})
            x
          end

          def map_update_with_atom_keys(x) do
            infer(%{x | y: nil})
            x
          end

          def map_update_with_unknown_keys(x, y) do
            infer(%{x | y => 123})
            x
          end

          defp infer(%A{x: <<_::binary>>, y: nil}) do
            :ok
          end
        end
        """
      }

      modules = compile_modules(files)
      exports = read_chunk(modules[A]).exports |> Map.new()

      return = fn name, arity ->
        pair = {name, arity}
        %{^pair => %{sig: {:infer, nil, [{_, return}]}}} = exports
        return
      end

      assert return.(:struct_create_with_atom_keys, 1) ==
               dynamic(
                 tuple([
                   binary(),
                   closed_map(
                     __struct__: atom([A]),
                     x: binary(),
                     y: atom([nil]),
                     z: atom([nil])
                   )
                 ])
               )

      assert return.(:map_create_with_atom_keys, 1) == dynamic(binary())

      assert return.(:map_update_with_atom_keys, 1) ==
               dynamic(
                 closed_map(
                   __struct__: atom([A]),
                   x: binary(),
                   y: term(),
                   z: term()
                 )
               )

      assert return.(:map_update_with_unknown_keys, 2) ==
               dynamic(open_map())
    end

    test "writes exports with inferred function types" do
      files = %{
        "a.ex" => """
        defmodule A do
          def captured, do: &to_capture/1
          defp to_capture(<<"ok">>), do: :ok
          defp to_capture(<<"error">>), do: :error
          defp to_capture([_ | _]), do: :list
        end
        """
      }

      modules = compile_modules(files)
      exports = read_chunk(modules[A]).exports |> Map.new()

      return = fn name, arity ->
        pair = {name, arity}
        %{^pair => %{sig: {:infer, nil, [{_, return}]}}} = exports
        return
      end

      assert return.(:captured, 0)
             |> equal?(
               fun_from_non_overlapping_clauses([
                 {[binary()], dynamic(atom([:ok, :error]))},
                 {[non_empty_list(term(), term())], dynamic(atom([:list]))}
               ])
             )
    end

    test "writes exports for implementations" do
      files = %{
        "pi.ex" => """
        defprotocol Itself do
          @fallback_to_any true
          def itself(data)
        end

        defimpl Itself,
          for: [
            Atom,
            BitString,
            Float,
            Function,
            Integer,
            List,
            Map,
            Port,
            PID,
            Reference,
            Tuple,
            Any,
            Range,
            Unknown
          ] do
          def itself(data), do: data
          def this_wont_warn(:ok), do: :ok
        end
        """
      }

      {modules, stderr} = with_io(:stderr, fn -> compile_modules(files) end)

      assert stderr =~
               "you are implementing a protocol for Unknown but said module is not available"

      refute stderr =~ "this_wont_warn"

      itself_arg = fn mod ->
        {_, %{sig: {:infer, nil, [{[value], value}]}}} =
          List.keyfind(read_chunk(modules[mod]).exports, {:itself, 1}, 0)

        value
      end

      assert itself_arg.(Itself.Atom) == dynamic(atom())
      assert itself_arg.(Itself.BitString) == dynamic(binary())
      assert itself_arg.(Itself.Float) == dynamic(float())
      assert itself_arg.(Itself.Function) == dynamic(fun())
      assert itself_arg.(Itself.Integer) == dynamic(integer())

      assert itself_arg.(Itself.List) ==
               dynamic(union(empty_list(), non_empty_list(term(), term())))

      assert itself_arg.(Itself.Map) == dynamic(open_map(__struct__: if_set(negation(atom()))))
      assert itself_arg.(Itself.Port) == dynamic(port())
      assert itself_arg.(Itself.PID) == dynamic(pid())
      assert itself_arg.(Itself.Reference) == dynamic(reference())
      assert itself_arg.(Itself.Tuple) == dynamic(tuple())
      assert itself_arg.(Itself.Any) == dynamic(term())

      assert itself_arg.(Itself.Range) ==
               dynamic(
                 closed_map(__struct__: atom([Range]), first: term(), last: term(), step: term())
               )

      assert itself_arg.(Itself.Unknown) == dynamic(open_map(__struct__: atom([Unknown])))
    end
  end

  describe "type checking" do
    test "inferred remote calls" do
      files = %{
        "a.ex" => """
        defmodule A do
          def fun(:ok), do: :doki
          def fun(:error), do: :bad
        end
        """,
        "b.ex" => """
        defmodule B do
          def badarg do
            A.fun(:unknown)
          end

          def badmatch do
            :doki = A.fun(:error)
          end
        end
        """
      }

      warnings = [
        """
            warning: incompatible types given to A.fun/1:

                A.fun(:unknown)
        """,
        """
            but expected one of:

                #1
                dynamic(:ok)

                #2
                dynamic(:error)
        """,
        """
            warning: the following pattern will never match:

                :doki = A.fun(:error)

            because the right-hand side has type:

                dynamic(:bad)
        """
      ]

      assert_warnings(files, warnings)
    end

    test "mismatched locals" do
      files = %{
        "a.ex" => """
        defmodule A do
          def error(), do: private(raise "oops")
          def public(x), do: private(List.to_tuple(x))
          defp private(:ok), do: nil
        end
        """
      }

      warnings = [
        """
            warning: incompatible types given to private/1:

                private(raise RuntimeError.exception("oops"))

        """,
        "the 1st argument is empty (often represented as none())",
        """
            typing violation found at:
            │
          2 │   def error(), do: private(raise "oops")
            │                    ~
            │
            └─ a.ex:2:20: A.error/0
        """,
        """
            warning: incompatible types given to private/1:

                private(List.to_tuple(x))
        """,
        """
            typing violation found at:
            │
          3 │   def public(x), do: private(List.to_tuple(x))
            │                      ~
            │
            └─ a.ex:3:22: A.public/1
        """
      ]

      assert_warnings(files, warnings)
    end

    test "unused private clauses" do
      files = %{
        "a.ex" => """
        defmodule A do
          def public(x) do
            private(List.to_tuple(x))
          end

          defp private(nil), do: nil
          defp private("foo"), do: "foo"
          defp private({:ok, ok}), do: ok
          defp private({:error, error}), do: error
          defp private("bar"), do: "bar"
        end
        """
      }

      warnings = [
        """
            warning: this clause of defp private/1 is never used
            │
          6 │   defp private(nil), do: nil
            │        ~
            │
            └─ a.ex:6:8: A.private/1
        """,
        """
            warning: this clause of defp private/1 is never used
            │
          7 │   defp private("foo"), do: "foo"
            │        ~
            │
            └─ a.ex:7:8: A.private/1
        """,
        """
            warning: this clause of defp private/1 is never used
            │
         10 │   defp private("bar"), do: "bar"
            │        ~
            │
            └─ a.ex:10:8: A.private/1
        """
      ]

      assert_warnings(files, warnings)
    end

    test "unused overridable private clauses" do
      files = %{
        "a.ex" => """
        defmodule A do
          use B
          def public(x), do: private(x)
          defp private(x), do: super(List.to_tuple(x))
        end
        """,
        "b.ex" => """
        defmodule B do
          defmacro __using__(_) do
            quote do
              defp private({:ok, ok}), do: ok
              defp private(:error), do: :error
              defoverridable private: 1
            end
          end
        end
        """
      }

      assert_no_warnings(files)
    end

    test "unused private clauses without warnings" do
      files = %{
        "a.ex" => """
        defmodule A do
          use B

          # Not all clauses are invoked, but do not warn since they are generated
          def public1(x), do: generated(List.to_tuple(x))

          # Avoid false positives caused by inference
          def public2(x), do: (:ok = raising_private(x))

          defp raising_private(true), do: :ok
          defp raising_private(false), do: raise "oops"
        end
        """,
        "b.ex" => """
        defmodule B do
          defmacro __using__(_) do
            quote generated: true do
              defp generated({:ok, ok}), do: ok
              defp generated(:error), do: :error
            end
          end
        end
        """
      }

      assert_no_warnings(files)
    end

    test "mismatched implementation" do
      files = %{
        "a.ex" => """
        defprotocol Itself do
          def itself(data)
        end

        defimpl Itself, for: Range do
          def itself(nil), do: nil
          def itself(range), do: range
        end
        """
      }

      warnings = [
        """
            warning: the 1st pattern in clause will never match:

                nil

            because it is expected to receive type:

                dynamic(%Range{})

            typing violation found at:
            │
          6 │   def itself(nil), do: nil
            │   ~~~~~~~~~~~~~~~~~~~~~~~~
            │
            └─ a.ex:6: Itself.Range.itself/1
        """
      ]

      assert_warnings(files, warnings)
    end

    @tag :require_ast
    test "no implementation" do
      files = %{
        "a.ex" => """
        defprotocol NoImplProtocol do
          def callback(data)
        end
        """,
        "b.ex" => """
        defmodule NoImplProtocol.Caller do
          def run do
            NoImplProtocol.callback(:hello)
          end
        end
        """
      }

      warnings = [
        """
            warning: incompatible types given to NoImplProtocol.callback/1:

                NoImplProtocol.callback(:hello)

            given types:

                -:hello-

            but the protocol was not yet implemented for any type and therefore will always fail. \
        This error typically happens within libraries that define protocols and will disappear as \
        soon as there is one implementation. If you expect the protocol to be implemented later on, \
        you can define an implementation specific for development/test.

            typing violation found at:
            │
          3 │     NoImplProtocol.callback(:hello)
            │                    ~
            │
            └─ b.ex:3:20: NoImplProtocol.Caller.run/0
        """
      ]

      assert_warnings(files, warnings, consolidate_protocols: true)
    end

    @tag :require_ast
    test "String.Chars protocol dispatch" do
      files = %{
        "a.ex" => """
        defmodule FooBar do
          def example1(_.._//_ = data), do: to_string(data)
          def example2(_.._//_ = data), do: "hello \#{data} world"
        end
        """
      }

      warnings = [
        """
            warning: incompatible value given to string interpolation:

                data

            it has type:

                -dynamic(%Range{})-

            but expected a type that implements the String.Chars protocol, it must be one of:

                dynamic(
                  %Date{} or %DateTime{} or %NaiveDateTime{} or %Time{} or %URI{} or %Version{} or
                    %Version.Requirement{}
                ) or atom() or binary() or empty_list() or float() or integer() or non_empty_list(term(), term())

            where "data" was given the type:

                # type: dynamic(%Range{})
                # from: a.ex:3:24
                _.._//_ = data

            hint: string interpolation uses the String.Chars protocol to convert a data structure into a string. Either convert the data type into a string upfront or implement the protocol accordingly
        """,
        """
            warning: incompatible types given to String.Chars.to_string/1:

                to_string(data)

            given types:

                -dynamic(%Range{})-

            but expected a type that implements the String.Chars protocol, it must be one of:

                dynamic(
                  %Date{} or %DateTime{} or %NaiveDateTime{} or %Time{} or %URI{} or %Version{} or
                    %Version.Requirement{}
                ) or atom() or binary() or empty_list() or float() or integer() or non_empty_list(term(), term())

            where "data" was given the type:

                # type: dynamic(%Range{})
                # from: a.ex:2:24
                _.._//_ = data
        """
      ]

      assert_warnings(files, warnings, consolidate_protocols: true)
    end

    @tag :require_ast
    test "Enumerable protocol dispatch" do
      files = %{
        "a.ex" => """
        defmodule FooBar do
          def example1(%Date{} = date), do: for(x <- date, do: x)
          def example2(), do: for(i <- [1, 2, 3], into: Date.utc_today(), do: i * 2)
          def example3(), do: for(i <- [1, 2, 3], into: 456, do: i * 2)
        end
        """
      }

      warnings = [
        """
            warning: incompatible value given to for-comprehension:

                x <- date

            it has type:

                -dynamic(%Date{})-

            but expected a type that implements the Enumerable protocol, it must be one of:

                dynamic(
                  %Date.Range{} or %File.Stream{} or %GenEvent.Stream{} or %HashDict{} or %HashSet{} or
                    %IO.Stream{} or %MapSet{} or %Range{} or %Stream{}
                ) or empty_list() or fun() or non_empty_list(term(), term()) or non_struct_map()

            where "date" was given the type:

                # type: dynamic(%Date{})
                # from: a.ex:2:24
                %Date{} = date

            hint: for-comprehensions use the Enumerable protocol to traverse data structures. Either convert the data type into a list (or another Enumerable) or implement the protocol accordingly
        """,
        """
            warning: incompatible value given to :into option in for-comprehension:

                into: Date.utc_today()

            it has type:
        """,
        """
            warning: incompatible value given to :into option in for-comprehension:

                into: 456

            it has type:

                -integer()-

            but expected a type that implements the Collectable protocol, it must be one of:

                dynamic(%File.Stream{} or %HashDict{} or %HashSet{} or %IO.Stream{} or %MapSet{}) or binary() or
                  empty_list() or non_empty_list(term(), term()) or non_struct_map()

            hint: the :into option in for-comprehensions use the Collectable protocol to build its result. Either pass a valid data type or implement the protocol accordingly
        """
      ]

      assert_warnings(files, warnings, consolidate_protocols: true)
    end

    test "incompatible default argument" do
      files = %{
        "a.ex" => """
        defmodule A do
          def ok(x = :ok \\\\ nil) do
            x
          end
        end
        """
      }

      warnings = [
        ~S"""
            warning: incompatible types given as default arguments to ok/1:

                -nil-

            but expected one of:

                dynamic(:ok)

            typing violation found at:
            │
          2 │   def ok(x = :ok \\ nil) do
            │                  ~
            │
            └─ a.ex:2:18: A.ok/0
        """
      ]

      assert_warnings(files, warnings)
    end

    test "returns diagnostics with source and file" do
      files = %{
        "a.ex" => """
        defmodule A do
          @file "generated.ex"
          def fun(arg) do
            :ok = List.to_tuple(arg)
          end
        end
        """
      }

      {_modules, warnings} = with_compile_warnings(files)

      assert [
               %{
                 message: "the following pattern will never match" <> _,
                 file: file,
                 source: source
               }
             ] = warnings.runtime_warnings

      assert String.ends_with?(source, "a.ex")
      assert Path.type(source) == :absolute
      assert String.ends_with?(file, "generated.ex")
      assert Path.type(file) == :absolute
    after
      purge(A)
    end

    @tag :require_ast
    test "regressions" do
      files = %{
        # do not emit false positives from defguard
        "a.ex" => """
        defmodule A do
          defguard is_non_nil_arity_function(fun, arity)
                   when arity != nil and is_function(fun, arity)

          def check(fun, args) do
            is_non_nil_arity_function(fun, length(args))
          end
        end
        """,
        # do not parse binary segments as variables
        "b.ex" => """
        defmodule B do
          def decode(byte) do
            case byte do
              enc when enc in [<<0x00>>, <<0x01>>] -> :ok
            end
          end
        end
        """,
        # String.Chars protocol dispatch on improper lists
        "c.ex" => """
        defmodule C do
          def example, do: to_string([?a, ?b | "!"])
        end
        """
      }

      assert_no_warnings(files, consolidate_protocols: true)
    end
  end

  describe "undefined warnings" do
    test "handles Erlang modules" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a, do: :not_a_module.no_module()
          def b, do: :lists.no_func()
        end
        """
      }

      warnings = [
        ":not_a_module.no_module/0 is undefined (module :not_a_module is not available or is yet to be defined)",
        "a.ex:2:28: A.a/0",
        ":lists.no_func/0 is undefined or private",
        "a.ex:3:21: A.b/0"
      ]

      assert_warnings(files, warnings)
    end

    test "handles built in functions" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a, do: Kernel.module_info()
          def b, do: Kernel.module_info(:functions)
          def c, do: Kernel.__info__(:functions)
          def d, do: GenServer.behaviour_info(:callbacks)
          def e, do: Kernel.behaviour_info(:callbacks)
        end
        """
      }

      warnings = [
        "Kernel.behaviour_info/1 is undefined or private",
        "a.ex:6:21: A.e/0"
      ]

      assert_warnings(files, warnings)
    end

    test "handles module body conditionals" do
      files = %{
        "a.ex" => """
        defmodule A do
          if function_exported?(List, :flatten, 1) do
            List.flatten([1, 2, 3])
          else
            List.old_flatten([1, 2, 3])
          end

          if function_exported?(List, :flatten, 1) do
            def flatten(arg), do: List.flatten(arg)
          else
            def flatten(arg), do: List.old_flatten(arg)
          end

          if function_exported?(List, :flatten, 1) do
            def flatten2(arg), do: List.old_flatten(arg)
          else
            def flatten2(arg), do: List.flatten(arg)
          end
        end
        """
      }

      warnings = [
        "List.old_flatten/1 is undefined or private. Did you mean:",
        "* flatten/1",
        "* flatten/2",
        "a.ex:15:33: A.flatten2/1"
      ]

      assert_warnings(files, warnings)
    end

    test "reports missing functions" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a, do: A.no_func()
          def b, do: A.a()

          @file "external_source.ex"
          def c, do: &A.no_func/1
        end
        """
      }

      warnings = [
        "A.no_func/0 is undefined or private",
        "a.ex:2:16: A.a/0",
        "A.no_func/1 is undefined or private",
        "external_source.ex:6:17: A.c/0"
      ]

      assert_warnings(files, warnings)
    end

    test "reports missing functions respecting arity" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a, do: :ok
          def b, do: A.a(1)

          @file "external_source.ex"
          def c, do: A.b(1)
        end
        """
      }

      warnings = [
        "A.a/1 is undefined or private. Did you mean:",
        "* a/0",
        "a.ex:3:16: A.b/0",
        "A.b/1 is undefined or private. Did you mean:",
        "* b/0",
        "external_source.ex:6:16: A.c/0"
      ]

      assert_warnings(files, warnings)
    end

    test "reports missing modules" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a, do: D.no_module()

          @file "external_source.ex"
          def c, do: E.no_module()

          def i, do: Io.puts "hello"
        end
        """
      }

      warnings = [
        "D.no_module/0 is undefined (module D is not available or is yet to be defined)",
        "a.ex:2:16: A.a/0",
        "E.no_module/0 is undefined (module E is not available or is yet to be defined)",
        "external_source.ex:5:16: A.c/0",
        "Io.puts/1 is undefined (module Io is not available or is yet to be defined)",
        "a.ex:7:17: A.i/0"
      ]

      assert_warnings(files, warnings)
    end

    test "reports missing captures" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a, do: &A.no_func/0

          @file "external_source.ex"
          def c, do: &A.no_func/1
        end
        """
      }

      warnings = [
        "A.no_func/0 is undefined or private",
        "a.ex:2:17: A.a/0",
        "A.no_func/1 is undefined or private",
        "external_source.ex:5:17: A.c/0"
      ]

      assert_warnings(files, warnings)
    end

    test "doesn't report missing functions at compile time" do
      files = %{
        "a.ex" => """
        Enum.map([], fn _ -> BadReferencer.no_func4() end)

        if function_exported?(List, :flatten, 1) do
          List.flatten([1, 2, 3])
        else
          List.old_flatten([1, 2, 3])
        end
        """
      }

      assert_no_warnings(files)
    end

    test "handles multiple modules in one file" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a, do: B.no_func()
          def b, do: B.a()
        end
        """,
        "b.ex" => """
        defmodule B do
          def a, do: A.no_func()
          def b, do: A.b()
        end
        """
      }

      warnings = [
        "B.no_func/0 is undefined or private",
        "a.ex:2:16: A.a/0",
        "A.no_func/0 is undefined or private",
        "b.ex:2:16: B.a/0"
      ]

      assert_warnings(files, warnings)
    end

    test "groups multiple warnings in one file" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a, do: A.no_func()

          @file "external_source.ex"
          def b, do: A2.no_func()

          def c, do: A.no_func()
          def d, do: A2.no_func()
        end
        """
      }

      warnings = [
        "A2.no_func/0 is undefined (module A2 is not available or is yet to be defined)",
        "└─ a.ex:8:17: A.d/0",
        "└─ external_source.ex:5:17: A.b/0",
        "A.no_func/0 is undefined or private",
        "└─ a.ex:2:16: A.a/0",
        "└─ a.ex:7:16: A.c/0"
      ]

      assert_warnings(files, warnings)
    end

    test "hints exclude deprecated functions" do
      files = %{
        "a.ex" => """
        defmodule A do
          def to_charlist(a), do: a

          @deprecated "Use String.to_charlist/1 instead"
          def to_char_list(a), do: a

          def c(a), do: A.to_list(a)
        end
        """
      }

      warnings = [
        "A.to_list/1 is undefined or private. Did you mean:",
        "* to_charlist/1",
        "a.ex:7:19: A.c/1"
      ]

      assert_warnings(files, warnings)
    end

    test "do not warn of module defined in local (runtime) context" do
      files = %{
        "a.ex" => """
        defmodule A do
          def a() do
            defmodule B do
              def b(), do: :ok
            end

            B.b()
          end
        end
        """
      }

      assert_no_warnings(files)
    end

    test "warn of unrequired module" do
      files = %{
        "ab.ex" => """
        defmodule A do
          def a(), do: B.b()
        end

        defmodule B do
          defmacro b(), do: :ok
        end
        """
      }

      warnings = [
        "Be sure to require B if you intend to invoke this macro",
        "ab.ex:2:18: A.a/0"
      ]

      assert_warnings(files, warnings)
    end

    test "excludes local no_warn_undefined" do
      files = %{
        "a.ex" => """
        defmodule A do
          @compile {:no_warn_undefined, [MissingModule, {MissingModule2, :func, 2}]}
          @compile {:no_warn_undefined, {B, :func, 2}}

          def a, do: MissingModule.func(1)
          def b, do: MissingModule2.func(1, 2)
          def c, do: MissingModule2.func(1)
          def d, do: MissingModule3.func(1, 2)
          def e, do: B.func(1)
          def f, do: B.func(1, 2)
          def g, do: B.func(1, 2, 3)
        end
        """,
        "b.ex" => """
        defmodule B do
          def func(_), do: :ok
        end
        """
      }

      warnings = [
        "MissingModule2.func/1 is undefined (module MissingModule2 is not available or is yet to be defined)",
        "a.ex:7:29: A.c/0",
        "MissingModule3.func/2 is undefined (module MissingModule3 is not available or is yet to be defined)",
        "a.ex:8:29: A.d/0",
        "B.func/3 is undefined or private. Did you mean:",
        "* func/1",
        "a.ex:11:16: A.g/0"
      ]

      assert_warnings(files, warnings)
    end

    test "warn of external nested module" do
      files = %{
        "a.ex" => """
        defmodule A.B do
          def a, do: :ok
        end
        defmodule A do
          alias A.B
          def a, do: B.a()
          def b, do: B.a(1)
          def c, do: B.no_func()
        end
        """
      }

      warnings = [
        "A.B.a/1 is undefined or private. Did you mean:",
        "* a/0",
        " def b, do: B.a(1)",
        "a.ex:7:16: A.b/0",
        "A.B.no_func/0 is undefined or private",
        "def c, do: B.no_func()",
        "a.ex:8:16: A.c/0"
      ]

      assert_warnings(files, warnings)
    end

    test "warn of compile time context module defined before calls" do
      files = %{
        "a.ex" => """
        defmodule A do
          defmodule B do
            def a, do: :ok
          end
          def a, do: B.a()
          def b, do: B.a(1)
          def c, do: B.no_func()
        end
        """
      }

      warnings = [
        "A.B.a/1 is undefined or private. Did you mean:",
        "* a/0",
        " def b, do: B.a(1)",
        "a.ex:6:16: A.b/0",
        "A.B.no_func/0 is undefined or private",
        "def c, do: B.no_func()",
        "a.ex:7:16: A.c/0"
      ]

      assert_warnings(files, warnings)
    end

    test "warn of compile time context module defined after calls and aliased" do
      files = %{
        "a.ex" => """
        defmodule A do
          alias A.B
          def a, do: B.a()
          def b, do: B.a(1)
          def c, do: B.no_func()
          defmodule B do
            def a, do: :ok
          end
        end
        """
      }

      warnings = [
        "A.B.a/1 is undefined or private. Did you mean:",
        "* a/0",
        " def b, do: B.a(1)",
        "a.ex:4:16: A.b/0",
        "A.B.no_func/0 is undefined or private",
        "def c, do: B.no_func()",
        "a.ex:5:16: A.c/0"
      ]

      assert_warnings(files, warnings)
    end

    test "excludes global no_warn_undefined" do
      no_warn_undefined = Code.get_compiler_option(:no_warn_undefined)

      try do
        Code.compiler_options(
          no_warn_undefined: [MissingModule, {MissingModule2, :func, 2}, {B, :func, 2}]
        )

        files = %{
          "a.ex" => """
          defmodule A do
            @compile {:no_warn_undefined, [MissingModule, {MissingModule2, :func, 2}]}
            @compile {:no_warn_undefined, {B, :func, 2}}

            def a, do: MissingModule.func(1)
            def b, do: MissingModule2.func(1, 2)
            def c, do: MissingModule2.func(1)
            def d, do: MissingModule3.func(1, 2)
            def e, do: B.func(1)
            def f, do: B.func(1, 2)
            def g, do: B.func(1, 2, 3)
          end
          """,
          "b.ex" => """
          defmodule B do
            def func(_), do: :ok
          end
          """
        }

        warnings = [
          "MissingModule2.func/1 is undefined (module MissingModule2 is not available or is yet to be defined)",
          "a.ex:7:29: A.c/0",
          "MissingModule3.func/2 is undefined (module MissingModule3 is not available or is yet to be defined)",
          "a.ex:8:29: A.d/0",
          "B.func/3 is undefined or private. Did you mean:",
          "* func/1",
          "a.ex:11:16: A.g/0"
        ]

        assert_warnings(files, warnings)
      after
        Code.compiler_options(no_warn_undefined: no_warn_undefined)
      end
    end

    test "global no_warn_undefined :all" do
      no_warn_undefined = Code.get_compiler_option(:no_warn_undefined)

      try do
        Code.compiler_options(no_warn_undefined: :all)

        files = %{
          "a.ex" => """
          defmodule A do
            def a, do: MissingModule.func(1)
          end
          """
        }

        assert_no_warnings(files)
      after
        Code.compiler_options(no_warn_undefined: no_warn_undefined)
      end
    end

    test "global no_warn_undefined :all and local exclude" do
      no_warn_undefined = Code.get_compiler_option(:no_warn_undefined)

      try do
        Code.compiler_options(no_warn_undefined: :all)

        files = %{
          "a.ex" => """
          defmodule A do
            @compile {:no_warn_undefined, MissingModule}

            def a, do: MissingModule.func(1)
            def b, do: MissingModule2.func(1, 2)
          end
          """
        }

        assert_no_warnings(files)
      after
        Code.compiler_options(no_warn_undefined: no_warn_undefined)
      end
    end
  end

  describe "after_verify" do
    test "reports functions" do
      files = %{
        "a.ex" => """
        defmodule A do
          @after_verify __MODULE__

          def __after_verify__(__MODULE__) do
            IO.warn "from after_verify", []
          end
        end
        """
      }

      warning = [
        "warning: ",
        "from after_verify"
      ]

      assert_warnings(files, warning)
    end
  end

  describe "deprecated" do
    test "reports functions" do
      files = %{
        "a.ex" => """
        defmodule A do
          @deprecated "oops"
          def a, do: A.a()
        end
        """
      }

      warnings = [
        "A.a/0 is deprecated. oops",
        "a.ex:3:16: A.a/0"
      ]

      assert_warnings(files, warnings)
    end

    test "reports imported functions" do
      files = %{
        "a.ex" => """
        defmodule A do
          @deprecated "oops"
          def a, do: :ok
        end
        """,
        "b.ex" => """
        defmodule B do
          import A
          def b, do: a()
        end
        """
      }

      warnings = [
        "A.a/0 is deprecated. oops",
        "b.ex:3:14: B.b/0"
      ]

      assert_warnings(files, warnings)
    end

    test "reports structs" do
      files = %{
        "a.ex" => """
        defmodule A do
          @deprecated "oops"
          defstruct [:x, :y]
          def match(%A{}), do: :ok
          def build(:ok), do: %A{}
        end
        """,
        "b.ex" => """
        defmodule B do
          def match(%A{}), do: :ok
          def build(:ok), do: %A{}
        end
        """
      }

      warnings = [
        "A.__struct__/0 is deprecated. oops",
        "└─ a.ex:4:13: A.match/1",
        "└─ a.ex:5:23: A.build/1",
        "A.__struct__/0 is deprecated. oops",
        "└─ b.ex:2:13: B.match/1",
        "└─ b.ex:3:23: B.build/1"
      ]

      assert_warnings(files, warnings)
    end

    test "reports module body" do
      files = %{
        "a.ex" => """
        defmodule A do
          @deprecated "oops"
          def a, do: :ok
        end
        """,
        "b.ex" => """
        defmodule B do
          require A
          A.a()
        end
        """
      }

      warnings = [
        "A.a/0 is deprecated. oops",
        "b.ex:3:5: B (module)"
      ]

      assert_warnings(files, warnings)
    end

    test "reports macro" do
      files = %{
        "a.ex" => """
        defmodule A do
          @deprecated "oops"
          defmacro a, do: :ok
        end
        """,
        "b.ex" => """
        defmodule B do
          require A
          def b, do: A.a()
        end
        """
      }

      warnings = [
        "A.a/0 is deprecated. oops",
        "b.ex:3:16: B.b/0"
      ]

      assert_warnings(files, warnings)
    end

    test "reports unquote functions" do
      files = %{
        "a.ex" => """
        defmodule A do
          @deprecated "oops"
          def a, do: :ok
        end
        """,
        "b.ex" => """
        defmodule B do
          def b, do: unquote(&A.a/0)
        end
        """
      }

      warnings = [
        "A.a/0 is deprecated. oops",
        "b.ex: B.b/0"
      ]

      assert_warnings(files, warnings)
    end
  end

  defp assert_warnings(files, expected, opts \\ [])

  defp assert_warnings(files, expected, opts) when is_binary(expected) do
    assert capture_compile_warnings(files, opts) == expected
  end

  defp assert_warnings(files, expecteds, opts) when is_list(expecteds) do
    output = capture_compile_warnings(files, opts)

    Enum.each(expecteds, fn expected ->
      assert output =~ expected
    end)
  end

  defp assert_no_warnings(files, opts \\ []) do
    assert capture_compile_warnings(files, opts) == ""
  end

  defp capture_compile_warnings(files, opts) do
    in_tmp(fn ->
      paths = generate_files(files)
      capture_io(:stderr, fn -> compile_to_path(paths, opts) end)
    end)
  end

  defp with_compile_warnings(files) do
    in_tmp(fn ->
      paths = generate_files(files)
      with_io(:stderr, fn -> compile_to_path(paths, []) end) |> elem(0)
    end)
  end

  defp compile_modules(files) do
    in_tmp(fn ->
      paths = generate_files(files)
      {modules, _warnings} = compile_to_path(paths, [])

      Map.new(modules, fn module ->
        {^module, binary, _filename} = :code.get_object_code(module)
        {module, binary}
      end)
    end)
  end

  defp compile_to_path(paths, opts) do
    if opts[:consolidate_protocols] do
      Code.prepend_path(".")

      result =
        compile_to_path_with_after_compile(paths, fn ->
          if Keyword.get(opts, :consolidate_protocols, false) do
            paths = [".", Application.app_dir(:elixir, "ebin")]
            protocols = Protocol.extract_protocols(paths)

            for protocol <- protocols do
              impls = Protocol.extract_impls(protocol, paths)
              {:ok, binary} = Protocol.consolidate(protocol, impls)
              File.write!(Atom.to_string(protocol) <> ".beam", binary)
              purge(protocol)
            end
          end
        end)

      Code.delete_path(".")
      Enum.each(builtin_protocols(), &purge/1)

      result
    else
      compile_to_path_with_after_compile(paths, fn -> :ok end)
    end
  end

  defp compile_to_path_with_after_compile(paths, callback) do
    {:ok, modules, warnings} =
      Kernel.ParallelCompiler.compile_to_path(paths, ".",
        return_diagnostics: true,
        after_compile: callback
      )

    for module <- modules do
      purge(module)
    end

    {modules, warnings}
  end

  defp generate_files(files) do
    for {file, contents} <- files do
      File.write!(file, contents)
      file
    end
  end

  defp read_chunk(binary) do
    assert {:ok, {_module, [{~c"ExCk", chunk}]}} = :beam_lib.chunks(binary, [~c"ExCk"])
    assert {:elixir_checker_v3, map} = :erlang.binary_to_term(chunk)
    map
  end

  defp purge(mod) do
    :code.delete(mod)
    :code.purge(mod)
  end

  defp in_tmp(fun) do
    path = PathHelpers.tmp_path("checker")

    File.rm_rf!(path)
    File.mkdir_p!(path)
    File.cd!(path, fun)
  end
end
