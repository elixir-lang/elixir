Code.require_file("../test_helper.exs", __DIR__)

defmodule Code.AstCommentsTest do
  use ExUnit.Case, async: true

  def parse_string!(string) do
    Code.string_to_quoted!(string, include_comments: true, literal_encoder: &{:ok, {:__block__, &2, [&1]}}, emit_warnings: false)
  end

  describe "merge_comments/2" do
    test "merges comments in empty AST" do
      quoted =
        parse_string!("""
        # some comment
        # another comment
        """)

      assert {:__block__, meta, []} = quoted

      assert [%{line: 1, text: "# some comment"}, %{line: 2, text: "# another comment"}] =
               meta[:inner_comments]
    end

    test "merges leading comments in assorted terms" do
      quoted =
        parse_string!("""
        # leading var
        var
        # trailing var
        """)

      assert {:var, meta, _} = quoted

      assert [%{line: 1, text: "# leading var"}] = meta[:leading_comments]
      assert [%{line: 3, text: "# trailing var"}] = meta[:trailing_comments]

      quoted =
        parse_string!("""
        # leading 1
        1
        # trailing 1
        """)

      assert {:__block__, one_meta, [1]} = quoted

      assert [%{line: 1, text: "# leading 1"}] = one_meta[:leading_comments]
      assert [%{line: 3, text: "# trailing 1"}] = one_meta[:trailing_comments]

      quoted =
        parse_string!("""
        # leading qualified call
        Foo.bar(baz)
        # trailing qualified call
        """)

      assert {{:., _, [_Foo, _bar]}, meta, _} = quoted

      assert [%{line: 1, text: "# leading qualified call"}] = meta[:leading_comments]
      assert [%{line: 3, text: "# trailing qualified call"}] = meta[:trailing_comments]

      quoted =
        parse_string!("""
        # leading qualified call
        Foo.
          # leading bar
          bar(baz)
        # trailing qualified call
        """)

      assert {{:., _, [_Foo, _]}, meta,
              [
                {:baz, _, _}
              ]} = quoted

      assert [%{line: 1, text: "# leading qualified call"}, %{line: 3, text: "# leading bar"}] =
               meta[:leading_comments]

      assert [%{line: 5, text: "# trailing qualified call"}] = meta[:trailing_comments]
    end

    # Do/end blocks

    test "merges comments in do/end block" do
      quoted =
        parse_string!("""
        def a do
          foo()
          :ok
          # A
        end # B
        """)

      assert {:def, def_meta,
              [
                {:a, _, _},
                [
                  {{:__block__, _, [:do]},
                   {:__block__, _,
                    [
                      {:foo, _, _},
                      {:__block__, meta, [:ok]}
                    ]}}
                ]
              ]} =
               quoted

      assert [%{line: 4, text: "# A"}] = meta[:trailing_comments]

      assert [%{line: 5, text: "# B"}] = def_meta[:trailing_comments]
    end

    test "merges comments for named do/end blocks" do
      quoted =
        parse_string!("""
        def a do
          # leading var1
          var1
          # trailing var1
        else
          # leading var2
          var2
          # trailing var2
        catch
          # leading var3
          var3
          # trailing var3
        rescue
          # leading var4
          var4
          # trailing var4
        after
          # leading var5
          var5
          # trailing var5
        end
        """)

      assert {:def, _,
              [
                {:a, _, _},
                [
                  {{:__block__, _, [:do]}, {:var1, var1_meta, _}},
                  {{:__block__, _, [:else]}, {:var2, var2_meta, _}},
                  {{:__block__, _, [:catch]}, {:var3, var3_meta, _}},
                  {{:__block__, _, [:rescue]}, {:var4, var4_meta, _}},
                  {{:__block__, _, [:after]}, {:var5, var5_meta, _}}
                ]
              ]} =
               quoted

      assert [%{line: 2, text: "# leading var1"}] = var1_meta[:leading_comments]
      assert [%{line: 4, text: "# trailing var1"}] = var1_meta[:trailing_comments]
      assert [%{line: 6, text: "# leading var2"}] = var2_meta[:leading_comments]
      assert [%{line: 8, text: "# trailing var2"}] = var2_meta[:trailing_comments]
      assert [%{line: 10, text: "# leading var3"}] = var3_meta[:leading_comments]
      assert [%{line: 12, text: "# trailing var3"}] = var3_meta[:trailing_comments]
      assert [%{line: 14, text: "# leading var4"}] = var4_meta[:leading_comments]
      assert [%{line: 16, text: "# trailing var4"}] = var4_meta[:trailing_comments]
      assert [%{line: 18, text: "# leading var5"}] = var5_meta[:leading_comments]
      assert [%{line: 20, text: "# trailing var5"}] = var5_meta[:trailing_comments]
    end

    test "merges inner comments for empty named do/end blocks" do
      quoted =
        parse_string!("""
        def a do
          # inside do
        else
          # inside else
        catch
          # inside catch
        rescue
          # inside rescue
        after
          # inside after
        end
        """)

      assert {:def, _,
              [
                {:a, _, _},
                [
                  {{:__block__, _, [:do]}, {:__block__, do_meta, _}},
                  {{:__block__, _, [:else]}, {:__block__, else_meta, _}},
                  {{:__block__, _, [:catch]}, {:__block__, catch_meta, _}},
                  {{:__block__, _, [:rescue]}, {:__block__, rescue_meta, _}},
                  {{:__block__, _, [:after]}, {:__block__, after_meta, _}}
                ]
              ]} =
               quoted

      assert [%{line: 2, text: "# inside do"}] = do_meta[:inner_comments]
      assert [%{line: 4, text: "# inside else"}] = else_meta[:inner_comments]
      assert [%{line: 6, text: "# inside catch"}] = catch_meta[:inner_comments]
      assert [%{line: 8, text: "# inside rescue"}] = rescue_meta[:inner_comments]
      assert [%{line: 10, text: "# inside after"}] = after_meta[:inner_comments]
    end

    # Lists

    test "merges comments in list" do
      quoted =
        parse_string!("""
        [
        #leading 1
        1,
        #leading 2
        2,
        3
        #trailing 3
        ] # trailing outside
        """)
        |> IO.inspect()

      assert {:__block__, list_meta,
              [
                [
                  {:__block__, one_meta, [1]},
                  {:__block__, two_meta, [2]},
                  {:__block__, three_meta, [3]}
                ]
              ]} = quoted

      assert [%{line: 2, text: "#leading 1"}] = one_meta[:leading_comments]
      assert [%{line: 4, text: "#leading 2"}] = two_meta[:leading_comments]
      assert [%{line: 7, text: "#trailing 3"}] = three_meta[:trailing_comments]
      assert [%{line: 8, text: "# trailing outside"}] = list_meta[:trailing_comments]
    end

    test "merges inner comments in empty list" do
      quoted =
        parse_string!("""
        [
        # inner 1
        # inner 2
        ] # trailing outside
        """)

      assert {:__block__, list_meta, [[]]} = quoted

      assert [%{line: 2, text: "# inner 1"}, %{line: 3, text: "# inner 2"}] =
               list_meta[:inner_comments]

      assert [%{line: 4, text: "# trailing outside"}] = list_meta[:trailing_comments]
    end

    # Keyword lists

    test "merges comments in keyword list" do
      quoted =
        parse_string!("""
        [
        #leading a
        a: 1,
        #leading b
        b: 2,
        c: 3
        #trailing 3
        ] # trailing outside
        """)

      assert {:__block__, keyword_list_meta,
              [
                [
                  {
                    {:__block__, a_key_meta, [:a]},
                    {:__block__, _, [1]}
                  },
                  {
                    {:__block__, b_key_meta, [:b]},
                    {:__block__, _, [2]}
                  },
                  {
                    {:__block__, _, [:c]},
                    {:__block__, c_value_meta, [3]}
                  }
                ]
              ]} = quoted

      assert [%{line: 2, text: "#leading a"}] = a_key_meta[:leading_comments]
      assert [%{line: 4, text: "#leading b"}] = b_key_meta[:leading_comments]
      assert [%{line: 7, text: "#trailing 3"}] = c_value_meta[:trailing_comments]
      assert [%{line: 8, text: "# trailing outside"}] = keyword_list_meta[:trailing_comments]
    end

    test "merges comments in partial keyword list" do
      quoted =
        parse_string!("""
        [
        #leading 1
        1,
        #leading b
        b: 2
        #trailing b
        ] # trailing outside
        """)

      assert {:__block__, keyword_list_meta,
              [
                [
                  {:__block__, one_key_meta, [1]},
                  {
                    {:__block__, b_key_meta, [:b]},
                    {:__block__, b_value_meta, [2]}
                  }
                ]
              ]} = quoted

      assert [%{line: 2, text: "#leading 1"}] = one_key_meta[:leading_comments]
      assert [%{line: 4, text: "#leading b"}] = b_key_meta[:leading_comments]
      assert [%{line: 6, text: "#trailing b"}] = b_value_meta[:trailing_comments]
      assert [%{line: 7, text: "# trailing outside"}] = keyword_list_meta[:trailing_comments]
    end

    # Tuples

    test "merges comments in n-tuple" do
      quoted =
        parse_string!("""
        {
        #leading 1
        1,
        #leading 2
        2,
        3
        #trailing 3
        } # trailing outside
        """)

      assert {:{}, tuple_meta,
              [
                {:__block__, one_meta, [1]},
                {:__block__, two_meta, [2]},
                {:__block__, three_meta, [3]}
              ]} = quoted

      assert [%{line: 2, text: "#leading 1"}] = one_meta[:leading_comments]
      assert [%{line: 4, text: "#leading 2"}] = two_meta[:leading_comments]
      assert [%{line: 7, text: "#trailing 3"}] = three_meta[:trailing_comments]
      assert [%{line: 8, text: "# trailing outside"}] = tuple_meta[:trailing_comments]
    end

    test "merges comments in 2-tuple" do
      quoted =
        parse_string!("""
        {
          #leading 1
          1,
          #leading 2
          2
          #trailing 2
        } # trailing outside
        """)

      assert {:__block__, tuple_meta,
              [
                {
                  {:__block__, one_meta, [1]},
                  {:__block__, two_meta, [2]}
                }
              ]} = quoted

      assert [%{line: 2, text: "#leading 1"}] = one_meta[:leading_comments]
      assert [%{line: 4, text: "#leading 2"}] = two_meta[:leading_comments]
      assert [%{line: 6, text: "#trailing 2"}] = two_meta[:trailing_comments]
      assert [%{line: 7, text: "# trailing outside"}] = tuple_meta[:trailing_comments]
    end

    test "merges inner comments in empty tuple" do
      quoted =
        parse_string!("""
        {
        # inner 1
        # inner 2
        } # trailing outside
        """)

      assert {:{}, tuple_meta, []} = quoted

      assert [%{line: 2, text: "# inner 1"}, %{line: 3, text: "# inner 2"}] =
               tuple_meta[:inner_comments]

      assert [%{line: 4, text: "# trailing outside"}] = tuple_meta[:trailing_comments]
    end

    # Maps

    test "merges comments in maps" do
      quoted =
        parse_string!("""
        %{
        #leading 1
        1 => 1,
        #leading 2
        2 => 2,
        3 => 3
        #trailing 3
        } # trailing outside
        """)

      assert {:%{}, map_meta,
              [
                {
                  {:__block__, one_key_meta, [1]},
                  {:__block__, _, [1]}
                },
                {
                  {:__block__, two_key_meta, [2]},
                  {:__block__, _, [2]}
                },
                {
                  {:__block__, _, [3]},
                  {:__block__, three_value_meta, [3]}
                }
              ]} = quoted

      assert [%{line: 2, text: "#leading 1"}] = one_key_meta[:leading_comments]
      assert [%{line: 4, text: "#leading 2"}] = two_key_meta[:leading_comments]
      assert [%{line: 7, text: "#trailing 3"}] = three_value_meta[:trailing_comments]
      assert [%{line: 8, text: "# trailing outside"}] = map_meta[:trailing_comments]
    end

    test "merges inner comments in empty maps" do
      quoted =
        parse_string!("""
        %{
        # inner 1
        # inner 2
        } # trailing outside
        """)

      assert {:%{}, map_meta, []} = quoted

      assert [%{line: 2, text: "# inner 1"}, %{line: 3, text: "# inner 2"}] =
               map_meta[:inner_comments]

      assert [%{line: 4, text: "# trailing outside"}] = map_meta[:trailing_comments]
    end

    test "handles the presence of unquote_splicing" do
      quoted =
        parse_string!("""
        %{
          # leading baz
          :baz => :bat,
          :quux => :quuz,
          # leading unquote splicing
          unquote_splicing(foo: :bar)
          # trailing unquote splicing
        }
        """)

      assert {:%{}, _,
              [
                {{:__block__, baz_key_meta, [:baz]}, {:__block__, _, [:bat]}},
                {{:__block__, _, [:quux]}, {:__block__, _, [:quuz]}},
                {:unquote_splicing, unquote_splicing_meta, _}
              ]} = quoted

      assert [%{line: 2, text: "# leading baz"}] = baz_key_meta[:leading_comments]

      assert [%{line: 5, text: "# leading unquote splicing"}] =
               unquote_splicing_meta[:leading_comments]

      assert [%{line: 7, text: "# trailing unquote splicing"}] =
               unquote_splicing_meta[:trailing_comments]
    end

    # Structs

    test "merges comments in structs" do
      quoted =
        parse_string!("""
        %SomeStruct{
        #leading 1
        a: 1,
        #leading 2
        b: 2,
        c: 3
        #trailing 3
        } # trailing outside
        """)

      assert {:%, struct_meta,
              [
                {:__aliases__, _, [:SomeStruct]},
                {:%{}, _,
                 [
                   {{:__block__, a_key_meta, [:a]}, {:__block__, _, [1]}},
                   {{:__block__, b_key_meta, [:b]}, {:__block__, _, [2]}},
                   {{:__block__, _, [:c]}, {:__block__, c_value_meta, [3]}}
                 ]}
              ]} = quoted

      assert [%{line: 2, text: "#leading 1"}] = a_key_meta[:leading_comments]
      assert [%{line: 4, text: "#leading 2"}] = b_key_meta[:leading_comments]
      assert [%{line: 7, text: "#trailing 3"}] = c_value_meta[:trailing_comments]
      assert [%{line: 8, text: "# trailing outside"}] = struct_meta[:trailing_comments]
    end

    test "merges inner comments in structs" do
      quoted =
        parse_string!("""
        %SomeStruct{
        # inner 1
        # inner 2
        } # trailing outside
        """)

      assert {:%, struct_meta,
              [
                {:__aliases__, _, [:SomeStruct]},
                {:%{}, args_meta, []}
              ]} = quoted

      assert [%{line: 2, text: "# inner 1"}, %{line: 3, text: "# inner 2"}] =
               args_meta[:inner_comments]

      assert [%{line: 4, text: "# trailing outside"}] = struct_meta[:trailing_comments]
    end

    # Stabs (->)

    test "merges comments in anonymous function" do
      quoted =
        parse_string!("""
        fn ->
          # comment
          hello
          world
          # trailing world
        end # trailing
        """)

      assert {:fn, fn_meta,
              [
                {:->, _,
                 [
                   [],
                   {:__block__, _, [{:hello, hello_meta, _}, {:world, world_meta, _}]}
                 ]}
              ]} = quoted

      assert [%{line: 2, text: "# comment"}] = hello_meta[:leading_comments]
      assert [%{line: 5, text: "# trailing world"}] = world_meta[:trailing_comments]
      assert [%{line: 6, text: "# trailing"}] = fn_meta[:trailing_comments]
    end

    test "merges inner comments in anonymous function" do
      quoted =
        parse_string!("""
        fn ->
          # inner 1
          # inner 2
        end
        """)

      assert {:fn, _,
              [
                {:->, _,
                 [
                   [],
                   {:__block__, args_meta, [nil]}
                 ]}
              ]} = quoted

      assert [%{line: 2, text: "# inner 1"}, %{line: 3, text: "# inner 2"}] =
               args_meta[:inner_comments]
    end

    test "merges trailing comments for do/end stags" do
      quoted =
        parse_string!("""
        case foo do
          _ ->
            bar
            # trailing
        end
        """)

      assert {:case, _,
              [
                {:foo, _, _},
                [
                  {{:__block__, _, [:do]}, [{:->, _, [[{:_, _, _}], {:bar, bar_meta, _}]}]}
                ]
              ]} = quoted

      assert [%{line: 4, text: "# trailing"}] = bar_meta[:trailing_comments]
    end

    test "merges inner comments for do/end stabs" do
      quoted =
        parse_string!("""
        case foo do
          _ ->
          # inner
        end
        """)

      assert {:case, _,
              [
                {:foo, _, _},
                [
                  {{:__block__, _, [:do]},
                   [{:->, _, [[{:_, _, _}], {:__block__, args_meta, [nil]}]}]}
                ]
              ]} = quoted

      assert [%{line: 3, text: "# inner"}] = args_meta[:inner_comments]
    end

    test "merges leading and trailing comments for stabs" do
      quoted =
        parse_string!("""
          # fn
          fn
            # before head
            # middle head
            hello ->
              # after head
              # before body
              # middle body
              world
              # after body
          end
        """)

      assert {:fn, fn_meta,
              [
                {:->, _,
                 [
                   [{:hello, hello_meta, _}],
                   {:world, world_meta, _}
                 ]}
              ]} = quoted

      assert [%{line: 1, text: "# fn"}] = fn_meta[:leading_comments]

      assert [%{line: 3, text: "# before head"}, %{line: 4, text: "# middle head"}] =
               hello_meta[:leading_comments]

      assert [
               %{line: 6, text: "# after head"},
               %{line: 7, text: "# before body"},
               %{line: 8, text: "# middle body"}
             ] = world_meta[:leading_comments]

      assert [%{line: 10, text: "# after body"}] = world_meta[:trailing_comments]
    end

    test "merges leading comments into the stab if left side is empty" do
      quoted =
        parse_string!("""
        fn
          # leading
          ->
          hello
          hello
        end
        """)

      assert {:fn, _,
              [
                {:->, stab_meta,
                 [
                   [],
                   _
                 ]}
              ]} = quoted

      assert [%{line: 2, text: "# leading"}] = stab_meta[:leading_comments]
    end
  end
end
