Code.require_file "../test_helper", __FILE__

EEx.setup

module EExTest
  mixin ExUnit::Case

  module ParserTest
    mixin ExUnit::Case

    def equal_end_test
      1 = invoke("<%= foo %>").size
      1 = invoke("<%= 1 +\n2 %>").size

      [_,equal_token,_] = invoke("foo\n<%= 1 +\n2 %>bar")
      { 'start_end, 2, '"=", $" 1 +\n2 " } = equal_token
    end

    def badsyntax_test
      try
        invoke("<% foo")
      catch 'error:{'badsyntax, _}
      end
    end

    private

    def invoke(string)
      EEx::Parser.string(string, "nofile")
    end
  end

  module EngineTest
    mixin ExUnit::Case

    def sum1(pre, fun_a)
      pre.to_s + fun_a.() + "baz"
    end

    def sum2(fun_a, fun_b)
      fun_a.() + fun_b.() + "baz"
    end

    def equal_end_test
      "3" = invoke("<%= 1 +\n2 %>")
      "foo\n3\nbar" = invoke("foo\n<%= 1 +\n2 %>\nbar")
    end

    def mark_test
      "foo\nbar\nbaz" = invoke("<%= EExTest::EngineTest.sum1 \"foo\", do ?>\nbar\n<? end %>")
      "foo\nbar\nbaz" = invoke("<%= EExTest::EngineTest.sum1 \"foo\", do ?>\n<%= 'bar %>\n<? end %>")
    end

    def nested_mark_test
      "\nfoo\n\nbar\nbaz" = invoke("<%= EExTest::EngineTest.sum2 do ?>\nfoo\n<? end, do ?>\n<%= 'bar %>\n<? end %>")
    end

    def escape_test
      "foo<% bar %>baz" = invoke("foo<%% bar %>baz")
      "foo<% bar ?>baz" = invoke("foo<%% bar ?>baz")
      "foo<? bar %>baz" = invoke("foo<%? bar %>baz")
      "foo<? bar ?>baz" = invoke("foo<%? bar ?>baz")
      "foo<%# bar ?>baz" = invoke("foo<%%# bar ?>baz")
      "foo<%= bar ?>baz" = invoke("foo<%%= bar ?>baz")
      "foo<%== bar ?>baz" = invoke("foo<%%== bar ?>baz")
    end

    def comment_test
      "foobaz" = invoke("foo<%# bar %>baz")
    end

    def line_numbers_test
      "foo\n2\n3\nbaz" = invoke("foo\n<%= EExTest::EngineTest.sum1 __LINE__, do ?>\n<%= __LINE__ %>\n<? end %>")
      "foo1\n\n4baz" = invoke("foo<%= __LINE__ %>\n<%# bar\nbar %>\n<%= __LINE__ %>baz")
      "foo1\n<% bar\nbar %>\n4baz" = invoke("foo<%= __LINE__ %>\n<%% bar\nbar %>\n<%= __LINE__ %>baz")
    end

    def invalid_test
      try
        invoke("foo<? 'bar %>baz")
      catch 'error:{'badsyntax, {1,"nofile","illegal","<?"}}
      end
    end

    private

    def invoke(string)
      Module.eval EEx::Compiler.string(EEx::Engine, string, "nofile")
    end
  end

  module BehaviorTest
    mixin ExUnit::Case

    def string_readers_test
      behavior = EEx.string("foo")
      "nofile" = behavior.filename
      1 = behavior.line
      assert_included "<<0:0", behavior.compiled
    end

    def file_readers_test
      behavior = EEx.file File.expand_path("../fixtures/template.eex", __FILE__)
      assert_included "fixtures/template.eex", behavior.filename
      1 = behavior.line
      assert_included "<<0:0", behavior.compiled
    end

    def render_test
      behavior = EEx.file File.expand_path("../fixtures/template.eex", __FILE__)
      "foo\nbar\n\nbaz\n" = behavior.render 'self: self, 'x: 2
    end

    % Function called from the rendered template
    def some_func(x, fun)
      2 = x
      fun.()
    end
  end
end