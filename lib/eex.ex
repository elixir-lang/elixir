% # EEx
%
% EEx provides an easy but powerful template for Elixir similar to Ruby's ERb.
%
% ## Examples
%
%     template = EEx.string ~~EOF
%       The value of x is: <%= x %>
%     ~~
%     template.render('x: 42) %=> "\n  The value of x is: 42\n"
%
% ## Allowed tags
%
%     <% Elixir code -- inline with output %>
%     <%= elixir expression -- replace with result %>
%     <%== Elixir expression -- replace with result %>
%     <%# comment -- ignored %>
%
% Also, you can escape any of the tags by putting an extra % at the beginning.
% For instance, the following items:
%
%     <%% 1 + 2 %>
%     <%%= 1 + 2 %>
%     <%? end %>
%
% Return respectively:
%
%     <% 1 + 2 %>
%     <%= 1 + 2 %>
%     <? end %>
%
% ## Engines
%
% Engines change the behavior of common tags as <%= as <%== to add extra behavior.
%
module EEx
  % TODO: Use application setup once we have it.
  def setup
    Code.append_path File.expand_path("../../ebin", __FILE__)
  end

  % Returns an EEx structure that can be rendered with the given binding.
  % See `EEx::Behavior` for more information.
  def string(source, engine := EEx::Engine, filename := "nofile", line := 1)
    compiled = EEx::Compiler.string(engine, source, filename)
    #EEx::Behavior(compiled, filename, line)
  end

  % Returns an EEx structure based on the given file.
  def file(filename, engine := EEx::Engine)
    string(File.read(filename), engine, filename, 1)
  end

  % Implements the behavior for the structures returned by `EEx.string`
  % and `EEx.file`. It basically holds information about the compiled
  % template and is able to render them.
  module Behavior
    attr_reader ['compiled, 'filename, 'line]

    def __bound__(compiled, filename, line)
      @('line: line, 'filename: filename, 'compiled: compiled)
    end

    % Renders the template by evaluating the generated code.
    % Accepts a binding as argument. To change the scope the code
    % is executed, you can pass 'self: some_object as binding.
    def render(binding := [])
      Module.eval @filename, @line, @compiled, binding
    end
  end

  % Implements the main engine for EEx. It supports comments,
  % meta and outputs. It also tries to keep the line
  % numbers sane. <%== is not implemented by this engine.
  module Engine
    def handle_text(buffer, chars, linebreaks)
      "#{buffer},#{"\n"*linebreaks}#{chars.inspect}|binary"
    end

    def handle_comment(buffer, _chars, 0)
      buffer
    end

    def handle_comment(buffer, _chars, linebreaks)
      "#{buffer},#{"\n"*linebreaks}0:0"
    end

    def handle_expr(buffer, '"%", chars)
      "#{buffer},(#{chars}; 0):0"
    end

    def handle_expr(buffer, '"=", chars)
      "#{buffer},(#{chars}).to_s|binary"
    end

    def handle_expr(buffer, '"#", chars)
      "#{buffer},(#{chars}).to_s|binary"
    end

    def wrap_mark(pre, buffer, post)
      "#{pre};#{wrap_buffer(buffer)}#{post}"
    end

    def wrap_buffer(buffer)
      "<<0:0#{buffer}>>"
    end
  end

  % Implements a safe engine that automtically escapes
  % everything output by the <%= tag. It implements
  % <%== to allow unsafe content.
  module SafeEngine
    mixin EEx::Engine

    def handle_expr(buffer, '"=", chars)
      "#{buffer},EEx::Util.html_escape(#{chars})|binary"
    end

    def handle_expr(buffer, '"==", chars)
      "#{buffer},(#{chars}).to_s|binary"
    end
  end

  % Provide some convenience methods for escaping content.
  module Util
    def html_escape(string)
      string.to_s.gsub(~r(&), "\\&amp;").gsub(~r("), "\\&quot;").gsub(~r(>), "\\&gt;").gsub(~r(<), "\\&lt;")
    end
    alias_local 'html_escape, 'h, 1
  end

  % The compiler is an internal structure used by EEx. It is
  % responsible to handle the tokens and call the given engine
  % to properly generate the compiled template.
  module Compiler
    def string(engine, string, filename)
      generate_buffer engine, EEx::Lexer.string(string, filename), "", filename, []
    end

    private

    % Handles text
    def generate_buffer(engine, [{'text,_line,chars}|t], buffer, filename, scope)
      generate_buffer engine, t, engine.handle_text(buffer, chars.to_bin, chars.count($\n)), filename, scope
    end

    % Handles <%# %>
    def generate_buffer(engine, [{'comment,_line,chars}|t], buffer, filename, scope)
      generate_buffer engine, t, engine.handle_comment(buffer, chars.to_bin, chars.count($\n)), filename, scope
    end

    % Handles <% %>
    def generate_buffer(engine, [{'start_end,_line,marker,chars}|t], buffer, filename, scope)
      generate_buffer engine, t, engine.handle_expr(buffer, marker, chars.to_bin), filename, scope
    end

    % Handles <% ?>
    def generate_buffer(engine, [{'start_mark,_line,marker,chars}|t], buffer, filename, scope)
      { contents, t } = generate_buffer engine, t, "", filename, [chars.to_bin|scope]
      generate_buffer engine, t, engine.handle_expr(buffer, marker, contents), filename, scope
    end

    % Handles <? ?>
    def generate_buffer(_engine, [{'mark_mark,line,_chars}|_], _buffer, filename, [])
      unexpected_start line, filename
    end

    def generate_buffer(engine, [{'mark_mark,_line,chars}|t], buffer, filename, [current|scope])
      wrapped = engine.wrap_mark(current, buffer, chars.to_bin)
      generate_buffer engine, t, "", filename, [wrapped|scope]
    end

    % Handles <? %>
    def generate_buffer(_engine, [{'mark_end,line,_chars}|_], _buffer, filename, [])
      unexpected_start line, filename
    end

    def generate_buffer(engine, [{'mark_end,_line,chars}|t], buffer, _filename, [current|_])
      { engine.wrap_mark(current, buffer, chars.to_bin), t }
    end

    % Handles end of string
    def generate_buffer(engine, [], buffer, _filename, [])
      engine.wrap_buffer buffer
    end

    def generate_buffer(_engine, [], _buffer, filename, _scope)
      Erlang.elixir_errors.syntax_error(0, filename, "undetermined end of string", [])
    end

    % Helpers
    def unexpected_start(line, filename)
      Erlang.elixir_errors.syntax_error(line, filename, 'illegal, $"<?")
    end
  end

  % A simple API that wraps the lexer generated automatically by Erlang.
  module Lexer
    % Receives a string and returns all tokens.
    def string(string, filename)
      case Erlang.eex_lexer.string(string.to_char_list, 1)
      match {'ok, tokens, _}
        tokens
      match {'error, {line, _, {error, token}}, _}
        Erlang.elixir_errors.syntax_error(line, filename, error, token)
      end
    end
  end
end