% Regular expressions for Elixir built on top of the re module
% in the Erlang Standard Library. More information can be found
% on re documentation: http://www.erlang.org/doc/man/re.html
%
% Regular expressions in Elixir can be created using Regexp.new,
% Regexp.compile (check their documentation) or using the special
% form with ~r:
%
%     % A simple regular expressions that matches foo anywhere in the string
%     ~r(foo)
%
%     % A regular expression with case insensitive options and handle unicode chars
%     ~r(foo)iu
%
% The re module provides several options, some of them are not available
% in Elixir while others are enabled by default. The ones enabled by default are:
%
% * multiline - the given string is always considered to be multiline, so
%   ^ and $ marks the beginning and end of each line. You need to use \A
%   and \z to match the end or beginning of the string
%
% The available options, followed by their shortcut in parenthesis, are:
%
% * unicode (u) - used when you want to match against specific unicode characters
% * caseless (i) - add case insensitivity
% * dotall (m) - causes dot to match newlines and also set newline to anycrlf.
%   The new line setting can be overwritten by setting (*CR) or (*LF) or (*CRLF)
%   or (*ANY) according to re documentation
% * extended (x) - whitespace characters are ignored except when escaped and
%   allow # to delimit comments
% * firstline (f) - forces the unanchored pattern to match before or at the first
%   newline, though the matched text may continue over the newline
% * ungreedy (r) - invert the "greediness" of the regexp
%
% The options not available are:
%
% * anchored - not available, use ^ or \A instead
% * dollar_endonly - not available, use \z instead
% * no_auto_capture - not available, use ?: instead
% * newline - not available, use (*CR) or (*LF) or (*CRLF) or (*ANYCRLF)
%   or (*ANY) at the beginning of the regexp according to the re documentation
%
object Regexp
  % The same as constructor(regexp, options) but no option is given.
  def constructor(regexp)
    constructor(regexp, [])
  end

  % Creates a new regular expression. It expects two arguments,
  % the regular expression and a set of options. Both should be
  % a string or a list of chars and, if not, to_char_list is
  % invoked in order to retrieve the list of chars.
  %
  % ## Examples
  %
  %     Regexp.new("foo", "iu")
  %
  def constructor(regexp, options)
    regexp_bin = regexp.to_bin

    parsed_options = options.to_char_list.foldl ['multiline], do (x, acc)
      parse_option(x, acc)
    end

    { 'ok, compiled } = Erlang.re.compile(regexp_bin, parsed_options)
    { 'bin: regexp_bin, 'parsed_options: parsed_options, 'compiled: compiled }
  end

  def run(target)
    Erlang.re.run(target.to_bin, @compiled)
  end

  def match?(target)
    match_happened?(run(target))
  end

  private

  def match_happened?('nomatch)
    false
  end

  def match_happened?(_)
    true
  end

  def parse_option($u, acc); ['unicode|acc]; end
  def parse_option($i, acc); ['caseless|acc]; end
  def parse_option($x, acc); ['extended|acc]; end
  def parse_option($f, acc); ['firstline|acc]; end
  def parse_option($r, acc); ['ungreedy|acc]; end
  def parse_option($m, acc); ['dotall, {'newline, 'anycrlf}|acc]; end

  % TODO Do not use Erlang.error once Elixir's exception system is working
  def parse_option(option, _)
    Erlang.error({'badarg, ~Q(unknown option "#{option.chr}").to_char_list})
  end
end