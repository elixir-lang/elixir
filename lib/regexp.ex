% elixir: cache

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
  module Mixin
    % Escape the given string so it can match a regular expression.
    def escape(string)
      binary = Erlang.re.replace(string.to_bin, @escape_regexp, $"\\\\&", [{'return,'binary},'global])
      String.new binary
    end
  end

  % Have the escape regexp pre-compiled and stored.
  { 'ok, compiled } = Erlang.re.compile($"\\\\|\\{|\\[|\\(|\\)|\\]|\\}")
  @('escape_regexp, compiled)

  % Creates a new regular expression. It expects two arguments,
  % the regular expression and a set of options. Both should be
  % a string or a list of chars and, if not, to_char_list is
  % invoked in order to retrieve the list of chars.
  %
  % ## Examples
  %
  %     Regexp.new("foo", "iu")
  %
  def initialize(regexp, options := [])
    regexp_bin = regexp.to_bin

    parsed_options = options.to_char_list.foldl ['multiline], do (x, acc)
      parse_option(x, acc)
    end

    { 'ok, compiled } = Erlang.re.compile(regexp_bin, parsed_options)
    @('bin: regexp_bin, 'parsed_options: parsed_options, 'compiled: compiled)
  end

  % Returns a boolean depending if the regular expressions matches the given string.
  def match?(target)
    'nomatch != Erlang.re.run(target.to_bin, @compiled)
  end

  % Run the regular expression against the given target. It returns a list with
  % all matches or an empty list if no match occurred.
  %
  % This method accepts *captures* as second argument specifying which captures
  % from the regular expression should be handled.
  def run(target, captures := 'all)
    case Erlang.re.run(target.to_bin, @compiled, [{'capture, captures, 'binary}])
    match 'nomatch
      []
    match {'match, results}
      [String.new(r) for r in results]
    end
  end

  % Same as run, but scan the target several times collecting all matches of
  % the regular expression. This is similar to the /g option in Perl.
  def scan(target, captures := 'all_but_first)
    case Erlang.re.run(target.to_bin, @compiled, [{'capture, captures, 'binary}, 'global])
    match 'nomatch
      []
    match {'match, results}
      results.map -> (result) [String.new(r) for r in result]
    end
  end

  % Split the given *target* in the number of *parts* specified.
  def split(target, parts := 'infinity)
    list = Erlang.re.split(target.to_bin, @compiled, [{'return,'binary},'trim,{'parts, parts}])
    [String.new(l) for l in list, l != <<>>]
  end

  % Receives a string and a replacement and returns a string where the first match
  % of the regular expressions is replaced by replacement. Inside the replacement,
  % you can either give "&" to access the whole regular expression or \N, where
  % N is in integer to access an specific matching parens.
  %
  % ## Examples
  %
  %     "abc"   = ~r(d).replace("abc", "d")
  %     "adc"   = ~r(b).replace("abc", "d")
  %     "a[b]c" = ~r(b).replace("abc", "[&]")
  %     "a[&]c" = ~r(b).replace("abc", "[\\&]")
  %     "a[b]c" = ~r[(b)].replace("abc", "[\\1]")
  %
  def replace(string, replacement)
    binary = Erlang.re.replace(string.to_bin, @compiled, replacement.to_bin, [{'return,'binary}])
    String.new binary
  end

  % The same as replace, but replaces all parts where the regular expressions
  % matches in the string. Please read `replace` for documentation and examples.
  def replace_all(string, replacement)
    binary = Erlang.re.replace(string.to_bin, @compiled, replacement.to_bin, [{'return,'binary},'global])
    String.new binary
  end

  private

  def parse_option($u, acc); ['unicode|acc]; end
  def parse_option($i, acc); ['caseless|acc]; end
  def parse_option($x, acc); ['extended|acc]; end
  def parse_option($f, acc); ['firstline|acc]; end
  def parse_option($r, acc); ['ungreedy|acc]; end
  def parse_option($m, acc); ['dotall, {'newline, 'anycrlf}|acc]; end

  def parse_option(option, _)
    self.error({'badarg, ~Q(unknown option "#{option.chr}").to_char_list})
  end
end