% ## String and Char lists
%
% In Elixir, we have both strings and char lists. They are two ways to
% represent the same thing, the choice is just a matter of which API
% you want to use. A String will be mapped to the String object while
% the char list to the List object.
%
% It is important to notice that if you need to interact with an Erlang
% method, you need to pass a to_char_list as Erlang does not understand
% the string representation from Elixir.
%
% For this reason, just String and List implement the to_char_list
% method to avoid implicit conversion of other data types. General
% conversion to string happens through the to_s method. Which is
% implemented in all objects (unless explicitly undefined).
%
object String
  % Initializes a string by keeping its internal list representation.
  def constructor(list)
    { 'list: list }
  end

  % Concatenate two strings.
  %
  % ## Examples
  %
  %     "eli" + "xir" % => "elixir"
  %
  def +(another)
    String.new(@list + another.to_char_list)
  end

  % Returns the length of the string. All strings parsed by the
  % interpreter are handled as utf-8. Any I/O driver should be
  % responsible to convert to utf-8.
  %
  % ## Examples
  %
  %     "elixir".length % => 6
  %     "josé".length   % => 4
  %
  def length
    Erlang.length(@list)
  end

  % Returns the list representation of this String.
  def to_list
    @list
  end

  % Returns the list of chars represantion of this String.
  def to_char_list
    @list
  end

  % Returns a string representation of this string.
  %
  % ## Examples
  %
  %     "elixir".inspect % => "\"elixir\""
  %
  % TODO Which one is better [$"|@list] + [$"] or "\"#{@list}\""?
  def inspect
    String.new [$"|@list] + [$"]
  end

  % Returns the string itself.
  def to_s
    self
  end
end
