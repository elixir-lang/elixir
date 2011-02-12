% ## String and Erlang
%
% In Elixir, we have both strings, binaries and char lists. They are
% all different ways to represent the same thing, the choice is just
% a matter of which API you want to use. A String will be mapped to
% the String object while the others will be mapped respectively to
% Binary and List objects.
%
% It is important to notice that if you need to interact with an Erlang
% method, you need to convert a string either to to_bin or to_char_list,
% as Erlang does not understand the string representation from Elixir.
%
object String
  % Initializes a string by keeping its internal list representation.
  def constructor(bin)
    { 'bin: bin.to_bin }
  end

  % Concatenate two strings.
  %
  % ## Examples
  %
  %     "eli" + "xir" % => "elixir"
  %
  def +(another)
    String.new <<@bin|binary, another.to_bin|binary>>
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
    Erlang.length(Erlang.unicode.characters_to_list(@bin, 'utf8))
  end

  % Returns the list representation of this String.
  def to_list
    Erlang.binary_to_list @bin
  end

  % Returns the list of chars represantion of this String.
  def to_char_list
    Erlang.binary_to_list @bin
  end

  % Returns a string representation of this string.
  %
  % ## Examples
  %
  %     "elixir".inspect % => "\"elixir\""
  %
  def inspect
    String.new <<$\", @bin|binary, $\">>
  end

  def to_bin
    @bin
  end

  % Returns the string itself.
  def to_s
    self
  end
end
