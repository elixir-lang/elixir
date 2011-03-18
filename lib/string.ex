% elixir: cache

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
  % Implement String as a record. This is done mainly
  % to have improved performance for the equality operator.
  module Mixin
    % Initializes a string by keeping its internal binary representation.
    def new([bin])
      { 'elixir_string__, bin.to_bin }
    end
  end

  % Retrieves a number that represents the given character.
  %
  % ## Examples
  %
  %     "elixir"[3]   % => 140
  %     "elixir"[-3]  % => 140
  %
  def [](number)
    Erlang.binary_to_list(bin)[number]
  end

  % Slice the string in the given *start* and *length* arguments. If length
  % is less than zero, it is the negative index to the end of the string.
  %
  % ## Examples
  %
  %     "[1,2,3]"[0,3]   % => "[1,"
  %     "[1,2,3]"[1,-2]  % => "1,2,3"
  %
  def [](start, length)
    bin = to_bin

    if length < 0
      String.new Erlang.binary_part(bin, start, Erlang.size(bin) - start + length + 1)
    else
      String.new Erlang.binary_part(bin, start, length)
    end
  end

  % Concatenate two strings.
  %
  % ## Examples
  %
  %     "eli" + "xir" % => "elixir"
  %
  def +(another)
    String.new <<bin|binary, another.to_bin|binary>>
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
    Erlang.size(bin)
  end

  % Returns the list representation of this String.
  def to_list
    Erlang.binary_to_list(bin)
  end

  % Returns the list of chars represantion of this String.
  def to_char_list
    Erlang.binary_to_list(bin)
  end

  % Check if the current string includes the given string.
  %
  % ## Examples
  %
  %     true  = "elixir".include?("el")
  %     false = "elixir".include?("ex")
  %
  def include?(string)
    cl1 = to_char_list
    cl2 = string.to_char_list
    include?(cl1, cl2, cl1.length, cl2.length)
  end

  % Substitute the first occurrence of *given* in the string by *replacement*.
  %
  % Currently, given can be only be a regular expression, strings may be allowed
  % in the future. Please check `Regexp#replace` for more information about
  % the characters allowed in *replacement*.
  %
  % ## Examples
  %
  %     "abc"   = "abc".sub(~r(d), "d")
  %     "adc"   = "abc".sub(~r(b), "d")
  %     "a[b]c" = "abc".sub(~r(b), "[&]")
  %     "a[&]c" = "abc".sub(~r(b), "[\\&]")
  %     "a[b]c" = "abc".sub(~r[(b)], "[\\1]")
  %
  def sub(given, replacement)
    given.replace(self, replacement)
  end

  % Substitute the **all** occurrence of *given* in the string by *replacement*.
  %
  % Currently, given can be only be a regular expression, strings may be allowed
  % in the future. Please check `Regexp#replace` for more information about
  % the characters allowed in *replacement*.
  %
  % ## Examples
  %
  %     "abcbe"     = "abcbe".gsub(~r(d), "d")
  %     "adcde"     = "abcbe".gsub(~r(b), "d")
  %     "a[b]c[b]e" = "abcbe".gsub(~r(b), "[&]")
  %     "a[&]c[&]e" = "abcbe".gsub(~r(b), "[\\&]")
  %     "a[b]c[b]e" = "abcbe".gsub(~r[(b)], "[\\1]")
  def gsub(given, replacement)
    given.replace_all(self, replacement)
  end

  % Returns a string representation of this string.
  %
  % ## Examples
  %
  %     "elixir".inspect % => "\"elixir\""
  %
  def inspect
    String.new <<$\", bin|binary, $\">>
  end

  % Returns true if the string is empty.
  def empty?
    Erlang.size(bin) == 0
  end

  def to_bin
    bin
  end

  % Returns the string itself.
  def to_s
    self
  end

  def to_atom
    Erlang.binary_to_atom(bin, 'utf8)
  end

  private

  def bin
    Erlang.element(2, self)
  end

  def include?([], _, _, _)
    false
  end

  def include?(original, compare, l1, l2)
    if prefix(compare, original, l2, l1)
      true
    else
      [_|t] = original
      include? t, compare, l1 - 1, l2
    end
  end

  def prefix(pre, string, l1, l2)
    if l2 < l1
      false
    else
      prefix(pre, string)
    end
  end

  def prefix([h|pre], [h|string])
    prefix(pre, string)
  end

  def prefix([], _)
    true
  end

  def prefix(_, _)
    false
  end
end
