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
% TODO: We need to inherit from BitString once we have inheritance.
object String
  % Implement String as a record. This is done mainly
  % to have improved performance for the equality operator.
  module Mixin
    % Initializes a string by keeping its internal binary representation.
    def new([bin])
      IO.puts "[ELIXIR] Calling String.new is deprecated. Just call .to_bin in the list object instead."
      bin.to_bin
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
    Erlang.binary_to_list(self)[number]
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
    if length < 0
      Erlang.binary_part(self, start, Erlang.size(self) - start + length + 1)
    else
      Erlang.binary_part(self, start, length)
    end
  end

  % Concatenate two strings.
  %
  % ## Examples
  %
  %     "eli" + "xir" % => "elixir"
  %
  def +(another)
    <<self|binary, another|binary>>
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
    Erlang.size(self)
  end

  % Returns the list representation of this String.
  def to_list
    Erlang.binary_to_list(self)
  end

  % Returns the list of chars represantion of this String.
  def to_char_list
    Erlang.binary_to_list(self)
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
  % Please check `Regexp#replace` for more information about the characters
  % allowed in *replacement*.
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
    if given.__parent_name__ == 'Regexp
      given.replace(self, replacement)
    else
      Regexp.new(Regexp.escape(given)).replace(self, replacement)
    end
  end

  % Substitute the **all** occurrence of *given* in the string by *replacement*.
  % Please check `Regexp#replace` for more information about the characters
  % allowed in *replacement*.
  %
  % ## Examples
  %
  %     "abcbe"     = "abcbe".gsub(~r(d), "d")
  %     "adcde"     = "abcbe".gsub(~r(b), "d")
  %     "a[b]c[b]e" = "abcbe".gsub(~r(b), "[&]")
  %     "a[&]c[&]e" = "abcbe".gsub(~r(b), "[\\&]")
  %     "a[b]c[b]e" = "abcbe".gsub(~r[(b)], "[\\1]")
  def gsub(given, replacement)
    if given.__parent_name__ == 'Regexp
      given.replace_all(self, replacement)
    else
      Regexp.new(Regexp.escape(given)).replace_all(self, replacement)
    end
  end

  % Returns a string representation of this string.
  %
  % ## Examples
  %
  %     "elixir".inspect % => "\"elixir\""
  %
  def inspect
    <<$\", self|binary, $\">>
  end

  % Receives a regular expression and split the string. An optional number
  % of parts to split the string can be given. By default is the atom infinity.
  %
  % ## Examples
  %
  %     ["foo", "baz", "bat"] = "foobarbazbarbat".split(~r"bar")
  %     ["foo", "bazbarbat"] = "foobarbazbarbat".split(~r"bar", 2)
  %
  def split(given, parts := 'infinity)
    if given.__parent_name__ == 'Regexp
      given.split(self, parts)
    else
      Regexp.new(Regexp.escape(given)).split(self, parts)
    end
  end

  % Scan the whole string returning all matches.
  %
  % ## Examples
  %
  %     "abc"   = "key1=value1; key2=value2".scan(~r"(?:(\w+)=(\w+);?)")
  %
  def scan(given, offset := 0)
    if given.__parent_name__ == 'Regexp
      given.scan(self, 'all, offset)
    else
      Regexp.new(Regexp.escape(given)).scan(self, 'all, offset)
    end
  end

  % Returns true if the string is empty.
  def empty?
    Erlang.size(self) == 0
  end

  % Returns the string itself.
  def to_bin
    self
  end

  % Returns the string itself.
  def to_s
    self
  end

  def to_atom
    Erlang.binary_to_atom(self, 'utf8)
  end

  % Returns a copy of the original string with all lowercase letters replaced with their uppercase counterparts.
  def upcase
    Erlang.string.to_upper(to_char_list).to_bin
  end
 
  % Returns a copy of the original string with all uppercase letters replaced with their lowercase counterparts.
  def downcase
    Erlang.string.to_lower(to_char_list).to_bin
  end
 
  % Returns a new string with the characters from original string in reverse order.
  def reverse
    to_char_list.reverse.to_bin
  end

  private

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
