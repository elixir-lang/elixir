% In Elixir, strings are represented as binaries. A binary is a bit
% string where its size is a multiple of eight. A BitString created
% conforming to this condition will be automatically mapped as String:
%
%     <<72, 73, 74>>.__parent__ % => String
%
% ## `to_char_list`, `to_bin`, `to_s` and `to_str`
%
% There are four methods responsible for conversion from and to strings.
%
% Since Erlang represents strings as lists, `to_bin` and `to_char_list`
% has the sole purpose of converting from Erlang strings (char lists)
% to Elixir strings (binaries) and vice-versa. In general, those methods
% are only invoked when handling data from/to Erlang. Such methods should
% not be implemented in your own structures.
%
% On the other hand, `to_s` should return a string representation of
% a data structure while implementing `to_str` means that a structure
% could be used in any place a string would normaly be used.
%
% Notice that to_i and to_int follows exactly the same convention.
module String
  module Behavior
    % Returns a new string as a concatenation of the given *number*
    % of the original string.
    %
    % ## Examples
    %
    %    "foo" * 3  % => "foofoofoo"
    %
    def *(0)
      ""
    end

    def *(number)
      Erlang.binary.copy(self, number)
    end

    % Retrieves a number that represents the given character.
    %
    % ## Examples
    %
    %     "elixir"[3]   % => 140
    %     "elixir"[-3]  % => 140
    %
    def [](number)
      if number < 0
        Erlang.binary.at(self, Erlang.size(self) + number)
      else
        Erlang.binary.at(self, number)
      end
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
    alias_local 'length, 'size, 0

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

    % Returns the index of the first occurence of the given substring or matching regex.
    % Returns nil if nothing is found.
    %
    % ## Examples
    %
    %    1   = "hello".index('e')
    %    3   = "hello".index('lo')
    %    nil = "hello".index('a')
    %
    def index(given)
      if given.__module_name__ == 'Regexp::Behavior
        case given.indexes(self)
        match [{x,_}|_]
          x
        match nil
          nil
        end
      else
        result = Erlang.string.str(to_char_list, given.to_char_list)
        case result
        match 0 then nil
        match _ then result - 1
        end
      end
    end

    % Returns the index of the first occurence of the given substring or matching regex.
    % Returns nil if nothing is found.
    %
    % ## Examples
    %
    %    1   = "hello".count('e')
    %    3   = "hello".count('lo')
    %    0   = "hello".count('a')
    %
    def count(given)
      count given.to_char_list, to_char_list, 0
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
      if given.__module_name__ == 'Regexp::Behavior
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
      if given.__module_name__ == 'Regexp::Behavior
        given.replace_all(self, replacement)
      else
        Regexp.new(Regexp.escape(given)).replace_all(self, replacement)
      end
    end

    % Remove all space characters from the beginning and end of the string.
    def strip
      gsub(~r"\A\s*|\s*\z", "")
    end

    % Remove all space characters from the beginning of the string.
    def lstrip
      gsub(~r"\A\s*", "")
    end

    % Remove all space characters from the end of the string.
    def rstrip
      gsub(~r"\s*\z", "")
    end

    % Returns a string representation of this string.
    %
    % ## Examples
    %
    %     "elixir".inspect % => "\"elixir\""
    %
    def inspect
      list = Erlang.binary_to_list(self)
      if Erlang.io_lib.printable_unicode_list(list)
        <<$\", escape(list, [])|binary, $\">>
      else
        Erlang.io_lib.format($"~w", [self]).to_bin
      end
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
      if given.__module_name__ == 'Regexp::Behavior
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
      if given.__module_name__ == 'Regexp::Behavior
        given.scan(self, offset)
      else
        Regexp.new(Regexp.escape(given)).scan(self, offset)
      end
    end

    % Return a string with the last character removed. If the string end
    % with \r\n then both characters are removed.
    %
    % ## Examples
    %
    %    "foo".chop     % => "fo"
    %    "foo\r\n".chop % => "foo"
    %    "foo\n\r".chop % => "foo\n"
    %    "x".chop.chop  % => ""
    %
    def chop
      sub(~r"(\r\n|.)\z", "")
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

    % Returns the string itself.
    def to_str
      self
    end

    % Returns the list of chars represantion of this String.
    def to_char_list
      Erlang.binary_to_list(self)
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

    % Returns a copy of the origin string with the first character converted to uppercase and the rest to lowercase.
    def capitalize
      [h|t] = Erlang.string.to_lower(to_char_list)
      [Erlang.string.to_upper(h)|t].to_bin
    end

    % Returns a new string with the characters from original string in reverse order.
    def reverse
      to_char_list.reverse.to_bin
    end

    private

    def escape([h|t], buffer)
      char = case h
      match $#, $\"
        [$\\,h]
      match $\b
        [$\\,$b]
      match $\d
        [$\\,$d]
      match $\e
        [$\\,$e]
      match $\f
        [$\\,$f]
      match $\n
        [$\\,$n]
      match $\r
        [$\\,$r]
      match $\t
        [$\\,$t]
      match $\v
        [$\\,$v]
      else
        h
      end

      escape(t, [char|buffer])
    end

    def escape([], buffer)
      Erlang.iolist_to_binary(Erlang.lists.reverse(buffer))
    end

    def count(items, [h|t], counter)
      count items, t, count_each(h, items, counter)
    end

    def count(_items, [], counter)
      counter
    end

    def count_each(item, [item|_], counter)
      counter + 1
    end

    def count_each(item, [_|t], counter)
      count_each item, t, counter
    end

    def count_each(_item, [], counter)
      counter
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
end
