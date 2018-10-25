defmodule Path do
  @moduledoc """
  This module provides conveniences for manipulating or
  retrieving file system paths.

  The functions in this module may receive a chardata as
  argument (i.e. a string or a list of characters / string)
  and will always return a string (encoded in UTF-8).

  The majority of the functions in this module do not
  interact with the file system, except for a few functions
  that require it (like `wildcard/2` and `expand/1`).
  """

  @type t :: :unicode.chardata()

  @doc """
  Converts the given path to an absolute one. Unlike
  `expand/1`, no attempt is made to resolve `..`, `.` or `~`.

  ## Examples

  ### Unix

      Path.absname("foo")
      #=> "/usr/local/foo"

      Path.absname("../x")
      #=> "/usr/local/../x"

  ### Windows

      Path.absname("foo")
      #=> "D:/usr/local/foo"

      Path.absname("../x")
      #=> "D:/usr/local/../x"

  """
  @spec absname(t) :: binary
  def absname(path) do
    absname(path, File.cwd!())
  end

  @doc """
  Builds a path from `relative_to` to `path`.

  If `path` is already an absolute path, `relative_to` is ignored. See also
  `relative_to/2`.

  Unlike `expand/2`, no attempt is made to
  resolve `..`, `.` or `~`.

  ## Examples

      iex> Path.absname("foo", "bar")
      "bar/foo"

      iex> Path.absname("../x", "bar")
      "bar/../x"

  """
  @spec absname(t, t) :: binary
  def absname(path, relative_to) do
    path = IO.chardata_to_string(path)

    case type(path) do
      :relative ->
        absname_join(relative_to, path)

      :absolute ->
        absname_join([path])

      :volumerelative ->
        relative_to = IO.chardata_to_string(relative_to)
        absname_vr(split(path), split(relative_to), relative_to)
    end
  end

  # Absolute path on current drive
  defp absname_vr(["/" | rest], [volume | _], _relative), do: absname_join([volume | rest])

  # Relative to current directory on current drive.
  defp absname_vr([<<x, ?:>> | rest], [<<x, _::binary>> | _], relative),
    do: absname(absname_join(rest), relative)

  # Relative to current directory on another drive.
  defp absname_vr([<<x, ?:>> | name], _, _relative) do
    cwd =
      case :file.get_cwd([x, ?:]) do
        {:ok, dir} -> IO.chardata_to_string(dir)
        {:error, _} -> <<x, ?:, ?/>>
      end

    absname(absname_join(name), cwd)
  end

  # Joins a list
  defp absname_join([name1, name2 | rest]), do: absname_join([absname_join(name1, name2) | rest])

  defp absname_join([name]),
    do: do_absname_join(IO.chardata_to_string(name), <<>>, [], major_os_type())

  # Joins two paths
  defp absname_join(left, right),
    do: do_absname_join(IO.chardata_to_string(left), relative(right), [], major_os_type())

  defp do_absname_join(<<uc_letter, ?:, rest::binary>>, relativename, [], :win32)
       when uc_letter in ?A..?Z do
    do_absname_join(rest, relativename, [?:, uc_letter + ?a - ?A], :win32)
  end

  defp do_absname_join(<<?\\, rest::binary>>, relativename, result, :win32),
    do: do_absname_join(<<?/, rest::binary>>, relativename, result, :win32)

  defp do_absname_join(<<?/, rest::binary>>, relativename, [?., ?/ | result], os_type),
    do: do_absname_join(rest, relativename, [?/ | result], os_type)

  defp do_absname_join(<<?/, rest::binary>>, relativename, [?/ | result], os_type),
    do: do_absname_join(rest, relativename, [?/ | result], os_type)

  defp do_absname_join(<<>>, <<>>, result, os_type),
    do: IO.iodata_to_binary(reverse_maybe_remove_dir_sep(result, os_type))

  defp do_absname_join(<<>>, relativename, [?: | rest], :win32),
    do: do_absname_join(relativename, <<>>, [?: | rest], :win32)

  defp do_absname_join(<<>>, relativename, [?/ | result], os_type),
    do: do_absname_join(relativename, <<>>, [?/ | result], os_type)

  defp do_absname_join(<<>>, relativename, result, os_type),
    do: do_absname_join(relativename, <<>>, [?/ | result], os_type)

  defp do_absname_join(<<char, rest::binary>>, relativename, result, os_type),
    do: do_absname_join(rest, relativename, [char | result], os_type)

  defp reverse_maybe_remove_dir_sep([?/, ?:, letter], :win32), do: [letter, ?:, ?/]
  defp reverse_maybe_remove_dir_sep([?/], _), do: [?/]
  defp reverse_maybe_remove_dir_sep([?/ | name], _), do: :lists.reverse(name)
  defp reverse_maybe_remove_dir_sep(name, _), do: :lists.reverse(name)

  @doc """
  Converts the path to an absolute one and expands
  any `.` and `..` characters and a leading `~`.

  ## Examples

      Path.expand("/foo/bar/../bar")
      #=> "/foo/bar"

  """
  @spec expand(t) :: binary
  def expand(path) do
    expand_dot(absname(expand_home(path), File.cwd!()))
  end

  @doc """
  Expands the path relative to the path given as the second argument
  expanding any `.` and `..` characters.

  If the path is already an absolute path, `relative_to` is ignored.

  Note that this function treats a `path` with a leading `~` as
  an absolute one.

  The second argument is first expanded to an absolute path.

  ## Examples

      # Assuming that the absolute path to baz is /quux/baz
      Path.expand("foo/bar/../bar", "baz")
      #=> "/quux/baz/foo/bar"

      Path.expand("foo/bar/../bar", "/baz")
      #=> "/baz/foo/bar"

      Path.expand("/foo/bar/../bar", "/baz")
      #=> "/foo/bar"

  """
  @spec expand(t, t) :: binary
  def expand(path, relative_to) do
    expand_dot(absname(absname(expand_home(path), expand_home(relative_to)), File.cwd!()))
  end

  @doc """
  Returns the path type.

  ## Examples

  ### Unix

      Path.type("/")                #=> :absolute
      Path.type("/usr/local/bin")   #=> :absolute
      Path.type("usr/local/bin")    #=> :relative
      Path.type("../usr/local/bin") #=> :relative
      Path.type("~/file")           #=> :relative

  ### Windows

      Path.type("D:/usr/local/bin") #=> :absolute
      Path.type("usr/local/bin")    #=> :relative
      Path.type("D:bar.ex")         #=> :volumerelative
      Path.type("/bar/foo.ex")      #=> :volumerelative

  """
  @spec type(t) :: :absolute | :relative | :volumerelative
  def type(name)
      when is_list(name)
      when is_binary(name) do
    pathtype(name, major_os_type()) |> elem(0)
  end

  @doc """
  Forces the path to be a relative path.

  ## Examples

  ### Unix

      Path.relative("/usr/local/bin")   #=> "usr/local/bin"
      Path.relative("usr/local/bin")    #=> "usr/local/bin"
      Path.relative("../usr/local/bin") #=> "../usr/local/bin"

  ### Windows

      Path.relative("D:/usr/local/bin") #=> "usr/local/bin"
      Path.relative("usr/local/bin")    #=> "usr/local/bin"
      Path.relative("D:bar.ex")         #=> "bar.ex"
      Path.relative("/bar/foo.ex")      #=> "bar/foo.ex"

  """
  @spec relative(t) :: binary
  def relative(name) do
    relative(name, major_os_type())
  end

  defp relative(name, os_type) do
    pathtype(name, os_type)
    |> elem(1)
    |> IO.chardata_to_string()
  end

  defp pathtype(name, os_type) do
    case os_type do
      :win32 -> win32_pathtype(name)
      _ -> unix_pathtype(name)
    end
  end

  defp unix_pathtype(path) when path in ["/", '/'], do: {:absolute, "."}
  defp unix_pathtype(<<?/, relative::binary>>), do: {:absolute, relative}
  defp unix_pathtype([?/ | relative]), do: {:absolute, relative}
  defp unix_pathtype([list | rest]) when is_list(list), do: unix_pathtype(list ++ rest)
  defp unix_pathtype(relative), do: {:relative, relative}

  @slash [?/, ?\\]

  defp win32_pathtype([list | rest]) when is_list(list), do: win32_pathtype(list ++ rest)

  defp win32_pathtype([char, list | rest]) when is_list(list),
    do: win32_pathtype([char | list ++ rest])

  defp win32_pathtype(<<c1, c2, relative::binary>>) when c1 in @slash and c2 in @slash,
    do: {:absolute, relative}

  defp win32_pathtype(<<char, relative::binary>>) when char in @slash,
    do: {:volumerelative, relative}

  defp win32_pathtype(<<_letter, ?:, char, relative::binary>>) when char in @slash,
    do: {:absolute, relative}

  defp win32_pathtype(<<_letter, ?:, relative::binary>>), do: {:volumerelative, relative}

  defp win32_pathtype([c1, c2 | relative]) when c1 in @slash and c2 in @slash,
    do: {:absolute, relative}

  defp win32_pathtype([char | relative]) when char in @slash, do: {:volumerelative, relative}

  defp win32_pathtype([c1, c2, list | rest]) when is_list(list),
    do: win32_pathtype([c1, c2 | list ++ rest])

  defp win32_pathtype([_letter, ?:, char | relative]) when char in @slash,
    do: {:absolute, relative}

  defp win32_pathtype([_letter, ?: | relative]), do: {:volumerelative, relative}
  defp win32_pathtype(relative), do: {:relative, relative}

  @doc """
  Returns the given `path` relative to the given `from` path.

  In other words, this function tries to strip the `from` prefix from `path`.

  This function does not query the file system, so it assumes
  no symlinks between the paths.

  In case a direct relative path cannot be found, it returns
  the original path.

  ## Examples

      iex> Path.relative_to("/usr/local/foo", "/usr/local")
      "foo"

      iex> Path.relative_to("/usr/local/foo", "/")
      "usr/local/foo"

      iex> Path.relative_to("/usr/local/foo", "/etc")
      "/usr/local/foo"

  """
  @spec relative_to(t, t) :: binary
  def relative_to(path, from) do
    path = IO.chardata_to_string(path)
    relative_to(split(path), split(from), path)
  end

  defp relative_to([h | t1], [h | t2], original) do
    relative_to(t1, t2, original)
  end

  defp relative_to([_ | _] = l1, [], _original) do
    join(l1)
  end

  defp relative_to(_, _, original) do
    original
  end

  @doc """
  Convenience to get the path relative to the current working
  directory.

  If, for some reason, the current working directory
  cannot be retrieved, this function returns the given `path`.
  """
  @spec relative_to_cwd(t) :: binary
  def relative_to_cwd(path) do
    case :file.get_cwd() do
      {:ok, base} -> relative_to(path, IO.chardata_to_string(base))
      _ -> path
    end
  end

  @doc """
  Returns the last component of the path or the path
  itself if it does not contain any directory separators.

  ## Examples

      iex> Path.basename("foo")
      "foo"

      iex> Path.basename("foo/bar")
      "bar"

      iex> Path.basename("/")
      ""

  """
  @spec basename(t) :: binary
  def basename(path) do
    :filename.basename(IO.chardata_to_string(path))
  end

  @doc """
  Returns the last component of `path` with the `extension`
  stripped.

  This function should be used to remove a specific
  extension which may or may not be there.

  ## Examples

      iex> Path.basename("~/foo/bar.ex", ".ex")
      "bar"

      iex> Path.basename("~/foo/bar.exs", ".ex")
      "bar.exs"

      iex> Path.basename("~/foo/bar.old.ex", ".ex")
      "bar.old"

  """
  @spec basename(t, t) :: binary
  def basename(path, extension) do
    :filename.basename(IO.chardata_to_string(path), IO.chardata_to_string(extension))
  end

  @doc """
  Returns the directory component of `path`.

  ## Examples

      iex> Path.dirname("/foo/bar.ex")
      "/foo"

      iex> Path.dirname("/foo/bar/baz.ex")
      "/foo/bar"

      iex> Path.dirname("/foo/bar/")
      "/foo/bar"

  """
  @spec dirname(t) :: binary
  def dirname(path) do
    :filename.dirname(IO.chardata_to_string(path))
  end

  @doc """
  Returns the extension of the last component of `path`.

  ## Examples

      iex> Path.extname("foo.erl")
      ".erl"

      iex> Path.extname("~/foo/bar")
      ""

  """
  @spec extname(t) :: binary
  def extname(path) do
    :filename.extension(IO.chardata_to_string(path))
  end

  @doc """
  Returns the `path` with the `extension` stripped.

  ## Examples

      iex> Path.rootname("/foo/bar")
      "/foo/bar"

      iex> Path.rootname("/foo/bar.ex")
      "/foo/bar"

  """
  @spec rootname(t) :: binary
  def rootname(path) do
    :filename.rootname(IO.chardata_to_string(path))
  end

  @doc """
  Returns the `path` with the `extension` stripped.

  This function should be used to remove a specific extension which may
  or may not be there.

  ## Examples

      iex> Path.rootname("/foo/bar.erl", ".erl")
      "/foo/bar"

      iex> Path.rootname("/foo/bar.erl", ".ex")
      "/foo/bar.erl"

  """
  @spec rootname(t, t) :: binary
  def rootname(path, extension) do
    :filename.rootname(IO.chardata_to_string(path), IO.chardata_to_string(extension))
  end

  @doc """
  Joins a list of paths.

  This function should be used to convert a list of paths to a path.
  Note that any trailing slash is removed when joining.

  ## Examples

      iex> Path.join(["~", "foo"])
      "~/foo"

      iex> Path.join(["foo"])
      "foo"

      iex> Path.join(["/", "foo", "bar/"])
      "/foo/bar"

  """
  @spec join(nonempty_list(t)) :: binary
  def join([name1, name2 | rest]), do: join([join(name1, name2) | rest])
  def join([name]), do: IO.chardata_to_string(name)

  @doc """
  Joins two paths.

  The right path will always be expanded to its relative format
  and any trailing slash will be removed when joining.

  ## Examples

      iex> Path.join("foo", "bar")
      "foo/bar"

      iex> Path.join("/foo", "/bar/")
      "/foo/bar"

  The functions in this module support chardata, so giving a list will
  treat it as a single entity:

      iex> Path.join("foo", ["bar", "fiz"])
      "foo/barfiz"

      iex> Path.join(["foo", "bar"], "fiz")
      "foobar/fiz"

  """
  @spec join(t, t) :: binary
  def join(left, right) do
    left = IO.chardata_to_string(left)
    os_type = major_os_type()
    do_join(left, right, os_type) |> remove_dir_sep(os_type)
  end

  defp do_join("", right, os_type), do: relative(right, os_type)
  defp do_join("/", right, os_type), do: "/" <> relative(right, os_type)

  defp do_join(left, right, os_type),
    do: remove_dir_sep(left, os_type) <> "/" <> relative(right, os_type)

  defp remove_dir_sep("", _os_type), do: ""
  defp remove_dir_sep("/", _os_type), do: "/"

  defp remove_dir_sep(bin, os_type) do
    last = :binary.last(bin)

    if last == ?/ or (last == ?\\ and os_type == :win32) do
      binary_part(bin, 0, byte_size(bin) - 1)
    else
      bin
    end
  end

  @doc ~S"""
  Splits the path into a list at the path separator.

  If an empty string is given, returns an empty list.

  On Windows, path is split on both "\" and "/" separators
  and the driver letter, if there is one, is always returned
  in lowercase.

  ## Examples

      iex> Path.split("")
      []

      iex> Path.split("foo")
      ["foo"]

      iex> Path.split("/foo/bar")
      ["/", "foo", "bar"]

  """
  @spec split(t) :: [binary]

  # Work around a bug in Erlang on UNIX
  def split(""), do: []

  def split(path) do
    :filename.split(IO.chardata_to_string(path))
  end

  defmodule Wildcard do
    @moduledoc false

    def read_link_info(file) do
      call({:read_link_info, file})
    end

    def list_dir(dir) do
      case call({:list_dir, dir}) do
        {:ok, files} ->
          {:ok, for(file <- files, hd(file) != ?., do: file)}

        other ->
          other
      end
    end

    @compile {:inline, call: 1}

    defp call(tuple) do
      x = :erlang.dt_spread_tag(true)
      y = :gen_server.call(:file_server_2, tuple)
      :erlang.dt_restore_tag(x)
      y
    end
  end

  @doc """
  Traverses paths according to the given `glob` expression and returns a
  list of matches.

  The wildcard looks like an ordinary path, except that the following
  "wildcard characters" are interpreted in a special way:

    * `?` - matches one character.

    * `*` - matches any number of characters up to the end of the filename, the
      next dot, or the next slash.

    * `**` - two adjacent `*`'s used as a single pattern will match all
      files and zero or more directories and subdirectories.

    * `[char1,char2,...]` - matches any of the characters listed; two
      characters separated by a hyphen will match a range of characters.
      Do not add spaces before and after the comma as it would then match
      paths containing the space character itself.

    * `{item1,item2,...}` - matches one of the alternatives.
      Do not add spaces before and after the comma as it would then match
      paths containing the space character itself.

  Other characters represent themselves. Only paths that have
  exactly the same character in the same position will match. Note
  that matching is case-sensitive: `"a"` will not match `"A"`.

  Directory separators must always be written as `/`, even on Windows.
  You may call `Path.expand/1` to normalize the path before invoking
  this function.

  By default, the patterns `*` and `?` do not match files starting
  with a dot `.`. See the `:match_dot` option in the "Options" section
  below.

  ## Options

    * `:match_dot` - (boolean) if `false`, the special wildcard characters `*` and `?`
      will not match files starting with a dot (`.`). If `true`, files starting with
      a `.` will not be treated specially. Defaults to `false`.

  ## Examples

  Imagine you have a directory called `projects` with three Elixir projects
  inside of it: `elixir`, `ex_doc`, and `plug`. You can find all `.beam` files
  inside the `ebin` directory of each project as follows:

      Path.wildcard("projects/*/ebin/**/*.beam")

  If you want to search for both `.beam` and `.app` files, you could do:

      Path.wildcard("projects/*/ebin/**/*.{beam,app}")

  """
  @spec wildcard(t, keyword) :: [binary]
  def wildcard(glob, opts \\ []) do
    mod = if Keyword.get(opts, :match_dot), do: :file, else: Path.Wildcard

    glob
    |> chardata_to_list!()
    |> :filelib.wildcard(mod)
    |> Enum.map(&IO.chardata_to_string/1)
  end

  defp chardata_to_list!(chardata) do
    case :unicode.characters_to_list(chardata) do
      result when is_list(result) ->
        if 0 in result do
          raise ArgumentError,
                "cannot execute Path.wildcard/2 for path with null byte, got: #{inspect(chardata)}"
        else
          result
        end

      {:error, encoded, rest} ->
        raise UnicodeConversionError, encoded: encoded, rest: rest, kind: :invalid

      {:incomplete, encoded, rest} ->
        raise UnicodeConversionError, encoded: encoded, rest: rest, kind: :incomplete
    end
  end

  defp expand_home(type) do
    case IO.chardata_to_string(type) do
      "~" <> rest -> resolve_home(rest)
      rest -> rest
    end
  end

  defp resolve_home(""), do: System.user_home!()

  defp resolve_home(rest) do
    case {rest, major_os_type()} do
      {"\\" <> _, :win32} ->
        System.user_home!() <> rest

      {"/" <> _, _} ->
        System.user_home!() <> rest

      _ ->
        rest
    end
  end

  # expand_dot the given path by expanding "..", "." and "~".
  defp expand_dot(<<"/", rest::binary>>), do: "/" <> do_expand_dot(rest)

  defp expand_dot(<<letter, ":/", rest::binary>>) when letter in ?a..?z,
    do: <<letter, ":/">> <> do_expand_dot(rest)

  defp expand_dot(path), do: do_expand_dot(path)

  defp do_expand_dot(path), do: do_expand_dot(:binary.split(path, "/", [:global]), [])
  defp do_expand_dot([".." | t], [_, _ | acc]), do: do_expand_dot(t, acc)
  defp do_expand_dot([".." | t], []), do: do_expand_dot(t, [])
  defp do_expand_dot(["." | t], acc), do: do_expand_dot(t, acc)
  defp do_expand_dot([h | t], acc), do: do_expand_dot(t, ["/", h | acc])
  defp do_expand_dot([], []), do: ""
  defp do_expand_dot([], ["/" | acc]), do: IO.iodata_to_binary(:lists.reverse(acc))

  defp major_os_type do
    :os.type() |> elem(0)
  end
end
