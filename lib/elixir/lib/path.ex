defmodule Path do
  @moduledoc """
  This module provides conveniences for manipulating or
  retrieving file system paths.

  The functions in this module may receive a char data as
  argument (i.e. a string or a list of characters / string)
  and will always return a string (encoded in UTF-8).

  The majority of the functions in this module do not
  interact with the file system, except for a few functions
  that require it (like `wildcard/1` and `expand/1`).
  """

  alias :filename, as: FN
  @type t :: :unicode.chardata()

  @doc """
  Converts the given path to an absolute one. Unlike
  `expand/1`, no attempt is made to resolve `..`, `.` or `~`.

  ## Unix examples

      Path.absname("foo")
      #=> "/usr/local/foo"

      Path.absname("../x")
      #=> "/usr/local/../x"

  ## Windows

      Path.absname("foo").
      "D:/usr/local/foo"
      Path.absname("../x").
      "D:/usr/local/../x"

  """
  @spec absname(t) :: binary
  def absname(path) do
    absname(path, System.cwd!)
  end

  @doc """
  Builds a path from `relative_to` to `path`. If `path` is already
  an absolute path, `relative_to` is ignored. See also `relative_to/2`.

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
      :relative -> join(relative_to, path)
      :absolute ->
        cond do
          path == "/" ->
            path
          :binary.last(path) == ?/ ->
            binary_part(path, 0, byte_size(path) - 1)
          true ->
            path
        end
      :volumerelative ->
        relative_to = IO.chardata_to_string(relative_to)
        absname_vr(split(path), split(relative_to), relative_to)
    end
  end

  ## Absolute path on current drive
  defp absname_vr(["/"|rest], [volume|_], _relative),
    do: join([volume|rest])

  ## Relative to current directory on current drive.
  defp absname_vr([<<x, ?:>>|rest], [<<x, _ :: binary>>|_], relative),
    do: absname(join(rest), relative)

  ## Relative to current directory on another drive.
  defp absname_vr([<<x, ?:>>|name], _, _relative) do
    cwd =
      case :file.get_cwd([x, ?:]) do
        {:ok, dir}  -> IO.chardata_to_string(dir)
        {:error, _} -> <<x, ?:, ?/>>
      end
    absname(join(name), cwd)
  end

  @doc """
  Converts the path to an absolute one and expands
  any `.` and `..` characters and a leading `~`.

  ## Examples

      Path.expand("/foo/bar/../bar")
      "/foo/bar"

  """
  @spec expand(t) :: binary
  def expand(path) do
    normalize absname(expand_home(path), System.cwd!)
  end

  @doc """
  Expands the path relative to the path given as the second argument
  expanding any `.` and `..` characters. If the path is already an
  absolute path, `relative_to` is ignored.

  Note, that this function treats `path` with a leading `~` as
  an absolute one.

  The second argument is first expanded to an absolute path.

  ## Examples

      # Assuming that the absolute path to baz is /quux/baz
      Path.expand("foo/bar/../bar", "baz")
      #=> "/quux/baz/foo/bar"

      Path.expand("foo/bar/../bar", "/baz")
      "/baz/foo/bar"
      Path.expand("/foo/bar/../bar", "/baz")
      "/foo/bar"

  """
  @spec expand(t, t) :: binary
  def expand(path, relative_to) do
    normalize absname(absname(expand_home(path), expand_home(relative_to)), System.cwd!)
  end

  @doc """
  Returns the path type.

  ## Unix examples

      Path.type("/")                #=> :absolute
      Path.type("/usr/local/bin")   #=> :absolute
      Path.type("usr/local/bin")    #=> :relative
      Path.type("../usr/local/bin") #=> :relative
      Path.type("~/file")           #=> :relative

  ## Windows examples

      Path.type("D:/usr/local/bin") #=> :absolute
      Path.type("usr/local/bin")    #=> :relative
      Path.type("D:bar.ex")         #=> :volumerelative
      Path.type("/bar/foo.ex")      #=> :volumerelative

  """
  @spec type(t) :: :absolute | :relative | :volumerelative
  def type(name) when is_list(name) or is_binary(name) do
    case :os.type() do
      {:win32, _} -> win32_pathtype(name)
      _           -> unix_pathtype(name)
    end |> elem(0)
  end

  @doc """
  Forces the path to be a relative path.

  ## Unix examples

      Path.relative("/usr/local/bin")   #=> "usr/local/bin"
      Path.relative("usr/local/bin")    #=> "usr/local/bin"
      Path.relative("../usr/local/bin") #=> "../usr/local/bin"

  ## Windows examples

      Path.relative("D:/usr/local/bin") #=> "usr/local/bin"
      Path.relative("usr/local/bin")    #=> "usr/local/bin"
      Path.relative("D:bar.ex")         #=> "bar.ex"
      Path.relative("/bar/foo.ex")      #=> "bar/foo.ex"

  """
  @spec relative(t) :: binary
  def relative(name) do
    case :os.type() do
      {:win32, _} -> win32_pathtype(name)
      _           -> unix_pathtype(name)
    end |> elem(1) |> IO.chardata_to_string
  end

  defp unix_pathtype(<<?/, relative :: binary>>), do:
    {:absolute, relative}
  defp unix_pathtype([?/|relative]), do:
    {:absolute, relative}
  defp unix_pathtype([list|rest]) when is_list(list), do:
    unix_pathtype(list ++ rest)
  defp unix_pathtype(relative), do:
    {:relative, relative}

  @slash [?/, ?\\]

  defp win32_pathtype([list|rest]) when is_list(list), do:
    win32_pathtype(list++rest)
  defp win32_pathtype([char, list|rest]) when is_list(list), do:
    win32_pathtype([char|list++rest])
  defp win32_pathtype(<<c1, c2, relative :: binary>>) when c1 in @slash and c2 in @slash, do:
    {:absolute, relative}
  defp win32_pathtype(<<c, relative :: binary>>) when c in @slash, do:
    {:volumerelative, relative}
  defp win32_pathtype(<<_letter, ?:, c, relative :: binary>>) when c in @slash, do:
    {:absolute, relative}
  defp win32_pathtype(<<_letter, ?:, relative :: binary>>), do:
    {:volumerelative, relative}

  defp win32_pathtype([c1, c2 | relative]) when c1 in @slash and c2 in @slash, do:
    {:absolute, relative}
  defp win32_pathtype([c | relative]) when c in @slash, do:
    {:volumerelative, relative}
  defp win32_pathtype([c1, c2, list|rest]) when is_list(list), do:
    win32_pathtype([c1, c2|list++rest])
  defp win32_pathtype([_letter, ?:, c | relative]) when c in @slash, do:
    {:absolute, relative}
  defp win32_pathtype([_letter, ?: | relative]), do:
    {:volumerelative, relative}
  defp win32_pathtype(relative), do:
    {:relative, relative}

  @doc """
  Returns the given `path` relative to the given `from` path.
  In other words, it tries to strip the `from` prefix from `path`.

  This function does not query the file system, so it assumes
  no symlinks in between the paths.

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

  defp relative_to([h|t1], [h|t2], original) do
    relative_to(t1, t2, original)
  end

  defp relative_to([_|_] = l1, [], _original) do
    join(l1)
  end

  defp relative_to(_, _, original) do
    original
  end

  @doc """
  Convenience to get the path relative to the current working
  directory. If, for some reason, the current working directory
  cannot be retrieved, returns the full path.
  """
  @spec relative_to_cwd(t) :: binary
  def relative_to_cwd(path) do
    case :file.get_cwd do
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
    FN.basename(IO.chardata_to_string(path))
  end

  @doc """
  Returns the last component of `path` with the `extension`
  stripped. This function should be used to remove a specific
  extension which may, or may not, be there.

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
    FN.basename(IO.chardata_to_string(path), IO.chardata_to_string(extension))
  end

  @doc """
  Returns the directory component of `path`.

  ## Examples

      Path.dirname("/foo/bar.ex")
      #=> "/foo"
      Path.dirname("/foo/bar/baz.ex")
      #=> "/foo/bar"

  """
  @spec dirname(t) :: binary
  def dirname(path) do
    FN.dirname(IO.chardata_to_string(path))
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
    FN.extension(IO.chardata_to_string(path))
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
    FN.rootname(IO.chardata_to_string(path))
  end

  @doc """
  Returns the `path` with the `extension` stripped. This function should be used to
  remove a specific extension which might, or might not, be there.

  ## Examples

      iex> Path.rootname("/foo/bar.erl", ".erl")
      "/foo/bar"

      iex> Path.rootname("/foo/bar.erl", ".ex")
      "/foo/bar.erl"

  """
  @spec rootname(t, t) :: binary
  def rootname(path, extension) do
    FN.rootname(IO.chardata_to_string(path), IO.chardata_to_string(extension))
  end

  @doc """
  Returns a string with one or more path components joined by the path separator.

  This function should be used to convert a list of strings to a path.
  Note that any trailing slash is removed on join.

  ## Examples

      iex> Path.join(["~", "foo"])
      "~/foo"

      iex> Path.join(["foo"])
      "foo"

      iex> Path.join(["/", "foo", "bar/"])
      "/foo/bar"

  """
  @spec join([t]) :: binary
  def join([name1, name2|rest]), do:
    join([join(name1, name2)|rest])
  def join([name]), do:
    do_join(IO.chardata_to_string(name), <<>>, [], major_os_type())

  @doc """
  Joins two paths.

  ## Examples

      iex> Path.join("foo", "bar")
      "foo/bar"

  """
  @spec join(t, t) :: binary
  def join(left, right),
    do: do_join(IO.chardata_to_string(left), relative(right), [], major_os_type())

  defp major_os_type do
    :os.type |> elem(0)
  end

  defp do_join(<<uc_letter, ?:, rest :: binary>>, relativename, [], :win32) when uc_letter in ?A..?Z, do:
    do_join(rest, relativename, [?:, uc_letter+?a-?A], :win32)
  defp do_join(<<?\\, rest :: binary>>, relativename, result, :win32), do:
    do_join(<<?/, rest :: binary>>, relativename, result, :win32)
  defp do_join(<<?/, rest :: binary>>, relativename, [?., ?/|result], os_type), do:
    do_join(rest, relativename, [?/|result], os_type)
  defp do_join(<<?/, rest :: binary>>, relativename, [?/|result], os_type), do:
    do_join(rest, relativename, [?/|result], os_type)
  defp do_join(<<>>, <<>>, result, os_type), do:
    IO.iodata_to_binary(maybe_remove_dirsep(result, os_type))
  defp do_join(<<>>, relativename, [?:|rest], :win32), do:
    do_join(relativename, <<>>, [?:|rest], :win32)
  defp do_join(<<>>, relativename, [?/|result], os_type), do:
    do_join(relativename, <<>>, [?/|result], os_type)
  defp do_join(<<>>, relativename, result, os_type), do:
    do_join(relativename, <<>>, [?/|result], os_type)
  defp do_join(<<char, rest :: binary>>, relativename, result, os_type), do:
    do_join(rest, relativename, [char|result], os_type)

  defp maybe_remove_dirsep([?/, ?:, letter], :win32), do:
    [letter, ?:, ?/]
  defp maybe_remove_dirsep([?/], _), do:
    [?/]
  defp maybe_remove_dirsep([?/|name], _), do:
    :lists.reverse(name)
  defp maybe_remove_dirsep(name, _), do:
    :lists.reverse(name)

  @doc """
  Returns a list with the path split by the path separator.
  If an empty string is given, returns the root path.

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
    FN.split(IO.chardata_to_string(path))
  end

  defmodule Wildcard do
    @moduledoc false

    def read_file_info(file) do
      call({:read_link_info, file})
    end

    def list_dir(dir) do
      case call({:list_dir, dir})  do
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
  Traverses paths according to the given `glob` expression.

  The wildcard looks like an ordinary path, except that certain
  "wildcard characters" are interpreted in a special way. The
  following characters are special:

    * `?` - matches one character

    * `*` - matches any number of characters up to the end of the filename, the
      next dot, or the next slash

    * `**` - two adjacent `*`'s used as a single pattern will match all
      files and zero or more directories and subdirectories

    * `[char1,char2,...]` - matches any of the characters listed; two
      characters separated by a hyphen will match a range of characters

    * `{item1,item2,...}` - matches one of the alternatives

  Other characters represent themselves. Only paths that have
  exactly the same character in the same position will match. Note
  that matching is case-sensitive; i.e. "a" will not match "A".

  By default, the patterns `*` and `?` do not match files starting
  with a dot `.` unless `match_dot: true` is given.

  ## Examples

  Imagine you have a directory called `projects` with three Elixir projects
  inside of it: `elixir`, `ex_doc` and `dynamo`. You can find all `.beam` files
  inside the `ebin` directory of each project as follows:

      Path.wildcard("projects/*/ebin/**/*.beam")

  If you want to search for both `.beam` and `.app` files, you could do:

      Path.wildcard("projects/*/ebin/**/*.{beam,app}")

  """
  @spec wildcard(t) :: [binary]
  def wildcard(glob, opts \\ []) do
    mod = if Keyword.get(opts, :match_dot), do: :file, else: Path.Wildcard
    glob
    |> chardata_to_list()
    |> :filelib.wildcard(mod)
    |> Enum.map(&IO.chardata_to_string/1)
  end

  # Normalize the given path by expanding "..", "." and "~".

  defp chardata_to_list(chardata) do
    case :unicode.characters_to_list(chardata) do
      result when is_list(result) ->
        result

      {:error, encoded, rest} ->
        raise UnicodeConversionError, encoded: encoded, rest: rest, kind: :invalid

      {:incomplete, encoded, rest} ->
        raise UnicodeConversionError, encoded: encoded, rest: rest, kind: :incomplete
    end
  end

  defp expand_home(type) do
    case IO.chardata_to_string(type) do
      "~" <> rest -> System.user_home! <> rest
      rest        -> rest
    end
  end

  defp normalize(path), do: normalize(split(path), [])

  defp normalize([".."|t], ["/"|_] = acc) do
    normalize t, acc
  end

  defp normalize([".."|t], [<<letter, ?:, ?/>>|_] = acc) when letter in ?a..?z do
    normalize t, acc
  end

  defp normalize([".."|t], [_|acc]) do
    normalize t, acc
  end

  defp normalize(["."|t], acc) do
    normalize t, acc
  end

  defp normalize([h|t], acc) do
    normalize t, [h|acc]
  end

  defp normalize([], acc) do
    join :lists.reverse(acc)
  end
end
