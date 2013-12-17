import Kernel, except: [to_char_list: 1]

defmodule Path do
  @moduledoc """
  This module provides conveniences for manipulating or
  retrieving file system paths.

  The functions in this module may receive a char list or
  a binary as an argument and will return a value of the same
  type.

  The majority of the functions in this module do not
  interact with the file system, except for a few functions
  that require it (like `wildcard/1` and `expand/1`).
  """

  alias :filename, as: FN

  @typep deep_list :: [char | atom | deep_list]
  @type t :: char_list | atom | binary | deep_list
  @type encoding :: :utf8 | :latin1

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
  @spec absname(t) :: t
  def absname(path) do
    FN.absname(path, get_cwd(path))
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
  @spec absname(t) :: t
  def absname(path, relative_to) do
    FN.absname(path, relative_to)
  end

  @doc """
  Converts the path to an absolute one and expands
  any `.` and `..` characters and a leading `~`.

  ## Examples

      Path.expand("/foo/bar/../bar")
      "/foo/bar"

  """
  @spec expand(t) :: t
  def expand(path) do
    normalize FN.absname(expand_home(path), get_cwd(path))
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
  @spec expand(t, t) :: t
  def expand(path, relative_to) do
    normalize FN.absname(FN.absname(expand_home(path), expand_home(relative_to)), get_cwd(path))
  end

  @doc """
  Returns the path type.

  ## Unix examples

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
  @spec type(t) :: :absolute | :relative
  def type(name) do
    case :os.type() do
      { :win32, _ } -> win32_pathtype(name)
      _             -> unix_pathtype(name)
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
  @spec relative(t) :: t
  def relative(name) do
    case :os.type() do
      { :win32, _ } -> win32_pathtype(name)
      _             -> unix_pathtype(name)
    end |> elem(1)
  end

  defp unix_pathtype(<<?/, relative :: binary>>), do:
    { :absolute, relative }
  defp unix_pathtype([?/|relative]), do:
    { :absolute, relative }
  defp unix_pathtype([list|rest]) when is_list(list), do:
    unix_pathtype(list ++ rest)
  defp unix_pathtype([atom|rest]) when is_atom(atom), do:
    unix_pathtype(atom_to_list(atom) ++ rest)
  defp unix_pathtype(relative), do:
    { :relative, relative }

  @slash [?/, ?\\]

  defp win32_pathtype([list|rest]) when is_list(list), do:
    win32_pathtype(list++rest)
  defp win32_pathtype([atom|rest]) when is_atom(atom), do:
    win32_pathtype(atom_to_list(atom)++rest)
  defp win32_pathtype([char, list|rest]) when is_list(list), do:
    win32_pathtype([char|list++rest])
  defp win32_pathtype(<<c1, c2, relative :: binary>>) when c1 in @slash and c2 in @slash, do:
    { :absolute, relative }
  defp win32_pathtype(<<c, relative :: binary>>) when c in @slash, do:
    { :volumerelative, relative }
  defp win32_pathtype(<<_letter, ?:, c, relative :: binary>>) when c in @slash, do:
    { :absolute, relative }
  defp win32_pathtype(<<_letter, ?:, relative :: binary>>), do:
    { :volumerelative, relative }

  defp win32_pathtype([c1, c2 | relative]) when c1 in @slash and c2 in @slash, do:
    { :absolute, relative }
  defp win32_pathtype([c | relative]) when c in @slash, do:
    { :volumerelative, relative }
  defp win32_pathtype([c1, c2, list|rest]) when is_list(list), do:
    win32_pathtype([c1, c2|list++rest])
  defp win32_pathtype([_letter, ?:, c | relative]) when c in @slash, do:
    { :absolute, relative }
  defp win32_pathtype([_letter, ?: | relative]), do:
    { :volumerelative, relative }
  defp win32_pathtype(relative), do:
    { :relative, relative }

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
  @spec relative_to(t, t) :: t
  def relative_to(path, from) when is_list(path) and is_binary(from) do
    path = to_binary!(path)
    relative_to(FN.split(path), FN.split(from), path)
  end

  def relative_to(path, from) when is_binary(path) and is_list(from) do
    relative_to(FN.split(path), FN.split(to_binary!(from)), path)
  end

  def relative_to(path, from) do
    relative_to(FN.split(path), FN.split(from), path)
  end

  defp relative_to([h|t1], [h|t2], original) do
    relative_to(t1, t2, original)
  end

  defp relative_to([_|_] = l1, [], _original) do
    FN.join(l1)
  end

  defp relative_to(_, _, original) do
    original
  end

  @doc """
  Convenience to get the path relative to the current working
  directory. If, for some reason, the current working directory
  cannot be retrieved, returns the full path.
  """
  @spec relative_to_cwd(t) :: t
  def relative_to_cwd(path) do
    case :file.get_cwd do
      { :ok, base } -> relative_to(path, base)
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
  @spec basename(t) :: t
  def basename(path) do
    FN.basename(path)
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
  @spec basename(t, t) :: t
  def basename(path, extension) do
    FN.basename(path, extension)
  end

  @doc """
  Returns the directory component of `path`.

  ## Examples

      Path.dirname("/foo/bar.ex")
      #=> "/foo"
      Path.dirname("/foo/bar/baz.ex")
      #=> "/foo/bar"

  """
  @spec dirname(t) :: t
  def dirname(path) do
    FN.dirname(path)
  end

  @doc """
  Returns the extension of the last component of `path`.

  ## Examples

      iex> Path.extname("foo.erl")
      ".erl"
      iex> Path.extname("~/foo/bar")
      ""

  """
  @spec extname(t) :: t
  def extname(path) do
    FN.extension(path)
  end

  @doc """
  Returns the `path` with the `extension` stripped.

  ## Examples

      iex> Path.rootname("/foo/bar")
      "/foo/bar"
      iex> Path.rootname("/foo/bar.ex")
      "/foo/bar"

  """
  @spec rootname(t) :: t
  def rootname(path) do
    FN.rootname(path)
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
  @spec rootname(t, t) :: t
  def rootname(path, extension) do
    FN.rootname(path, extension)
  end

  @doc """
  Returns a string with one or more path components joined by the path separator.
  This function should be used to convert a list of strings to a path.

  ## Examples

      iex> Path.join(["~", "foo"])
      "~/foo"
      iex> Path.join(["foo"])
      "foo"
      iex> Path.join(["/", "foo", "bar"])
      "/foo/bar"

  """
  @spec join([t]) :: t
  def join([name1, name2|rest]), do:
    join([join(name1, name2)|rest])

  def join([name]) when is_binary(name), do:
    do_join(name, <<>>, [], major_os_type())

  def join([name]) do
    encoding = native_encoding()
    do_join(to_binary!(name, encoding), <<>>, [], major_os_type())
    |> to_char_list!(encoding)
  end

  @doc """
  Joins two paths.

  ## Examples

      iex> Path.join("foo", "bar")
      "foo/bar"

  """
  @spec join(t, t) :: t
  def join(left, right) when is_binary(left) and is_binary(right), do:
    do_join(left, Path.relative(right), [], major_os_type())

  def join(left, right) when is_binary(left) and is_list(right), do:
    join(left, to_binary!(right))

  def join(left, right) when is_list(left) and is_binary(right), do:
    join(to_binary!(left), right)

  def join(left, right) when is_list(left) and is_list(right) do
    encoding = native_encoding()
    join(to_binary!(left, encoding), to_binary!(right, encoding))
    |> to_char_list!(encoding)
  end

  def join(left, right) when is_atom(left), do:
    join(atom_to_list(left), right)

  def join(left, right) when is_atom(right), do:
    join(left, atom_to_list(right))

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
    iolist_to_binary(maybe_remove_dirsep(result, os_type))
  defp do_join(<<>>, relativename, [?:|rest], :win32), do:
    do_join(relativename, <<>>, [?:|rest], :win32)
  defp do_join(<<>>, relativename, [?/|result], os_type), do:
    do_join(relativename, <<>>, [?/|result], os_type)
  defp do_join(<<>>, relativename, result, os_type), do:
    do_join(relativename, <<>>, [?/|result], os_type)
  defp do_join(<<char, rest :: binary>>, relativename, result, os_type) when is_integer(char), do:
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
  # Work around a bug in Erlang on UNIX
  @spec split(t) :: [t]
  def split(""), do: []

  def split(path) do
    FN.split(path)
  end

  @doc """
  Traverses paths according to the given `glob` expression.

  The wildcard looks like an ordinary path, except that certain
  "wildcard characters" are interpreted in a special way. The
  following characters are special:

  * `?` - Matches one character.
  * `*` - Matches any number of characters up to the end of
          the filename, the next dot, or the next slash.
  * `**` - Two adjacent <c>*</c>'s used as a single pattern will
           match all files and zero or more directories and subdirectories.
  * `[char1,char2,...]` - Matches any of the characters listed. Two characters
                          separated by a hyphen will match a range of characters.
  * `{item1,item2,...}` - Matches one of the alternatives.

  Other characters represent themselves. Only paths that have
  exactly the same character in the same position will match. Note
  that matching is case-sensitive; i.e. "a" will not match "A".

  ## Examples

  Imagine you have a directory called `projects` with three Elixir projects
  inside of it: `elixir`, `ex_doc` and `dynamo`. You can find all `.beam` files
  inside the `ebin` directory of each project as follows:

      Path.wildcard("projects/*/ebin/**/*.beam")

  If you want to search for both `.beam` and `.app` files, you could do:

      Path.wildcard("projects/*/ebin/**/*.{beam,app}")

  """
  @spec wildcard(t) :: [t]
  def wildcard(glob) when is_binary(glob) do
    encoding = native_encoding()
    to_char_list!(glob, encoding)
    |> :filelib.wildcard
    |> Enum.map(&( from_char_list!(&1, encoding) ))
  end

  def wildcard(glob) when is_list(glob) or is_atom(glob) do
    :filelib.wildcard glob
  end

  @doc """
  Returns the VMs configured path encoding when using `char_list`'s and
  `atom`'s.

  The encoding used can be configured with the `--erl` VM flags `+fnl`, `+fnu`,
  `+fnuw`, `+fnue` and `+fnui`. `+fnl` will translate `char_list` and `atom`
  paths to latin-1. The other flags will translate `char_list` and `atom` paths
  to utf8. `+fnuw` (and `+fnu`) will send a warning to the error logger and skip
  files containing incorrectly encoded names when listing a directory. `+fnui`
  will silently ignore incorrectily encoded names. `+fnue` will return an error
  when an incorrectly encoded file or directory name is encountered.

  The default encoding is OS dependent. Mac and Windows will default to `:utf8`
  as their filesystems enforce unicode file and directory names.

  However most other OS's have a transparent filesystem which means that each
  file or directory is a list of bytes. It is left to the user/program to
  determine the encoding for themselves. On these platforms the default encoding
  is `:latin1`. UTF-8 encoded filenames can be accessed by using a raw binary
  string, but `char_list`'s and `atoms`'s will be treated as bytes lists so that
  every file on the system is accessible using each type. If all file and
  directory names are UTF-8 encoded then using one of the unicode flags will
  allow `char_list`'s and `atom`'s to represent code points instead of bytes.
  """
  @spec native_encoding() :: encoding
  defdelegate native_encoding(), to: :file, as: :native_name_encoding

  defexception ConversionError, encoded: nil, rest: nil, kind: nil, encoding: nil do
    def message(exception) do
      if exception.encoding do
        "#{exception.kind} #{exception.encoding} #{detail(exception.rest)}"
      else
        "#{exception.kind} #{detail(exception.rest)}"
      end
    end

    defp detail(rest) when is_binary(rest) do
      "encoding starting at #{inspect rest}"
    end

    defp detail([h|_]) when is_list(h) do
      detail(h)
    end

    defp detail([h|_]) do
      "code point #{h}"
    end
  end

  @doc """
  Converts a path into a `char_list`. If the path is a raw binary path its
  conversion uses the VM configured encoding.

  This and the other to/from functions in this module should be used when
  interacting with paths instead of `String.to_char_list/1` and the other
  string conversion functions.

  ## Examples

      iex> Path.to_char_list("raw/path")
      { :ok, 'raw/path' }
      iex> Path.to_char_list(['deep/', [[:nested], ['/path']]])
      { :ok, 'deep/nested/path' }
      iex> Path.to_char_list(:"atom/path")
      { :ok, 'atom/path' }
  """
  @spec to_char_list(t) ::
    { :ok, char_list } | { :error, list, binary } |
    { :incomplete, list, binary }
  def to_char_list(path) when is_binary(path) do
    to_char_list(path, native_encoding())
  end
  def to_char_list(path) when is_list(path) do
    { :ok, FN.flatten(path) }
  end
  def to_char_list(path) when is_atom(path) do
    { :ok, atom_to_list(path) }
  end

  @doc """
  Converts a path into a `char_list`. If the path is a raw binary path its
  conversion uses the supplied encoding.

  ## Examples

      iex> Path.to_char_list("raw/path", :latin1)
      { :ok, 'raw/path' }
      iex> Path.to_char_list(<<"invalid/utf8/path", 255>>, :utf8)
      { :error, 'invalid/utf8/path', <<255>> }

  """
  @spec to_char_list(t, encoding) ::
    { :ok, char_list } | { :error, list, binary } |
    { :incomplete, list, binary }
  def to_char_list(path, encoding) when is_binary(path) do
    case :unicode.characters_to_list(path, encoding) do
      char_list when is_list(char_list) ->
        { :ok, char_list }
      { :error, _, _ } = error ->
        error
      { :incomplete, _, _ } = incomplete ->
        incomplete
    end
  end

  def to_char_list(path, _encoding) do
    to_char_list(path)
  end

  @doc """
  Converts a path into a `char_list`. If the path is a raw binary path its
  conversion uses the VM configured encoding. A `Path.ConversionError` is raised
  when a conversion is not possible.

  ## Examples

      iex> Path.to_char_list!("raw/path")
      'raw/path'
      iex> Path.to_char_list!(:"atom/path")
      'atom/path'

  """
  @spec to_char_list!(t) :: char_list
  def to_char_list!(path) when is_binary(path) do
    to_char_list!(path, native_encoding())
  end

  def to_char_list!(path) do
    { :ok, char_list } = to_char_list(path)
    char_list
  end

  @doc """
  Converts a path to a `char_list`. If the path is a raw binary path its
  conversion the supplied encoding. A `Path.ConversionError` is raised when a
  conversion is not possible.

  ## Examples

      iex> Path.to_char_list!("raw/path", :latin1)
      'raw/path'
      iex> Path.to_char_list!(<<"invalid/utf8/path", 255>>, :utf8)
      ** (Path.ConversionError) invalid utf8 encoding starting at <<255>>

  """
  @spec to_char_list!(t, encoding) :: char_list
  def to_char_list!(path, encoding) do
    case to_char_list(path, encoding) do
      { :ok, char_list } ->
        char_list
      { :error, encoded, rest } ->
        raise ConversionError, encoded: encoded, rest: rest, kind: :invalid,
          encoding: encoding
      { :incomplete, encoded, rest } ->
        raise ConversionError, encoded: encoded, rest: rest, kind: :incomplete,
          encoding: encoding
    end
  end

  @doc """
  Converts a `char_list` into a raw binary path using the supplied encoding. If
  no encoding is supplied the VM configured encoding is used.

  ## Examples

      iex> Path.from_char_list('char/list/path')
      { :ok, "char/list/path" }
      iex> Path.from_char_list('path/with/invalid/code/point' ++ [256], :latin1)
      { :error, "path/with/invalid/code/point", [[256]] }

  """

  @spec from_char_list(char_list) ::
    { :ok, t } | { :error, binary, list } | { :incomplete, binary, list }
  @spec from_char_list(char_list, encoding) ::
    { :ok, t } | { :error, binary, list } | { :incomplete, binary, list }
  def from_char_list(char_list, encoding // native_encoding()) do
    case :unicode.characters_to_binary(char_list, encoding, encoding) do
      path when is_binary(path) ->
        { :ok, path }
      { :error, _, _ } = error ->
        error
      { :incomplete, _, _ } = incomplete ->
        incomplete
    end
  end

  @doc """
  Converts a `char_list` into a raw binary path using the supplied encoding. If
  no encoding is supplied the VM configured encoding is used. A
  `Path.ConversionError` is raised if the conversion fails.

  ## Examples

      iex> Path.from_char_list!('char/list/path')
      "char/list/path"
      iex> Path.from_char_list!('path/with/invalid/code/point' ++ [256], :latin1)
      ** (Path.ConversionError) invalid latin1 code point 256

  """

  @spec from_char_list!(char_list) :: t
  @spec from_char_list!(char_list, encoding) :: t
  def from_char_list!(char_list, encoding // native_encoding()) do
    case from_char_list(char_list, encoding) do
      { :ok, path } ->
        path
      { :error, encoded, rest } ->
        raise ConversionError, encoded: encoded, rest: rest, kind: :invalid,
          encoding: encoding
      { :incomplete, encoded, rest } ->
        raise ConversionError, encoded: encoded, rest: rest, kind: :incomplete,
          encoding: encoding
    end
  end

  @doc """
  Converts a path to a raw binary path. If the path is not a binary the
  conversion uses the VM configured encoding.

  ## Examples

      iex> Path.to_binary("raw/path")
      { :ok, "raw/path" }
      iex> Path.to_binary(:"atom/path")
      { :ok, "atom/path"}

  """
  @spec to_binary(t) ::
    { :ok, binary } | { :error, binary, list } | { :incomplete, binary, list }
  def to_binary(path) when is_binary(path) do
    { :ok, path }
  end

  def to_binary(path) when is_list(path) do
    FN.flatten(path)
    |> from_char_list
  end

  def to_binary(path) when is_atom(path) do
    atom_to_list(path)
    |> from_char_list
  end

  @doc """
  Converts a path to a raw binary path. If the path is not a binary the
  conversion uses the supplied encoding.

  ## Examples

      iex> Path.to_binary("raw/path", :utf8)
      { :ok, "raw/path" }
      iex> Path.to_binary('path/with/invalid/code/point' ++ [256], :latin1)
      { :error, "path/with/invalid/code/point", [[256]] }

  """
  @spec to_binary(t, encoding) ::
    { :ok, binary } | { :error, binary, list } | { :incomplete, binary, list }
  def to_binary(path, _encoding) when is_binary(path) do
    {:ok, path }
  end

  def to_binary(path, encoding) when is_list(path) do
    FN.flatten(path)
    |> from_char_list(encoding)
  end

  def to_binary(path, encoding) when is_atom(path) do
    atom_to_list(path)
    |> from_char_list(encoding)
  end

  @doc """
  Converts a path to a raw binary path. If the path is not a binary the
  conversion uses the VM configured encoding. A `Path.ConversionError` is raised
  if the conversion fails.

  ## Examples

      iex> Path.to_binary!("raw/path")
      "raw/path"
      iex> Path.to_binary!(:"atom/path")
      "atom/path"

  """
  @spec to_binary!(t) :: binary
  def to_binary!(path) when is_binary(path), do: path

  def to_binary!(path) do
    encoding = native_encoding()
    case to_binary(path, encoding) do
      { :ok, binary } ->
        binary
      { :error, encoded, rest } ->
        raise ConversionError, encoded: encoded, rest: rest, kind: :invalid,
          encoding: encoding
      { :incomplete, encoded, rest } ->
        raise ConversionError, encoded: encoded, rest: rest, kind: :incomplete,
          encoding: encoding
    end
  end

  @doc """
  Converts a path to a raw binary path. If the path is not a binary the
  conversion uses the supplied encoding. A `Path.ConversionError` is raised if
  the conversion fails.

  ## Examples

      iex> Path.to_binary!('char/list/path', :utf8)
      "char/list/path"
      iex> Path.to_binary!('path/with/invalid/code/point' ++ [256], :latin1)
      ** (Path.ConversionError) invalid latin1 code point 256

  """
  @spec to_binary!(t, encoding) :: binary
  def to_binary!(path, encoding) do
    case to_binary(path, encoding) do
      { :ok, binary } ->
        binary
      { :error, encoded, rest } ->
        raise ConversionError, encoded: encoded, rest: rest, kind: :invalid,
          encoding: encoding
      { :incomplete, encoded, rest } ->
        raise ConversionError, encoded: encoded, rest: rest, kind: :incomplete,
          encoding: encoding
    end
  end

  ## Helpers

  defp get_cwd(path) when is_binary(path), do: System.cwd!
  defp get_cwd(_), do: System.cwd! |> to_char_list!

  # Normalize the given path by expanding "..", "." and "~".

  defp expand_home(path), do: FN.flatten(path) |> do_expand_home

  defp do_expand_home(<<?~, rest :: binary>>) do
    System.user_home! <> rest
  end

  defp do_expand_home('~' ++ rest) do
    home = System.user_home!
    case to_char_list(home) do
      { :ok, home } ->
        home ++ rest
      { :error, _, _ } ->
        home <> to_binary!(rest)
      { :incomplete, _, _ } ->
        home <> to_binary!(rest)
    end
  end

  defp do_expand_home(other), do: other

  defp normalize(path), do: normalize(FN.split(path), [])

  defp normalize([top|t], [_|acc]) when top in ["..", '..'] do
    normalize t, acc
  end

  defp normalize([top|t], acc) when top in [".", '.'] do
    normalize t, acc
  end

  defp normalize([h|t], acc) do
    normalize t, [h|acc]
  end

  defp normalize([], acc) do
    join Enum.reverse(acc)
  end
end
