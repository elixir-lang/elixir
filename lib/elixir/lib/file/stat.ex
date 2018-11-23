require Record

defmodule File.Stat do
  @moduledoc """
  A struct that holds file information.

  In Erlang, this struct is represented by a `:file_info` record.
  Therefore this module also provides functions for converting
  between the Erlang record and the Elixir struct.

  Its fields are:

    * `size` - size of file in bytes.

    * `type` - `:device | :directory | :regular | :other | :symlink`; the type of the
      file.

    * `access` - `:read | :write | :read_write | :none`; the current system
      access to the file.

    * `atime` - the last time the file was read.

    * `mtime` - the last time the file was written.

    * `ctime` - the interpretation of this time field depends on the operating
      system. On Unix, it is the last time the file or the inode was changed.
      In Windows, it is the time of creation.

    * `mode` - the file permissions.

    * `links` - the number of links to this file. This is always 1 for file
      systems which have no concept of links.

    * `major_device` - identifies the file system where the file is located.
      In Windows, the number indicates a drive as follows: 0 means A:, 1 means
      B:, and so on.

    * `minor_device` - only valid for character devices on Unix. In all other
      cases, this field is zero.

    * `inode` - gives the inode number. On non-Unix file systems, this field
      will be zero.

    * `uid` - indicates the owner of the file. Will be zero for non-Unix file
      systems.

    * `gid` - indicates the group that owns the file. Will be zero for
      non-Unix file systems.

  The time type returned in `atime`, `mtime`, and `ctime` is dependent on the
  time type set in options. `{:time, type}` where type can be `:local`,
  `:universal`, or `:posix`. Default is `:universal`.
  """

  record = Record.extract(:file_info, from_lib: "kernel/include/file.hrl")
  keys = :lists.map(&elem(&1, 0), record)
  vals = :lists.map(&{&1, [], nil}, keys)
  pairs = :lists.zip(keys, vals)

  defstruct keys

  @type t :: %__MODULE__{
          size: non_neg_integer(),
          type: :device | :directory | :regular | :other | :symlink,
          access: :read | :write | :read_write | :none,
          atime: :calendar.datetime(),
          mtime: :calendar.datetime(),
          ctime: :calendar.datetime(),
          mode: non_neg_integer(),
          links: non_neg_integer(),
          major_device: non_neg_integer(),
          minor_device: non_neg_integer(),
          inode: non_neg_integer(),
          uid: non_neg_integer(),
          gid: non_neg_integer()
        }

  @doc """
  Converts a `File.Stat` struct to a `:file_info` record.
  """
  def to_record(%File.Stat{unquote_splicing(pairs)}) do
    {:file_info, unquote_splicing(vals)}
  end

  @doc """
  Converts a `:file_info` record into a `File.Stat`.
  """
  def from_record(file_info)

  def from_record({:file_info, unquote_splicing(vals)}) do
    %File.Stat{unquote_splicing(pairs)}
  end
end
