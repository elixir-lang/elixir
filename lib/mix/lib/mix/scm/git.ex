defmodule Mix.SCM.Git.Format do
  @moduledoc false

  # Helper module to encapsulate formatting functions

  @doc false
  def format(dep, { :git, _repo, rev, _opts }=lock) when is_binary(rev) do
    String.slice(rev, 0, 7)
    <> working_dir_mark(dep)
    <> " "
    <> get_lock_refs(dep, lock)
  end

  def format(_, _), do: ""

  ###

  # Returns "*" iff the working dir is dirty
  defp working_dir_mark(dep) do
    Mix.Deps.in_dependency(dep, fn _ ->
      case System.cmd("git status -s") do
        "" -> ""
        _  -> "*"
      end
    end)
  end

  ###

  # Returns a string with refs that point to `rev`
  # For example: "(tags: 0.1, 0.2, 0.3)", "(origin/master)"
  defp get_lock_refs(dep, { :git, _repo, rev, _opts }) do
    import Enum

    Mix.Deps.in_dependency(dep, fn _ ->
      System.cmd("git show-ref")                               # print all refs with commit hashes
      |> String.split("\n")
      |> filter(&String.starts_with?(&1, rev))                 # leave those refs that point to our commit (rev)
      |> map(fn s -> [_, ref] = String.split(s, " "); ref end) # strip the commit hash
      |> refs_to_string
    end)
  end

  # Helpers for get_lock_refs()

  defrecordp :lock_refs, [tags: [], refs: []]

  defp refs_to_string(refs) do
    refs_to_string(refs, lock_refs())
  end

  # Collect

  defp refs_to_string(["refs/heads/" <> name | t], lock_refs(refs: refs)=r) do
    refs_to_string(t, lock_refs(r, refs: prepend_local(name, refs)))
  end

  defp refs_to_string(["refs/remotes/" <> name | t], lock_refs(refs: refs)=r) do
    refs_to_string(t, lock_refs(r, refs: prepend_remote(name, refs)))
  end

  defp refs_to_string(["refs/tags/" <> name | t], lock_refs(tags: tags)=r) do
    refs_to_string(t, lock_refs(r, tags: [name | tags]))
  end

  # Format

  defp refs_to_string([], lock_refs(tags: [], refs: [])) do
    ""
  end

  defp refs_to_string([], lock_refs(tags: [], refs: refs)) do
    "(" <> Enum.join(refs, ", ") <> ")"
  end

  defp refs_to_string([], lock_refs(tags: tags)) do
    "(" <> map_tags(tags) <> ")"
  end

  ###

  defp prepend_local(name, list) do
    cond do
      # HEAD is not useful
      String.ends_with?(name, "HEAD") ->
        list

      # If there is a remote with the same name, throw the local name away
      Enum.any?(list, fn ref -> String.ends_with?(ref, name) end) ->
        list

      true ->
        [name | list]
    end
  end

  defp prepend_remote(name, list) do
    if String.ends_with?(name, "HEAD") do
      list
    else
      filter_remote(name, list)
    end
  end

  defp filter_remote(name, list) do
    [_, localname] = String.split(name, "/")
    case Enum.find_index(list, &(&1 == localname)) do
      nil -> [name | list]
      n   -> [name | List.delete_at(list, n)]
    end
  end

  ###

  defp map_tags([tag]) do
    "tag: " <> tag
  end

  defp map_tags(tags) do
    "tags: " <> Enum.join(tags, ", ")
  end
end

defmodule Mix.SCM.Git do
  @behavior Mix.SCM
  @moduledoc false

  def format(opts) do
    opts[:git]
  end

  def format_lock(dep, lock) do
    Mix.SCM.Git.Format.format(dep, lock)
  end

  def accepts_options(_app, opts) do
    cond do
      gh = opts[:github] ->
        opts |> Keyword.delete(:github) |> Keyword.put(:git, "git://github.com/#{gh}.git")
      opts[:git] ->
        opts
      true ->
        nil
    end
  end

  def checked_out?(opts) do
    File.dir?(Path.join(opts[:dest], ".git"))
  end

  def lock_status(opts) do
    case opts[:lock] do
      { :git, lock_repo, lock_rev, lock_opts } ->
        File.cd!(opts[:dest], fn ->
          cond do
            lock_repo != opts[:git] -> :outdated
            lock_opts != get_lock_opts(opts) -> :outdated
            lock_rev  != get_rev -> :mismatch
            true -> :ok
          end
        end)
      nil ->
        :mismatch
      _ ->
        :outdated
    end
  end

  def equal?(opts1, opts2) do
    get_lock(opts1, false) == get_lock(opts2, false)
  end

  def checkout(opts) do
    path     = opts[:dest]
    location = location(opts[:git])
    command  = %s(git clone --no-checkout --progress "#{location}" "#{path}")

    run_cmd_or_raise(command)
    File.cd! path, fn -> do_checkout(opts) end
  end

  def update(opts) do
    File.cd! opts[:dest], fn ->
      command = "git fetch --force --progress"
      if opts[:tag] do
        command = command <> " --tags"
      end

      run_cmd_or_raise(command)
      do_checkout(opts)
    end
  end

  ## Helpers

  defp location("git://github.com/" <> rest) do
    if System.get_env("MIX_GIT_FORCE_HTTPS") == "1" do
      "https://github.com/" <> rest
    else
      "git://github.com/" <> rest
    end
  end

  defp location(other), do: other

  defp do_checkout(opts) do
    ref = get_lock_rev(opts[:lock]) || get_opts_rev(opts)
    run_cmd_or_raise "git checkout --quiet #{ref}"

    if opts[:submodules] do
      run_cmd_or_raise "git submodule update --init --recursive"
    end

    get_lock(opts, true)
  end

  defp get_lock(opts, fresh) do
    lock = if fresh, do: get_rev, else: get_lock_rev(opts[:lock])
    { :git, opts[:git], lock, get_lock_opts(opts) }
  end

  defp get_lock_rev({ :git, _repo, lock, _opts }) when is_binary(lock), do: lock
  defp get_lock_rev(_), do: nil

  defp get_lock_opts(opts) do
    lock_opts = Enum.find_value [:branch, :ref, :tag], &List.keyfind(opts, &1, 0)
    lock_opts = List.wrap(lock_opts)
    if opts[:submodules] do
      lock_opts ++ [submodules: true]
    else
      lock_opts
    end
  end

  defp get_opts_rev(opts) do
    if branch = opts[:branch] do
      "origin/#{branch}"
    else
      opts[:ref] || opts[:tag] || "origin/master"
    end
  end

  defp get_rev do
    check_rev System.cmd('git rev-parse --verify --quiet HEAD')
  end

  defp check_rev([]),   do: nil
  defp check_rev(list), do: check_rev(list, [])

  defp check_rev([h|t], acc) when h in ?a..?f or h in ?0..?9 do
    check_rev(t, [h|acc])
  end

  defp check_rev(fin, acc) when fin == [?\n] or fin == [] do
    Enum.reverse(acc) |> iolist_to_binary
  end

  defp check_rev(_, _) do
    nil
  end

  defp run_cmd_or_raise(command) do
    if Mix.shell.cmd(command) != 0 do
      raise Mix.Error, message: "Command `#{command}` failed"
    end
    true
  end
end
