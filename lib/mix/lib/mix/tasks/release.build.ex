defmodule Mix.Tasks.Release.Build do
  use Mix.Task

  defrecord App, vsn: nil, lib_dir: nil,
                 modules: [],
                 mod_cond: :derived,
                 incl_cond: :include,
                 debug_info: :keep,
                 app_file: :strip,
                 excl_lib: nil,
                 incl_app_filters: [".*"],
                 excl_app_filters: [],
                 incl_archive_filters: [".*"],
                 excl_archive_filters: ["^include$","^priv$"],
                 archive_opts: [] do

     def to_reltool(rec) do
       [
          {:mod_cond, mod_cond(rec)},
          {:incl_cond, incl_cond(rec)},
          {:app_file, app_file(rec)},
          {:debug_info, debug_info(rec)},
          {:incl_app_filters, (lc f inlist incl_app_filters(rec), do: to_char_list(f))},
          {:excl_app_filters, (lc f inlist excl_app_filters(rec), do: to_char_list(f))},
          {:incl_archive_filters, (lc f inlist incl_archive_filters(rec), do: to_char_list(f))},
          {:excl_archive_filters, (lc f inlist excl_archive_filters(rec), do: to_char_list(f))},
          {:archive_opts, archive_opts(rec)}
        ] ++
        (lc {mod, opts} inlist modules(rec), do: {:mod, mod, opts}) ++
        (if excl_lib(rec), do: [{:excl_lib, excl_lib(rec)}], else: []) ++
        (if lib_dir(rec), do: [{:lib_dir, to_char_list(lib_dir(rec))}], else: [])
     end
  end                 

  defrecord Rel, name: nil, vsn: nil, apps: [] do
    def to_reltool(rec) do
      {:rel, to_char_list(name(rec)), to_char_list(vsn(rec)), apps(rec)}
    end
  end

  # TODO
  defrecord Escript, file: nil


  defrecord Sys, apps: [], escripts: [],
                 releases: [],
                 root_dir: nil, 
                 lib_dirs: [],
                 profile: :embedded,
                 erts: Mix.Tasks.Release.Build.App.new(),
                 mod_cond: :derived,
                 incl_cond: :include,
                 boot_rel: nil,
                 relocatable: true,
                 app_file: :strip,
                 debug_info: :keep,
                 incl_sys_filters: [".*"],
                 excl_sys_filters: [],
                 incl_app_filters: [".*"],
                 excl_app_filters: [],
                 incl_archive_filters: [".*"],
                 excl_archive_filters: ["^include$","^priv$"],
                 archive_opts: [] do

    def to_reltool(rec) do
      {:sys, [
          {:erts, Mix.Tasks.Release.Build.App.to_reltool(erts(rec))},
          {:profile, profile(rec)},
          {:mod_cond, mod_cond(rec)},
          {:incl_cond, incl_cond(rec)},
          {:boot_rel, to_char_list(boot_rel(rec))},
          {:relocatable, relocatable(rec)},
          {:app_file, app_file(rec)},
          {:debug_info, debug_info(rec)},
          {:incl_sys_filters, (lc f inlist incl_sys_filters(rec), do: to_char_list(f))},
          {:excl_sys_filters, (lc f inlist excl_sys_filters(rec), do: to_char_list(f))},
          {:incl_app_filters, (lc f inlist incl_app_filters(rec), do: to_char_list(f))},
          {:excl_app_filters, (lc f inlist excl_app_filters(rec), do: to_char_list(f))},
          {:incl_archive_filters, (lc f inlist incl_archive_filters(rec), do: to_char_list(f))},
          {:excl_archive_filters, (lc f inlist excl_archive_filters(rec), do: to_char_list(f))},
          {:archive_opts, archive_opts(rec)}
        ] ++ 
        (lc rel inlist releases(rec), do: Mix.Tasks.Release.Build.Rel.new(rel).to_reltool) ++        
        (lc {app_name, app} inlist apps(rec), do: {:app, app_name, Mix.Tasks.Release.Build.App.new(app).to_reltool}) ++
        (if root_dir(rec), do: [{:root_dir, to_char_list(root_dir(rec))}], else: [])
      }      
    end

    defoverridable boot_rel: 1
    def boot_rel(rec) do
      super(rec) || Mix.Tasks.Release.Build.Rel.new(hd(releases(rec))).name
    end

  end                 

  @shortdoc "Build a release"

  def run(_) do
    project = Mix.Project.get!
    release = Mix.Tasks.Release.Build.Sys.new(project.release)
    sys = Mix.Tasks.Release.Build.Sys.to_reltool(release)
    {:ok, server} = :reltool.start_server([sys])
    {:ok, spec} = :reltool.get_target_spec(server)

    :ok = :reltool.eval_target_spec(spec, Mix.project[:root_dir] || :code.root_dir, 
                                          to_char_list(Mix.project[:release_directory] || "release"))
  end
end
