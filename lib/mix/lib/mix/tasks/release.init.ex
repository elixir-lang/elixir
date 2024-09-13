defmodule Mix.Tasks.Release.Init do
  use Mix.Task

  @shortdoc "Generates sample files for releases"

  @moduledoc """
  Generates sample files for releases.

      $ mix release.init
      * creating rel/vm.args.eex
      * creating rel/remote.vm.args.eex
      * creating rel/env.sh.eex
      * creating rel/env.bat.eex
  """

  import Mix.Generator

  @switches [
    force: :boolean,
    quiet: :boolean
  ]

  @aliases [
    force: :f
  ]

  @impl true
  def run(args) do
    {opts, args} = OptionParser.parse!(args, strict: @switches, aliases: @aliases)

    if args != [] do
      Mix.raise("Expected \"mix release.init\" without arguments, got: #{inspect(args)}")
    end

    create_file("rel/vm.args.eex", vm_args_text(false), opts)
    create_file("rel/remote.vm.args.eex", vm_args_text(true), opts)
    create_file("rel/env.sh.eex", env_text(), opts)
    create_file("rel/env.bat.eex", env_bat_text(), opts)
    :ok
  end

  @doc false
  def vm_args_text(remote?),
    do: """
    ## Customize flags given to the VM: https://www.erlang.org/doc/man/erl.html
    ## -mode/-name/-sname/-setcookie are configured via env vars, do not set them here

    ## Increase number of concurrent ports/sockets
    ##+Q 65536

    ## Tweak GC to run more often
    ##-env ERL_FULLSWEEP_AFTER 10

    ## Enable deployment without epmd
    ## (requires changing both vm.args and remote.vm.args)
    ##-start_epmd false -erl_epmd_port 6789#{if(remote?, do: " -dist_listen false")}
    """

  @doc false
  def env_text,
    do: ~S"""
    #!/bin/sh

    # # Sets and enables heart (recommended only in daemon mode)
    # case $RELEASE_COMMAND in
    #   daemon*)
    #     HEART_COMMAND="$RELEASE_ROOT/bin/$RELEASE_NAME $RELEASE_COMMAND"
    #     export HEART_COMMAND
    #     export ELIXIR_ERL_OPTIONS="-heart"
    #     ;;
    #   *)
    #     ;;
    # esac

    # # Set the release to load code on demand (interactive) instead of preloading (embedded).
    # export RELEASE_MODE=interactive

    # # Set the release to work across nodes.
    # # RELEASE_DISTRIBUTION must be "sname" (local), "name" (distributed) or "none".
    # export RELEASE_DISTRIBUTION=name
    # export RELEASE_NODE=<%= @release.name %>
    """

  @doc false
  def cli_text,
    do: ~S"""
    #!/bin/sh
    set -e

    readlink_f () {
      cd "$(dirname "$1")" > /dev/null
      filename="$(basename "$1")"
      if [ -h "$filename" ]; then
        readlink_f "$(readlink "$filename")"
      else
        echo "$(pwd -P)/$filename"
      fi
    }

    SELF=$(readlink_f "$0")
    RELEASE_ROOT="$(CDPATH='' cd "$(dirname "$SELF")/.." && pwd -P)"
    export RELEASE_ROOT
    RELEASE_NAME="${RELEASE_NAME:-"<%= @release.name %>"}"
    export RELEASE_NAME
    RELEASE_VSN="${RELEASE_VSN:-"$(cut -d' ' -f2 "$RELEASE_ROOT/releases/start_erl.data")"}"
    export RELEASE_VSN
    RELEASE_COMMAND="$1"
    export RELEASE_COMMAND
    RELEASE_PROG="${RELEASE_PROG:-"$(echo "$0" | sed 's/.*\///')"}"
    export RELEASE_PROG

    REL_VSN_DIR="$RELEASE_ROOT/releases/$RELEASE_VSN"
    . "$REL_VSN_DIR/env.sh"

    RELEASE_COOKIE="${RELEASE_COOKIE:-"$(cat "$RELEASE_ROOT/releases/COOKIE")"}"
    export RELEASE_COOKIE
    RELEASE_MODE="${RELEASE_MODE:-"embedded"}"
    export RELEASE_MODE
    RELEASE_NODE="${RELEASE_NODE:-"$RELEASE_NAME"}"
    export RELEASE_NODE
    RELEASE_TMP="${RELEASE_TMP:-"$RELEASE_ROOT/tmp"}"
    export RELEASE_TMP
    RELEASE_VM_ARGS="${RELEASE_VM_ARGS:-"$REL_VSN_DIR/vm.args"}"
    export RELEASE_VM_ARGS
    RELEASE_REMOTE_VM_ARGS="${RELEASE_REMOTE_VM_ARGS:-"$REL_VSN_DIR/remote.vm.args"}"
    export RELEASE_REMOTE_VM_ARGS
    RELEASE_DISTRIBUTION="${RELEASE_DISTRIBUTION:-"sname"}"
    export RELEASE_DISTRIBUTION
    RELEASE_BOOT_SCRIPT="${RELEASE_BOOT_SCRIPT:-"start"}"
    export RELEASE_BOOT_SCRIPT
    RELEASE_BOOT_SCRIPT_CLEAN="${RELEASE_BOOT_SCRIPT_CLEAN:-"start_clean"}"
    export RELEASE_BOOT_SCRIPT_CLEAN

    rand () {
      dd count=1 bs=2 if=/dev/urandom 2> /dev/null | od -x | awk 'NR==1{print $2}'
    }

    release_distribution () {
      case $RELEASE_DISTRIBUTION in
        none)
          ;;

        name | sname)
          echo "--$RELEASE_DISTRIBUTION $1"
          ;;

        *)
          echo "ERROR: Expected RELEASE_DISTRIBUTION to be sname, name, or none, got: $RELEASE_DISTRIBUTION" >&2
          exit 1
          ;;
      esac
    }

    rpc () {
      exec "$REL_VSN_DIR/elixir" \
           --hidden --cookie "$RELEASE_COOKIE" \
           $(release_distribution "rpc-$(rand)-$RELEASE_NODE") \
           --boot "$REL_VSN_DIR/$RELEASE_BOOT_SCRIPT_CLEAN" \
           --boot-var RELEASE_LIB "$RELEASE_ROOT/lib" \
           --vm-args "$RELEASE_REMOTE_VM_ARGS" \
           --rpc-eval "$RELEASE_NODE" "$1"
    }

    start () {
      export_release_sys_config
      REL_EXEC="$1"
      shift
      exec "$REL_VSN_DIR/$REL_EXEC" \
           --cookie "$RELEASE_COOKIE" \
           $(release_distribution "$RELEASE_NODE") \
           --erl "<%= release_mode(@release, "$RELEASE_MODE") %>" \
           --erl-config "$RELEASE_SYS_CONFIG" \
           --boot "$REL_VSN_DIR/$RELEASE_BOOT_SCRIPT" \
           --boot-var RELEASE_LIB "$RELEASE_ROOT/lib" \
           --vm-args "$RELEASE_VM_ARGS" "$@"
    }

    export_release_sys_config () {
      DEFAULT_SYS_CONFIG="${RELEASE_SYS_CONFIG:-"$REL_VSN_DIR/sys"}"

      if grep -q "RUNTIME_CONFIG=true" "$DEFAULT_SYS_CONFIG.config"; then
        RELEASE_SYS_CONFIG="$RELEASE_TMP/$RELEASE_NAME-$RELEASE_VSN-$(date +%Y%m%d%H%M%S)-$(rand).runtime"

        (mkdir -p "$RELEASE_TMP" && cat "$DEFAULT_SYS_CONFIG.config" >"$RELEASE_SYS_CONFIG.config") || (
          echo "ERROR: Cannot start release because it could not write $RELEASE_SYS_CONFIG.config" >&2
          exit 1
        )
      else
        RELEASE_SYS_CONFIG="$DEFAULT_SYS_CONFIG"
      fi

      export RELEASE_SYS_CONFIG
    }

    case $1 in
      start)
        start "elixir" --no-halt
        ;;

      start_iex)
        start "iex"
        ;;

      daemon)
        start "elixir" --no-halt --pipe-to "${RELEASE_TMP}/pipe" "${RELEASE_TMP}/log"
        ;;

      daemon_iex)
        start "iex" --pipe-to "${RELEASE_TMP}/pipe" "${RELEASE_TMP}/log"
        ;;

      eval)
        if [ -z "$2" ]; then
          echo "ERROR: EVAL expects an expression as argument" >&2
          exit 1
        fi
        script="$2"
        shift 2
        export_release_sys_config
        exec "$REL_VSN_DIR/elixir" \
           --cookie "$RELEASE_COOKIE" \
           --erl-config "$RELEASE_SYS_CONFIG" \
           --boot "$REL_VSN_DIR/$RELEASE_BOOT_SCRIPT_CLEAN" \
           --boot-var RELEASE_LIB "$RELEASE_ROOT/lib" \
           --vm-args "$RELEASE_VM_ARGS" --eval "$script" -- "$@"
        ;;

      remote)
        exec "$REL_VSN_DIR/iex" \
             --hidden --cookie "$RELEASE_COOKIE" \
             $(release_distribution "rem-$(rand)-$RELEASE_NODE") \
             --boot "$REL_VSN_DIR/$RELEASE_BOOT_SCRIPT_CLEAN" \
             --boot-var RELEASE_LIB "$RELEASE_ROOT/lib" \
             --vm-args "$RELEASE_REMOTE_VM_ARGS" \
             --remsh "$RELEASE_NODE"
        ;;

      rpc)
        if [ -z "$2" ]; then
          echo "ERROR: RPC expects an expression as argument" >&2
          exit 1
        fi
        rpc "$2"
        ;;

      restart|stop)
        rpc "System.$1()"
        ;;

      pid)
        rpc "IO.puts System.pid()"
        ;;

      version)
        echo "$RELEASE_NAME $RELEASE_VSN"
        ;;

      *)
        echo "Usage: $(basename "$0") COMMAND [ARGS]

    The known commands are:

        start          Starts the system
        start_iex      Starts the system with IEx attached
        daemon         Starts the system as a daemon
        daemon_iex     Starts the system as a daemon with IEx attached
        eval \"EXPR\"    Executes the given expression on a new, non-booted system
        rpc \"EXPR\"     Executes the given expression remotely on the running system
        remote         Connects to the running system via a remote shell
        restart        Restarts the running system via a remote command
        stop           Stops the running system via a remote command
        pid            Prints the operating system PID of the running system via a remote command
        version        Prints the release name and version to be booted
    " >&2

        if [ -n "$1" ]; then
          echo "ERROR: Unknown command $1" >&2
          exit 1
        fi
        ;;
    esac
    """

  @doc false
  def env_bat_text,
    do: ~S"""
    @echo off
    rem Set the release to load code on demand (interactive) instead of preloading (embedded).
    rem set RELEASE_MODE=interactive

    rem Set the release to work across nodes.
    rem RELEASE_DISTRIBUTION must be sname (local), name (distributed) or none.
    rem set RELEASE_DISTRIBUTION=name
    rem set RELEASE_NODE=<%= @release.name %>
    """

  @doc false
  def cli_bat_text,
    do: ~S"""
    @echo off
    setlocal enabledelayedexpansion

    pushd .
    pushd "%~dp0\.."
    set RELEASE_ROOT=%cd%
    popd
    popd

    if not defined RELEASE_NAME (set RELEASE_NAME=<%= @release.name %>)
    if not defined RELEASE_VSN (for /f "tokens=1,2" %%K in ('type "!RELEASE_ROOT!\releases\start_erl.data"') do (set ERTS_VSN=%%K) && (set RELEASE_VSN=%%L))
    if not defined RELEASE_PROG (set RELEASE_PROG=%~nx0)
    set RELEASE_COMMAND=%~1
    set REL_VSN_DIR=!RELEASE_ROOT!\releases\!RELEASE_VSN!
    call "!REL_VSN_DIR!\env.bat" %*

    if not defined RELEASE_COOKIE (set /p RELEASE_COOKIE=<!RELEASE_ROOT!\releases\COOKIE)
    if not defined RELEASE_MODE (set RELEASE_MODE=embedded)
    if not defined RELEASE_NODE (set RELEASE_NODE=!RELEASE_NAME!)
    if not defined RELEASE_TMP (set RELEASE_TMP=!RELEASE_ROOT!\tmp)
    if not defined RELEASE_VM_ARGS (set RELEASE_VM_ARGS=!REL_VSN_DIR!\vm.args)
    if not defined RELEASE_REMOTE_VM_ARGS (set RELEASE_REMOTE_VM_ARGS=!REL_VSN_DIR!\remote.vm.args)
    if not defined RELEASE_DISTRIBUTION (set RELEASE_DISTRIBUTION=sname)
    if not defined RELEASE_BOOT_SCRIPT (set RELEASE_BOOT_SCRIPT=start)
    if not defined RELEASE_BOOT_SCRIPT_CLEAN (set RELEASE_BOOT_SCRIPT_CLEAN=start_clean)
    if not defined RELEASE_SYS_CONFIG (set RELEASE_SYS_CONFIG=!REL_VSN_DIR!\sys)

    if "!RELEASE_DISTRIBUTION!" == "none" (
      rem
    ) else if "!RELEASE_DISTRIBUTION!" == "name" (
      rem
    ) else if "!RELEASE_DISTRIBUTION!" == "sname" (
      rem
    ) else (
      echo ERROR: Expected RELEASE_DISTRIBUTION to be sname, name, or none, got: !RELEASE_DISTRIBUTION!
      exit /B 1
    )

    if "!RELEASE_MODE!" == "embedded" (
      rem
    ) else if "!RELEASE_MODE!" == "interactive" (
      rem
    ) else (
      echo ERROR: Expected RELEASE_MODE to be embedded or interactive, got: !RELEASE_MODE!
      exit /B 1
    )

    if "%~1" == "start" (set "REL_EXEC=elixir" && set "REL_EXTRA=--no-halt" && set "REL_GOTO=start")
    if "%~1" == "start_iex" (set "REL_EXEC=iex" && set "REL_GOTO=start")
    if "%~1" == "install" (set "REL_GOTO=install")
    if "%~1" == "eval" (
      if "%~2" == "" (
        echo ERROR: EVAL expects an expression as argument
        exit /B 1
      )
      set "REL_GOTO=eval"
    )

    if not "!REL_GOTO!" == "" (
      findstr "RUNTIME_CONFIG=true" "!RELEASE_SYS_CONFIG!.config" >nul 2>&1 && (
        set DEFAULT_SYS_CONFIG=!RELEASE_SYS_CONFIG!
        for /f "skip=1" %%X in ('wmic os get localdatetime') do if not defined TIMESTAMP set TIMESTAMP=%%X
        set RELEASE_SYS_CONFIG=!RELEASE_TMP!\!RELEASE_NAME!-!RELEASE_VSN!-!TIMESTAMP:~0,11!-!RANDOM!.runtime
        mkdir "!RELEASE_TMP!" >nul 2>&1
        copy /y "!DEFAULT_SYS_CONFIG!.config" "!RELEASE_SYS_CONFIG!.config" >nul || (
          echo Cannot start release because it could not write to "!RELEASE_SYS_CONFIG!.config"
          goto end
        )
      )

      goto !REL_GOTO!
    )

    if "%~1" == "remote" (goto remote)
    if "%~1" == "version" (goto version)
    if "%~1" == "stop" (set "REL_RPC=System.stop()" && goto rpc)
    if "%~1" == "restart" (set "REL_RPC=System.restart()" && goto rpc)
    if "%~1" == "pid" (set "REL_RPC=IO.puts(System.pid())" && goto rpc)
    if "%~1" == "rpc" (
      if "%~2" == "" (
        echo ERROR: RPC expects an expression as argument
        exit /B 1
      )
      set "REL_RPC=%~2"
      goto rpc
    )

    echo Usage: %~nx0 COMMAND [ARGS]
    echo.
    echo The known commands are:
    echo.
    echo    start        Starts the system
    echo    start_iex    Starts the system with IEx attached
    echo    install      Installs this system as a Windows service
    echo    eval "EXPR"  Executes the given expression on a new, non-booted system
    echo    rpc "EXPR"   Executes the given expression remotely on the running system
    echo    remote       Connects to the running system via a remote shell
    echo    restart      Restarts the running system via a remote command
    echo    stop         Stops the running system via a remote command
    echo    pid          Prints the operating system PID of the running system via a remote command
    echo    version      Prints the release name and version to be booted
    echo.
    if not "%~1" == "" (
      echo ERROR: Unknown command %~1
      exit /B 1
    )
    goto end

    :start
    if "!RELEASE_DISTRIBUTION!" == "none" (
      set RELEASE_DISTRIBUTION_FLAG=
    ) else (
      set RELEASE_DISTRIBUTION_FLAG=--!RELEASE_DISTRIBUTION! "!RELEASE_NODE!"
    )

    "!REL_VSN_DIR!\!REL_EXEC!.bat" !REL_EXTRA! ^
      --cookie "!RELEASE_COOKIE!" ^
      !RELEASE_DISTRIBUTION_FLAG! ^
      --erl "<%= release_mode(@release, "!RELEASE_MODE!") %>" ^
      --erl-config "!RELEASE_SYS_CONFIG!" ^
      --boot "!REL_VSN_DIR!\!RELEASE_BOOT_SCRIPT!" ^
      --boot-var RELEASE_LIB "!RELEASE_ROOT!\lib" ^
      --vm-args "!RELEASE_VM_ARGS!"
    goto end

    :eval
    set EVAL=%~2
    shift
    :loop
    shift
    if not "%1"=="" (
      set args=%args% %1
      goto :loop
    )
    "!REL_VSN_DIR!\elixir.bat" ^
      --eval "!EVAL!" ^
      --cookie "!RELEASE_COOKIE!" ^
      --erl-config "!RELEASE_SYS_CONFIG!" ^
      --boot "!REL_VSN_DIR!\!RELEASE_BOOT_SCRIPT_CLEAN!" ^
      --boot-var RELEASE_LIB "!RELEASE_ROOT!\lib" ^
      --vm-args "!RELEASE_VM_ARGS!" -- %args%
    goto end

    :remote
    if "!RELEASE_DISTRIBUTION!" == "none" (
      set RELEASE_DISTRIBUTION_FLAG=
    ) else (
      set RELEASE_DISTRIBUTION_FLAG=--!RELEASE_DISTRIBUTION! "rem-!RANDOM!-!RELEASE_NODE!"
    )

    "!REL_VSN_DIR!\iex.bat" ^
      --hidden --cookie "!RELEASE_COOKIE!" ^
      !RELEASE_DISTRIBUTION_FLAG! ^
      --boot "!REL_VSN_DIR!\!RELEASE_BOOT_SCRIPT_CLEAN!" ^
      --boot-var RELEASE_LIB "!RELEASE_ROOT!\lib" ^
      --vm-args "!RELEASE_REMOTE_VM_ARGS!" ^
      --remsh "!RELEASE_NODE!"
    goto end

    :rpc
    if "!RELEASE_DISTRIBUTION!" == "none" (
      set RELEASE_DISTRIBUTION_FLAG=
    ) else (
      set RELEASE_DISTRIBUTION_FLAG=--!RELEASE_DISTRIBUTION! "rpc-!RANDOM!-!RELEASE_NODE!"
    )

    "!REL_VSN_DIR!\elixir.bat" ^
      --hidden --cookie "!RELEASE_COOKIE!" ^
      !RELEASE_DISTRIBUTION_FLAG! ^
      --boot "!REL_VSN_DIR!\!RELEASE_BOOT_SCRIPT_CLEAN!" ^
      --boot-var RELEASE_LIB "!RELEASE_ROOT!\lib" ^
      --vm-args "!RELEASE_REMOTE_VM_ARGS!" ^
      --rpc-eval "!RELEASE_NODE!" "!REL_RPC!"
    goto end

    :version
    echo !RELEASE_NAME! !RELEASE_VSN!
    goto end

    :install
    if exist !RELEASE_ROOT!\erts-!ERTS_VSN! (
      set ERLSRV=!RELEASE_ROOT!\erts-!ERTS_VSN!\bin\erlsrv.exe
    ) else (
      set ERLSRV=erlsrv.exe
    )

    if "!RELEASE_DISTRIBUTION!" == "none" (
      echo ERROR: RELEASE_DISTRIBUTION is required in install command
      exit /B 1
    )

    "!ERLSRV!" add "!RELEASE_NAME!_!RELEASE_NAME!" ^
      -!RELEASE_DISTRIBUTION! "!RELEASE_NODE!" ^
      -env RELEASE_ROOT="!RELEASE_ROOT!" -env RELEASE_NAME="!RELEASE_NAME!" -env RELEASE_VSN="!RELEASE_VSN!" -env RELEASE_MODE="!RELEASE_MODE!" -env RELEASE_COOKIE="!RELEASE_COOKIE!" -env RELEASE_NODE="!RELEASE_NODE!" -env RELEASE_VM_ARGS="!RELEASE_VM_ARGS!" -env RELEASE_TMP="!RELEASE_TMP!" -env RELEASE_SYS_CONFIG="!RELEASE_SYS_CONFIG!" ^
      -args "-setcookie !RELEASE_COOKIE! -config ""!RELEASE_SYS_CONFIG!"" <%= release_mode(@release, "!RELEASE_MODE!") %> -boot ""!REL_VSN_DIR!\start"" -boot_var RELEASE_LIB ""!RELEASE_ROOT!\lib"" -args_file ""!REL_VSN_DIR!\vm.args\"""
    if %ERRORLEVEL% EQU 0 (
      echo Service installed but not started. From now on, it must be started and stopped by erlsrv:
      echo.
      echo     !ERLSRV! start !RELEASE_NAME!_!RELEASE_NAME!
      echo     !ERLSRV! stop !RELEASE_NAME!_!RELEASE_NAME!
      echo     !ERLSRV! remove !RELEASE_NAME!_!RELEASE_NAME!
      echo     !ERLSRV! list
      echo     !ERLSRV! help
      echo.
    )
    goto end

    :end
    endlocal
    """
end
