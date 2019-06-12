defmodule Mix.Tasks.Release.Init do
  use Mix.Task

  @shortdoc "Generates sample files for releases"

  @moduledoc """
  Generates sample files for releases.

      mix release.init
      * creating rel/vm.args.eex
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

    create_file("rel/vm.args.eex", vm_args_text(), opts)
    create_file("rel/env.sh.eex", env_text(), opts)
    create_file("rel/env.bat.eex", env_bat_text(), opts)
    :ok
  end

  @doc false
  def vm_args_text,
    do: ~S"""
    ## Customize flags given to the VM: http://erlang.org/doc/man/erl.html
    ## -mode/-name/-sname/-setcookie are configured via env vars, do not set them here

    ## Number of dirty schedulers doing IO work (file, sockets, etc)
    ##+SDio 5

    ## Increase number of concurrent ports/sockets
    ##+Q 65536

    ## Tweak GC to run more often
    ##-env ERL_FULLSWEEP_AFTER 10
    """

  @doc false
  def env_text,
    do: ~S"""
    #!/bin/sh

    # Sets and enables heart (recommended only in daemon mode)
    # if [ "$RELEASE_COMMAND" = "daemon" ] || [ "$RELEASE_COMMAND" = "daemon_iex" ]; then
    #   HEART_COMMAND="$RELEASE_ROOT/bin/$RELEASE_NAME $RELEASE_COMMAND"
    #   export HEART_COMMAND
    #   export ELIXIR_ERL_OPTIONS="-heart"
    # fi

    # Set the release to work across nodes. If using the long name format like
    # the one below (my_app@127.0.0.1), you need to also uncomment the
    # RELEASE_DISTRIBUTION variable below.
    # export RELEASE_DISTRIBUTION=name
    # export RELEASE_NODE=<%= @release.name %>@127.0.0.1
    """

  @doc false
  def cli_text,
    do: ~S"""
    #!/bin/sh
    set -e

    SELF=$(readlink "$0" || true)
    if [ -z "$SELF" ]; then SELF="$0"; fi
    RELEASE_ROOT="$(cd "$(dirname "$SELF")/.." && pwd -P)"
    export RELEASE_ROOT
    export RELEASE_NAME="${RELEASE_NAME:-"<%= @release.name %>"}"
    export RELEASE_VSN="${RELEASE_VSN:-"$(cut -d' ' -f2 "$RELEASE_ROOT/releases/start_erl.data")"}"
    export RELEASE_COMMAND="$1"
    export RELEASE_MODE="${RELEASE_MODE:-"embedded"}"

    REL_VSN_DIR="$RELEASE_ROOT/releases/$RELEASE_VSN"
    . "$REL_VSN_DIR/env.sh"

    export RELEASE_COOKIE="${RELEASE_COOKIE:-"$(cat "$RELEASE_ROOT/releases/COOKIE")"}"
    export RELEASE_NODE="${RELEASE_NODE:-"$RELEASE_NAME"}"
    export RELEASE_TMP="${RELEASE_TMP:-"$RELEASE_ROOT/tmp"}"
    export RELEASE_VM_ARGS="${RELEASE_VM_ARGS:-"$REL_VSN_DIR/vm.args"}"
    export RELEASE_DISTRIBUTION="${RELEASE_DISTRIBUTION:-"sname"}"
    export RELEASE_BOOT_SCRIPT="${RELEASE_BOOT_SCRIPT:-"start"}"
    export RELEASE_BOOT_SCRIPT_CLEAN="${RELEASE_BOOT_SCRIPT_CLEAN:-"start_clean"}"

    rand () {
      od -t xS -N 2 -A n /dev/urandom | tr -d " \n"
    }

    rpc () {
      exec "$REL_VSN_DIR/elixir" \
           --hidden --cookie "$RELEASE_COOKIE" \
           --$RELEASE_DISTRIBUTION "rpc-$(rand)-$RELEASE_NODE" \
           --boot "$REL_VSN_DIR/$RELEASE_BOOT_SCRIPT_CLEAN" \
           --boot-var RELEASE_LIB "$RELEASE_ROOT/lib" \
           --rpc-eval "$RELEASE_NODE" "$1"
    }

    start () {
      export_release_sys_config
      REL_EXEC="$1"
      shift
      exec "$REL_VSN_DIR/$REL_EXEC" \
           --cookie "$RELEASE_COOKIE" \
           --$RELEASE_DISTRIBUTION "$RELEASE_NODE" \
           --erl "-mode $RELEASE_MODE" \
           --erl-config "$RELEASE_SYS_CONFIG" \
           --boot "$REL_VSN_DIR/$RELEASE_BOOT_SCRIPT" \
           --boot-var RELEASE_LIB "$RELEASE_ROOT/lib" \
           --vm-args "$RELEASE_VM_ARGS" "$@"
    }

    export_release_sys_config () {
      if grep -q "RUNTIME_CONFIG=true" "$REL_VSN_DIR/sys.config"; then
        RELEASE_SYS_CONFIG="$RELEASE_TMP/$RELEASE_NAME-$RELEASE_VSN-$(date +%Y%m%d%H%M%S)-$(rand).runtime"

        (mkdir -p "$RELEASE_TMP" && cp "$REL_VSN_DIR/sys.config" "$RELEASE_SYS_CONFIG.config") || (
          echo "ERROR: Cannot start release because it could not write $RELEASE_SYS_CONFIG.config" >&2
          exit 1
        )
      else
        RELEASE_SYS_CONFIG="$REL_VSN_DIR/sys"
      fi

      export RELEASE_SYS_CONFIG
    }

    case $1 in
      start)
        start "elixir" --no-halt
        ;;

      start_iex)
        start "iex" --werl
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

        export_release_sys_config
        exec "$REL_VSN_DIR/elixir" \
           --cookie "$RELEASE_COOKIE" \
           --erl-config "$RELEASE_SYS_CONFIG" \
           --boot "$REL_VSN_DIR/$RELEASE_BOOT_SCRIPT_CLEAN" \
           --boot-var RELEASE_LIB "$RELEASE_ROOT/lib" \
           --vm-args "$RELEASE_VM_ARGS" --eval "$2"
        ;;

      remote)
        exec "$REL_VSN_DIR/iex" \
             --werl --hidden --cookie "$RELEASE_COOKIE" \
             --$RELEASE_DISTRIBUTION "rem-$(rand)-$RELEASE_NODE" \
             --boot "$REL_VSN_DIR/$RELEASE_BOOT_SCRIPT_CLEAN" \
             --boot-var RELEASE_LIB "$RELEASE_ROOT/lib" \
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
        rpc "System.$1"
        ;;

      pid)
        rpc "IO.puts System.pid"
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
        pid            Prints the OS PID of the running system via a remote command
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
    rem Set the release to work across nodes. If using the long name format like
    rem the one below (my_app@127.0.0.1), you need to also uncomment the
    rem RELEASE_DISTRIBUTION variable below.
    rem set RELEASE_DISTRIBUTION=name
    rem set RELEASE_NODE=<%= @release.name %>@127.0.0.1
    """

  @doc false
  def cli_bat_text,
    do: ~S"""
    @echo off
    setlocal enabledelayedexpansion

    pushd .
    cd "%~dp0\.."
    set RELEASE_ROOT=%cd%
    popd

    if not defined RELEASE_NAME (set RELEASE_NAME=<%= @release.name %>)
    if not defined RELEASE_VSN (for /f "tokens=1,2" %%K in ('type "!RELEASE_ROOT!\releases\start_erl.data"') do (set ERTS_VSN=%%K) && (set RELEASE_VSN=%%L))
    if not defined RELEASE_MODE (set RELEASE_MODE=embedded)
    set RELEASE_COMMAND=%~1
    set REL_VSN_DIR=!RELEASE_ROOT!\releases\!RELEASE_VSN!
    call "!REL_VSN_DIR!\env.bat"

    if not defined RELEASE_COOKIE (set /p RELEASE_COOKIE=<!RELEASE_ROOT!\releases\COOKIE)
    if not defined RELEASE_NODE (set RELEASE_NODE=!RELEASE_NAME!)
    if not defined RELEASE_TMP (set RELEASE_TMP=!RELEASE_ROOT!\tmp)
    if not defined RELEASE_VM_ARGS (set RELEASE_VM_ARGS=!REL_VSN_DIR!\vm.args)
    if not defined RELEASE_DISTRIBUTION (set RELEASE_DISTRIBUTION=sname)
    if not defined RELEASE_BOOT_SCRIPT (set RELEASE_BOOT_SCRIPT=start)
    if not defined RELEASE_BOOT_SCRIPT (set RELEASE_BOOT_SCRIPT_CLEAN=start_clean)
    set RELEASE_SYS_CONFIG=!REL_VSN_DIR!\sys

    if "%~1" == "start" (set "REL_EXEC=elixir" && set "REL_EXTRA=--no-halt" && set "REL_GOTO=start")
    if "%~1" == "start_iex" (set "REL_EXEC=iex" && set "REL_EXTRA=--werl" && set "REL_GOTO=start")
    if "%~1" == "install" (set "REL_GOTO=install")
    if "%~1" == "eval" (
      if "%~2" == "" (
        echo ERROR: EVAL expects an expression as argument
        goto end
      )
      set "REL_GOTO=eval"
    )

    if not "!REL_GOTO!" == "" (
      findstr "RUNTIME_CONFIG=true" "!RELEASE_SYS_CONFIG!.config" >nul 2>&1 && (
        for /f "skip=1" %%X in ('wmic os get localdatetime') do if not defined TIMESTAMP set TIMESTAMP=%%X
        set RELEASE_SYS_CONFIG=!RELEASE_TMP!\!RELEASE_NAME!-!RELEASE_VSN!-!TIMESTAMP:~0,11!-!RANDOM!.runtime
        mkdir "!RELEASE_TMP!" >nul
        copy /y "!REL_VSN_DIR!\sys.config" "!RELEASE_SYS_CONFIG!.config" >nul || (
          echo Cannot start release because it could not write to "!RELEASE_SYS_CONFIG!.config"
          goto end
        )
      )

      goto !REL_GOTO!
    )

    if "%~1" == "remote" (goto remote)
    if "%~1" == "version" (goto version)
    if "%~1" == "stop" (set "REL_RPC=System.stop()" && goto rpc)
    if "%~1" == "restart" (set "REL_RPC=System.stop()" && goto rpc)
    if "%~1" == "pid" (set "REL_RPC=IO.puts(System.pid())" && goto rpc)
    if "%~1" == "rpc" (
      if "%~2" == "" (
        echo ERROR: RPC expects an expression as argument
        goto end
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
    echo    pid          Prints the OS PID of the running system via a remote command
    echo    version      Prints the release name and version to be booted
    echo.
    if not "%~1" == "" (echo ERROR: Unknown command %~1)
    goto end

    :start
    "!REL_VSN_DIR!\!REL_EXEC!.bat" !REL_EXTRA! ^
      --cookie "!RELEASE_COOKIE!" ^
      --!RELEASE_DISTRIBUTION! "!RELEASE_NODE!" ^
      --erl "-mode !RELEASE_MODE!" ^
      --erl-config "!RELEASE_SYS_CONFIG!" ^
      --boot "!REL_VSN_DIR!\!RELEASE_BOOT_SCRIPT!" ^
      --boot-var RELEASE_LIB "!RELEASE_ROOT!\lib" ^
      --vm-args "!RELEASE_VM_ARGS!"
    goto end

    :eval
    "!REL_VSN_DIR!\elixir.bat" ^
      --eval "%~2" ^
      --cookie "!RELEASE_COOKIE!" ^
      --erl-config "!RELEASE_SYS_CONFIG!" ^
      --boot "!REL_VSN_DIR!\!RELEASE_BOOT_SCRIPT_CLEAN!" ^
      --boot-var RELEASE_LIB "!RELEASE_ROOT!\lib" ^
      --vm-args "!RELEASE_VM_ARGS!"
    goto end

    :remote
    "!REL_VSN_DIR!\iex.bat" ^
      --werl --hidden --cookie "!RELEASE_COOKIE!" ^
      --!RELEASE_DISTRIBUTION! "rem-!RANDOM!-!RELEASE_NODE!" ^
      --boot "!REL_VSN_DIR!\!RELEASE_BOOT_SCRIPT_CLEAN!" ^
      --boot-var RELEASE_LIB "!RELEASE_ROOT!\lib" ^
      --remsh "!RELEASE_NODE!"
    goto end

    :rpc
    "!REL_VSN_DIR!\elixir.bat" ^
      --hidden --cookie "!RELEASE_COOKIE!" ^
      --!RELEASE_DISTRIBUTION! "rpc-!RANDOM!-!RELEASE_NODE!" ^
      --boot "!REL_VSN_DIR!\!RELEASE_BOOT_SCRIPT_CLEAN!" ^
      --boot-var RELEASE_LIB "!RELEASE_ROOT!\lib" ^
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

    !ERLSRV! add !RELEASE_NAME!_!RELEASE_NAME! ^
      -name "!RELEASE_NODE!" ^
      -args "-setcookie !RELEASE_COOKIE! -config !RELEASE_SYS_CONFIG! -mode !RELEASE_MODE! -boot !REL_VSN_DIR!\start -boot_var RELEASE_LIB !RELEASE_ROOT!\lib -args_file !REL_VSN_DIR!\vm.args"

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
