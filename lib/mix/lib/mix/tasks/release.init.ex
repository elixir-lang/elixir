defmodule Mix.Tasks.Release.Init do
  use Mix.Task

  @shortdoc "Generates sample files for releases"

  @moduledoc """
  Generates sample files for releases.

      mix release.init
      * creating rel/vm.args.eex
      * creating rel/start.eex
      * creating rel/start.bat.eex

  """

  import Mix.Generator

  @switches [
    force: :boolean,
    quiet: :boolean
  ]

  @aliases [
    f: :force
  ]

  @impl true
  def run(args) do
    {opts, args} = OptionParser.parse!(args, strict: @switches, aliases: @aliases)

    if args != [] do
      Mix.raise("Expected \"mix release.init\" without arguments, got: #{inspect(args)}")
    end

    create_file("rel/vm.args.eex", vm_args_text(), opts)
    create_file("rel/start.eex", start_text(), opts)
    create_file("rel/start.bat.eex", start_bat_text(), opts)
    :ok
  end

  @doc false
  def vm_args_text,
    do: ~S"""
    ## Customize flags given to the VM: http://erlang.org/doc/man/erl.html
    ## -name/-sname/-setcookie are given via the CLI, do not set them here

    ## Preloads all modules instead of loading them dynamically
    -mode embedded

    ## Number of dirty schedulers doing IO work (file, sockets, etc)
    ##+SDio 5

    ## Increase number of concurrent ports/sockets
    ##+Q 65536

    ## Tweak GC to run more often
    ##-env ERL_FULLSWEEP_AFTER 10
    """

  @doc false
  def start_text,
    do: ~S"""
    #!/bin/sh
    # Feel free to edit this file in any way you want
    set -e

    # Sets and enables heart (recommended only in daemon mode)
    # HEART_COMMAND="$(dirname "$0")/start"
    # export HEART_COMMAND
    # export ELIXIR_ERL_OPTIONS="-heart"

    # To start your system using IEx: "$(dirname "$0")/<%= @release.name %>" start_iex
    # To start it as a daemon using IEx: "$(dirname "$0")/<%= @release.name %>" daemon_iex
    "$(dirname "$0")/<%= @release.name %>" start
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
    export RELEASE_COOKIE=${RELEASE_COOKIE:-"$(cat "$RELEASE_ROOT/releases/COOKIE")"}
    export RELEASE_NODE=${RELEASE_NODE:-"$RELEASE_NAME@127.0.0.1"}
    export RELEASE_TMP=${RELEASE_TMP:-"$RELEASE_ROOT/tmp"}
    export RELEASE_VM_ARGS=${RELEASE_VM_ARGS:-"$RELEASE_ROOT/releases/$RELEASE_VSN/vm.args"}
    REL_VSN_DIR="$RELEASE_ROOT/releases/$RELEASE_VSN"

    rand () {
      od -t xS -N 2 -A n /dev/urandom | tr -d " \n"
    }

    rpc () {
      exec "$REL_VSN_DIR/elixir" \
           --hidden --name "rpc-$(rand)@127.0.0.1" --cookie "$RELEASE_COOKIE" \
           --boot "$REL_VSN_DIR/start_clean" \
           --boot-var RELEASE_LIB "$RELEASE_ROOT/lib" \
           --rpc-eval "$RELEASE_NODE" "$1"
    }

    start () {
      export_release_sys_config
      REL_EXEC="$1"
      shift
      exec "$REL_VSN_DIR/$REL_EXEC" \
           --name "$RELEASE_NODE" --cookie "$RELEASE_COOKIE" \
           --erl-config "$RELEASE_SYS_CONFIG" \
           --boot "$REL_VSN_DIR/start" \
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
           --boot "$REL_VSN_DIR/start_clean" \
           --boot-var RELEASE_LIB "$RELEASE_ROOT/lib" \
           --vm-args "$RELEASE_VM_ARGS" --eval "$2"
        ;;

      remote)
        exec "$REL_VSN_DIR/iex" \
             --werl --hidden --name "remote-$(rand)@127.0.0.1" --cookie "$RELEASE_COOKIE" \
             --boot "$REL_VSN_DIR/start_clean" \
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
  def start_bat_text,
    do: ~S"""
    @echo off
    rem Feel free to edit this file in anyway you want
    rem To start your system using IEx: %~dp0/<%= @release.name %> start_iex
    %~dp0/<%= @release.name %> start
    """

  @doc false
  def cli_bat_text,
    do: ~S"""
    @echo off
    setlocal enabledelayedexpansion

    pushd .
    cd "%~dp0/.."
    set RELEASE_ROOT=%cd%
    popd

    if not defined RELEASE_NAME (set RELEASE_NAME=<%= @release.name %>)
    if not defined RELEASE_VSN (for /f "tokens=1,2" %%K in (!RELEASE_ROOT!\releases\start_erl.data) do (set ERTS_VSN=%%K) && (set RELEASE_VSN=%%L))
    if not defined RELEASE_COOKIE (set /p RELEASE_COOKIE=<!RELEASE_ROOT!\releases\COOKIE)
    if not defined RELEASE_NODE (set RELEASE_NODE=!RELEASE_NAME!@127.0.0.1)
    if not defined RELEASE_TMP (set RELEASE_TMP=!RELEASE_ROOT!\tmp)
    if not defined RELEASE_VM_ARGS (set RELEASE_VM_ARGS=!RELEASE_ROOT!\releases\!RELEASE_VSN!\vm.args)
    set REL_VSN_DIR=!RELEASE_ROOT!\releases\!RELEASE_VSN!
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
      findstr "RUNTIME_CONFIG=true" "!RELEASE_SYS_CONFIG!.config" >nil 2>&1 && (
        for /f "skip=1" %%X in ('wmic os get localdatetime') do if not defined TIMESTAMP set TIMESTAMP=%%X
        set RELEASE_SYS_CONFIG=!RELEASE_TMP!\!RELEASE_NAME!-!RELEASE_VSN!-!TIMESTAMP:~0,11!-!RANDOM!.runtime
        mkdir "!RELEASE_TMP!" >nil
        copy /y "!REL_VSN_DIR!\sys.config" "!RELEASE_SYS_CONFIG!.config" >nil || (
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
      --name "!RELEASE_NODE!" --cookie "!RELEASE_COOKIE!" ^
      --erl-config "!RELEASE_SYS_CONFIG!" ^
      --boot "!REL_VSN_DIR!\start" ^
      --boot-var RELEASE_LIB "!RELEASE_ROOT!\lib" ^
      --vm-args "!RELEASE_VM_ARGS!"
    goto end

    :eval
    "!REL_VSN_DIR!\elixir.bat" ^
      --eval "%~2" ^
      --cookie "!RELEASE_COOKIE!" ^
      --erl-config "!RELEASE_SYS_CONFIG!" ^
      --boot "!REL_VSN_DIR!\start_clean" ^
      --boot-var RELEASE_LIB "!RELEASE_ROOT!\lib" ^
      --vm-args "!RELEASE_VM_ARGS!"
    goto end

    :remote
    "!REL_VSN_DIR!\iex.bat" ^
      --werl --hidden --name "remote-!RANDOM!@127.0.0.1" --cookie "!RELEASE_COOKIE!" ^
      --boot "!REL_VSN_DIR!\start_clean" ^
      --boot-var RELEASE_LIB "!RELEASE_ROOT!\lib" ^
      --remsh "!RELEASE_NODE!"
    goto end

    :rpc
    "!REL_VSN_DIR!\elixir.bat" ^
      --hidden --name "rpc-!RANDOM!@127.0.0.1" --cookie "!RELEASE_COOKIE!" ^
      --boot "!REL_VSN_DIR!\start_clean" ^
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
      -args "-setcookie !RELEASE_COOKIE! -config !RELEASE_SYS_CONFIG! -boot !REL_VSN_DIR!\start -boot_var RELEASE_LIB !RELEASE_ROOT!\lib -args_file !REL_VSN_DIR!\vm.args"

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
