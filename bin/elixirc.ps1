$ScriptName = Split-Path -Leaf $PSCommandPath

if (($Args.Count -eq 0) -or ($Args[0] -in @("-h", "--help"))) {
  Write-Host @"
Usage: $ScriptName [elixir switches] [compiler switches] [.ex files]

  -h, --help                Prints this message and exits
  -o                        The directory to output compiled files
  -v, --version             Prints Elixir version and exits (standalone)

  --ignore-module-conflict  Does not emit warnings if a module was previously defined
  --no-debug-info           Does not attach debug info to compiled modules
  --no-docs                 Does not attach documentation to compiled modules
  --profile time            Profile the time to compile modules
  --verbose                 Prints compilation status
  --warnings-as-errors      Treats warnings as errors and returns non-zero exit status

** Options given after -- are passed down to the executed code
** Options can be passed to the Erlang runtime using ELIXIR_ERL_OPTIONS
** Options can be passed to the Erlang compiler using ERL_COMPILER_OPTIONS
"@
exit
}

$ScriptPath = Split-Path -Parent $PSCommandPath
$Command = Join-Path -Path $ScriptPath -ChildPath "elixir.ps1"

$NewArgs = @()
$NewArgs += "+elixirc"

for ($i = 0; $i -lt $Args.Count; $i++) {
  $Arg = $Args[$i]

  if ($Arg -is [string]) {
    $NewArgs += $Arg
  } else {
    $NewArgs += [string]::Join(",", $Arg)
  }
}

Invoke-Expression "$Command $([string]::Join(" ", $NewArgs))"
