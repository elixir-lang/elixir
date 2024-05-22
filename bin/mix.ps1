#!/usr/bin/env pwsh

$ScriptPath = Split-Path -Parent $PSCommandPath
$Command = Join-Path -Path $ScriptPath -ChildPath "elixir.ps1"
$MixCommand = Join-Path -Path $ScriptPath -ChildPath "mix"

$NewArgs = @($MixCommand)

for ($i = 0; $i -lt $Args.Count; $i++) {
  $Arg = $Args[$i]

  if ($Arg -is [string]) {
    $NewArgs += $Arg
  }
  else {
    $NewArgs += [string]::Join(",", $Arg)
  }
}

& $Command $NewArgs
