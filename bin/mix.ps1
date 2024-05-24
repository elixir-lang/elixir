#!/usr/bin/env pwsh

$ScriptPath = Split-Path -Parent $PSCommandPath
$Command = Join-Path -Path $ScriptPath -ChildPath "elixir.ps1"
$MixFile = Join-Path -Path $ScriptPath -ChildPath "mix"

Invoke-Expression "$Command $MixFile $($Args -join " ")"
