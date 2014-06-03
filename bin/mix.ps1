# Initialize with path to mix.bat relative to caller's working directory
$toCmd = '' + (Resolve-Path -relative (Split-Path $MyInvocation.MyCommand.Path)) + '\mix.bat'

foreach ($arg in $args)
{
  $toCmd += ' '
  
  if ($arg -is [array])
  {
    # Commas created the array so we need to reintroduce those commas
    for ($i = 0; $i -lt $arg.length; $i++)
    {
      $toCmd += $arg[$i]
      if ($i -ne ($arg.length - 1))
      {
        $toCmd += ', '
      }
    }
  }
  else
  {
    $toCmd += $arg
  }
}

# Corrected arguments are ready to pass to batch file
cmd /c $toCmd