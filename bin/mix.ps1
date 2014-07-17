# Store path to mix.bat as a FileInfo object
$mixBatPath = (Get-ChildItem (((Get-ChildItem $MyInvocation.MyCommand.Path).Directory.FullName) + '\mix.bat'))

for ($i = 0; $i -lt $args.length; $i++)
{
  if ($args[$i] -is [array])
  {
    # Commas created the array so we need to reintroduce those commas
    for ($j = 0; $j -lt $args[$i].length - 1; $i++)
    {
      $args[$i][$j] += ','
    }
  }
}

# Corrected arguments are ready to pass to batch file
& $mixBatPath $args
