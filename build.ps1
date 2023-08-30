Set-Location (Split-Path -parent $MyInvocation.MyCommand.Definition)
& .\.bin\dasm.exe .\64-t.asm     -f1 -v4 '-o64-t.prg'     '-l64-t.listing.txt'
& .\.bin\dasm.exe .\64-tng.asm   -f1 -v4 '-o64-tng.prg'   '-l64-tng.listing.txt'
& .\.bin\dasm.exe .\newmodem.asm -f1 -v4 '-onewmodem.prg' '-lnewmodem.listing.txt'