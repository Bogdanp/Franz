@echo off
raco exe ^
  -o .\Franz.exe ^
  --gui ^
  --ico .\assets\icon.ico ^
  ++lang lua ^
  ++lib lua/lang/runtime-config ^
  ++lib racket/runtime-config ^
  .\main.rkt
raco dist dist .\Franz.exe
