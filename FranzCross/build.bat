@echo off
raco exe -o .\Franz.exe --gui --ico .\assets\icon.ico .\main.rkt
raco dist dist .\Franz.exe
