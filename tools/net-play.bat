@echo off
set /P port= "TCP Port to use (default:12345): "
if "%port%"=="" set port=12345
choice /C SC /M "act as Server or Client "
if errorlevel 2 goto client
if errorlevel 1 goto server
:client
set /P host= "Server IP or Hostname: "
@echo on
cribbage -p %port% -c --host %host% 
goto rest
:server
@echo on
cribbage -p %port% -s
:rest
@pause
