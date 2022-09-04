@echo off
setlocal enabledelayedexpansion

pushd "%~dp0"

md ..\..\build

pushd "..\..\build"

cmake ../ -G "Visual Studio 16 2019" -A x64 

popd
popd