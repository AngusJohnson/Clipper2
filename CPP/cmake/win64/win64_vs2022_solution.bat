@echo off
setlocal enabledelayedexpansion

pushd "%~dp0"

md ..\..\build

pushd "..\..\build"

cmake ../ -G "Visual Studio 17 2022" -A x64 

popd
popd