@echo off
setlocal enabledelayedexpansion

pushd "%~dp0"

md ..\..\build

pushd "..\..\build"

cmake ../ -G "Visual Studio 17 2022" -A x64 -DCMAKE_INSTALL_PREFIX:PATH="%~dp0\..\..\install"

popd
popd