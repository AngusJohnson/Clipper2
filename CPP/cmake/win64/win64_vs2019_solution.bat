@echo off
setlocal enabledelayedexpansion

pushd "%~dp0"

md ..\..\build_vs2019

pushd "..\..\build_vs2019"

cmake ../ -G "Visual Studio 16 2019" -A x64 -DCMAKE_INSTALL_PREFIX:PATH="%~dp0\..\..\install"

popd
popd