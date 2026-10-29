# Clipper2 DLL

Developers using languages other than C++ can access almost all of Clipper2's features by dynamically linking to a C++ compiled library. Exported functions and their related data structures are well documented in [clipper.export.h](https://github.com/PolyClipping/Clipper2/blob/main/CPP/Clipper2Lib/include/clipper2/clipper.export.h). And the latest precompiled DLLs can be found in [Clipper2's Releases](https://github.com/PolyClipping/Clipper2/releases).

This folder contains Visual Studio projects that compile **Clipper2** into Windows DLLs (both 32bit and 64bit). `Clipper2_DLL.sln` builds the standard library and `Clipper2_Z_DLL.sln` builds a variant with `USINGZ` defined.
