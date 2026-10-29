# Clipper2

### A Polygon <a href="https://en.wikipedia.org/wiki/Clipping_(computer_graphics)">Clipping</a>, <a href="https://en.wikipedia.org/wiki/Parallel_curve">Offsetting</a> library in C++<br>
[![GitHub Actions C++ status](https://github.com/PolyClipping/Clipper2/actions/workflows/actions_cpp.yml/badge.svg)](https://github.com/PolyClipping/Clipper2/actions/workflows/actions_cpp.yml)&nbsp;[![License](https://img.shields.io/badge/License-Boost_1.0-lightblue.svg)](https://www.boost.org/LICENSE_1_0.txt)
[![documentation](https://user-images.githubusercontent.com/5280692/187832279-b2a43890-da80-4888-95fe-793f092be372.svg)](https://github.com/PolyClipping/Clipper2/wiki)

The <b>Clipper2</b> library performs **intersection**, **union**, **difference** and **XOR** boolean operations on both simple and complex polygons. It also performs **polygon offsetting**. This is a major update of the original <a href="https://sourceforge.net/projects/polyclipping/"><b>Clipper</b></a> library that was written around 2008 by Angus Johnson. That library is now called <b>Clipper1</b>, and while it still works very well, Clipper2 is just [better](https://github.com/PolyClipping/Clipper2/wiki/Changes).

In 2026, the original repository at [https://github.com/AngusJohnson/Clipper2](https://github.com/AngusJohnson/Clipper2) was archived and primary development has moved to this location.

### Compilers
<b>Clipper2</b> is written in [C++](https://github.com/PolyClipping/Clipper2/tree/main/CPP) and requires C++17. The library can also be accessed from other languages by dynamically linking to exported functions in the [C++ compiled Clipper2 library](https://github.com/PolyClipping/Clipper2/tree/main/DLL).

The C# and Delphi versions of Clipper2 remain available in the archived [original repository](https://github.com/AngusJohnson/Clipper2).

### Documentation

 <a href="https://github.com/PolyClipping/Clipper2/wiki"><b>Extensive documentation (wiki)</b></a>
<br><br>

### Examples

**Clipping**

https://github.com/PolyClipping/Clipper2/blob/fa7add77364eb3877dd1b8caf9b3cbd5486347f6/CPP/Examples/SimpleClipping/SimpleClipping.cpp#L29-L34

![clipperB](https://user-images.githubusercontent.com/5280692/178123810-1719a1f5-25c3-4a9e-b419-e575ff056272.svg)

**Inflating (aka Offsetting)**

https://github.com/PolyClipping/Clipper2/blob/fa7add77364eb3877dd1b8caf9b3cbd5486347f6/CPP/Examples/Inflate/Inflate.cpp#L36-L47

![rabbit](https://github.com/user-attachments/assets/a0f2f43c-f0a3-45ec-887d-d9ca34256088)
![rabbit_offset](https://github.com/user-attachments/assets/ca05688e-293f-4596-86ab-df529694e778)

<hr>

### Ports to other languages
| lang. | link |
| ------ | ------ |
| **golang** | https://github.com/epit3d/goclipper2 |
| **Haxe** | https://github.com/jeremyfa/Clipper2/tree/main/Haxe |
| **Java** | https://github.com/micycle1/Clipper2-java/ |
| **Kotlin** | https://github.com/Monkey-Maestro/clipper2-kotlin |
| **Lua** | https://github.com/Ark223/Clipper2-Lua |
| **Rust** | https://github.com/larsbrubaker/clipper2-rust |
| **Swift** | https://github.com/zyunlongz/clipper2-swift |
| **TypeScript** | https://github.com/countertype/clipper2-ts |
| **WASM** | https://github.com/ErikSom/Clipper2-WASM/ |
