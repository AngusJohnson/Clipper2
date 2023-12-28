# Clipper2
### A Polygon <a href="https://en.wikipedia.org/wiki/Clipping_(computer_graphics)">Clipping</a> and <a href="https://en.wikipedia.org/wiki/Parallel_curve">Offsetting</a> library (in C++, C# &amp; Delphi)<br>
[![GitHub Actions C++ status](https://github.com/AngusJohnson/Clipper2/actions/workflows/actions_cpp.yml/badge.svg)](https://github.com/AngusJohnson/Clipper2/actions/workflows/actions_cpp.yml)&nbsp;[![C#](https://github.com/AngusJohnson/Clipper2/actions/workflows/actions_csharp.yml/badge.svg)](https://github.com/AngusJohnson/Clipper2/actions/workflows/actions_csharp.yml)&nbsp;[![License](https://img.shields.io/badge/License-Boost_1.0-lightblue.svg)](https://www.boost.org/LICENSE_1_0.txt)
[![Nuget](https://img.shields.io/nuget/v/Clipper2?color=green)](https://www.nuget.org/packages/Clipper2)
[![documentation](https://user-images.githubusercontent.com/5280692/187832279-b2a43890-da80-4888-95fe-793f092be372.svg)](http://www.angusj.com/clipper2/Docs/Overview.htm)

The <b>Clipper2</b> library performs **intersection**, **union**, **difference** and **XOR** boolean operations on both simple and complex polygons. It also performs polygon offsetting. This is a major update of my original <a href="https://sourceforge.net/projects/polyclipping/"><b>Clipper</b></a> library that was written over 10 years ago. That library I'm now calling <b>Clipper1</b>, and while it still works very well, Clipper2 is [better](http://www.angusj.com/clipper2/Docs/Changes.htm) in just about every way.

### Compilers
<b>Clipper2</b> can be compiled using either C++, or C#, or Delphi Pascal. The library can also be accessed from other programming languages by dynamically linking to exported functions in the [C++ compiled Clipper2 library](https://github.com/AngusJohnson/Clipper2/tree/main/DLL). (Since the C++ compiled code is [measurably](https://www.angusj.com/clipper2/Docs/Changes.htm) faster, C# and Delphi developers may also prefer this approach in applications where the library's performance is critical.) 
| Lang. | Requirements |
| --- | --- |
| [**C++:**](https://github.com/AngusJohnson/Clipper2/tree/main/CPP) | Requires C++17 (could be modified to C++11 with relatively minor changes), **or**| 
| [**C#:**](https://github.com/AngusJohnson/Clipper2/tree/main/CSharp) | The library uses Standard Library 2.0 but the sample code uses .NET6, **or**| 
| [**Delphi:**](https://github.com/AngusJohnson/Clipper2/tree/main/Delphi) | Compiles with any version of Delphi from version 7 to current.| 

### Documentation

 <a href="http://www.angusj.com/clipper2/Docs/Overview.htm"><b>Extensive HTML documentation</b></a>
<br><br>

### Examples

<pre>
      //C++
      Paths64 subject, clip, solution;
      subject.push_back(MakePath({100, 50, 10, 79, 65, 2, 65, 98, 10, 21}));
      clip.push_back(MakePath({98, 63, 4, 68, 77, 8, 52, 100, 19, 12}));
      solution = Intersect(subject, clip, FillRule::NonZero);</pre>
<pre>      //C#
      Paths64 subj = new Paths64();
      Paths64 clip = new Paths64();
      subj.Add(Clipper.MakePath(new int[] { 100, 50, 10, 79, 65, 2, 65, 98, 10, 21 }));
      clip.Add(Clipper.MakePath(new int[] { 98, 63, 4, 68, 77, 8, 52, 100, 19, 12 }));
      Paths64 solution = Clipper.Intersect(subj, clip, FillRule.NonZero);</pre>
<pre>      //Delphi
      var 
        subject, clip, solution: TPaths64;
      begin
        SetLength(subject, 1);
        subject[0] := MakePath([100, 50, 10, 79, 65, 2, 65, 98, 10, 21]);
        SetLength(clip, 1);
        clip[0] := MakePath([98, 63, 4, 68, 77, 8, 52, 100, 19, 12]);
        solution := Intersect( subject, clip, frNonZero);</pre>
![clipperB](https://user-images.githubusercontent.com/5280692/178123810-1719a1f5-25c3-4a9e-b419-e575ff056272.svg)

<hr>

### Ports to other languages
| lang. | link |
| ------ | ------ |
| **WASM** | https://github.com/ErikSom/Clipper2-WASM/ |
| **Java** | https://github.com/micycle1/Clipper2-java/ |
| **Kotlin** | https://github.com/Monkey-Maestro/clipper2-kotlin |
| **golang** | https://github.com/epit3d/goclipper2 |
