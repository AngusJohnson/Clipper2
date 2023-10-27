# Clipper2
### A Polygon <a href="https://en.wikipedia.org/wiki/Clipping_(computer_graphics)">Clipping</a> and <a href="https://en.wikipedia.org/wiki/Parallel_curve">Offsetting</a> library (in C++, C# &amp; Delphi)<br>
[![GitHub Actions C++ status](https://github.com/AngusJohnson/Clipper2/actions/workflows/actions_cpp.yml/badge.svg)](https://github.com/AngusJohnson/Clipper2/actions/workflows/actions_cpp.yml)&nbsp;[![C#](https://github.com/AngusJohnson/Clipper2/actions/workflows/actions_csharp.yml/badge.svg)](https://github.com/AngusJohnson/Clipper2/actions/workflows/actions_csharp.yml)&nbsp;[![License](https://img.shields.io/badge/License-Boost_1.0-lightblue.svg)](https://www.boost.org/LICENSE_1_0.txt)
[![Nuget](https://img.shields.io/nuget/v/Clipper2?color=green)](https://www.nuget.org/packages/Clipper2)
[![documentation](https://user-images.githubusercontent.com/5280692/187832279-b2a43890-da80-4888-95fe-793f092be372.svg)](http://www.angusj.com/clipper2/Docs/Overview.htm)

# Clipper2 Library

The **Clipper2 library** is a powerful tool for performing a wide range of boolean operations on both simple and complex polygons. These operations include intersection, union, difference, and XOR, making it an essential resource for various applications. Additionally, Clipper2 provides polygon offsetting capabilities, further enhancing its functionality.

## What's New in Clipper2

[Clipper2](http://www.angusj.com/clipper2/Docs/Changes.htm) represents a significant upgrade from the original Clipper library, which was developed over a decade ago and is now referred to as Clipper1. While Clipper1 continues to be effective, Clipper2 outshines it in nearly every aspect, offering improved performance and a more extensive set of features.

## Supported Compilers

- **C++:** Clipper2 is compatible with C++17, with the potential for adaptation to C++11 with minor adjustments.
- **C#:** The core library utilizes Standard Library 2.0, while the accompanying sample code is designed for .NET6.
- **Delphi:** Clipper2 can be compiled with any version of Delphi dating back to Delphi.

This updated version of the library provides a versatile and efficient solution for working with polygons, making it an invaluable resource for developers across various platforms and programming languages.

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
**Java**: https://github.com/micycle1/Clipper2-java/<br>
**Kotlin**: https://github.com/Monkey-Maestro/clipper2-kotlin<br>
**golang**: https://github.com/epit3d/goclipper2
