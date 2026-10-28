# Clipper2

<mark>**The future of Clipper development is now [here](https://github.com/PolyClipping/Clipper2).**</mark><br>

<mark>**The bad news:** As of now I’m retiring from Clipper2 development. While this Clipper2 repository will remain, it will not be updated. So please don't post new Issues or Pull Requests here.</mark><br>
<mark>**The good news:** I’m delighted to announce that **[Seth Hillibrand](https://github.com/sethhillbrand)** has agreed to take over Clipper maintenance and development, and this will continue at the link above.</mark><br>

**<mark>The long version:</mark>**<br>
<mark>Software development has been a wonderful hobby of mine for almost 50 years. I started Clipper more than 15 years ago, and it has been 15 years of fun and challenge. But over the last few months I’ve come to realise that the challenge has gone, and it has just become a responsibility.</mark><br>
<mark>So I’m passing the baton. And I’m delighted to announce [Seth Hillibrand](https://github.com/sethhillbrand) has agreed to pick it up and will be taking over Clipper’s ongoing maintenance and development. Seth is the founder and lead developer of KiCad Services, and has used Clipper almost from the beginning. I’m extremely confident that he has the skills and temperament to see Clipper continue to improve. But Clipper will, however, become a C++ only library; it will be up to third parties to translate Clipper into other languages, or to use it as a dynamic linked library.</mark>

**<mark>Transitioning to the new Clipper2 repository:</mark>**<br>
<mark>I suggest that those who have contributed Pull Requests do so again [here](https://github.com/PolyClipping/Clipper2).</mark><br>
<mark>And I suggest that those who have raised Issues also resubmit them [here](https://github.com/PolyClipping/Clipper2) or, even better, consider converting them into Pull Requests if you are able to do the debugging.</mark><br>

<mark>Finally, as I said above, creating and developing Clipper has been a lot of fun. I have learned so much from you, the library’s users, and from your code contributions. And that has been enormously encouraging. Thank you.</mark>

<hr>

### A Polygon <a href="https://en.wikipedia.org/wiki/Clipping_(computer_graphics)">Clipping</a>, <a href="https://en.wikipedia.org/wiki/Parallel_curve">Offsetting</a> library (in C++, C# &amp; Delphi)<br>
[![GitHub Actions C++ status](https://github.com/AngusJohnson/Clipper2/actions/workflows/actions_cpp.yml/badge.svg)](https://github.com/AngusJohnson/Clipper2/actions/workflows/actions_cpp.yml)&nbsp;[![C#](https://github.com/AngusJohnson/Clipper2/actions/workflows/actions_csharp.yml/badge.svg)](https://github.com/AngusJohnson/Clipper2/actions/workflows/actions_csharp.yml)&nbsp;[![License](https://img.shields.io/badge/License-Boost_1.0-lightblue.svg)](https://www.boost.org/LICENSE_1_0.txt)
[![Nuget](https://img.shields.io/nuget/v/Clipper2?color=green)](https://www.nuget.org/packages/Clipper2)
[![documentation](https://user-images.githubusercontent.com/5280692/187832279-b2a43890-da80-4888-95fe-793f092be372.svg)](https://www.angusj.com/clipper2/Docs/Overview.htm)

The <b>Clipper2</b> library performs **intersection**, **union**, **difference** and **XOR** boolean operations on both simple and complex polygons. It also performs **polygon offsetting**. This is a major update of my original <a href="https://sourceforge.net/projects/polyclipping/"><b>Clipper</b></a> library that was written about 15 years ago. That library I'm now calling <b>Clipper1</b>, and while it still works very well, Clipper2 is just [better](https://www.angusj.com/clipper2/Docs/Changes.htm).

### Compilers
<b>Clipper2</b> can be compiled using any one of three supported programming languages: C++, or C#, or Delphi Pascal. The library can also be accessed from other languages by dynamically linking to exported functions in the [C++ compiled Clipper2 library](https://github.com/AngusJohnson/Clipper2/tree/main/DLL). (The C++ compiled code is [measurably](https://www.angusj.com/clipper2/Docs/Changes.htm) faster so, where performance is critical, even C# and Delphi developers may prefer this approach in application development.) 
| Lang. | Requirements |
| --- | --- |
| [**C++:**](https://github.com/AngusJohnson/Clipper2/tree/main/CPP) | Requires C++17, **or**| 
| [**C#:**](https://github.com/AngusJohnson/Clipper2/tree/main/CSharp) | The library uses Standard Library 2.0 but the sample code uses .NET6, **or**| 
| [**Delphi:**](https://github.com/AngusJohnson/Clipper2/tree/main/Delphi) | Compiles with any version of Delphi from version 7 to current.| 

### Documentation

 <a href="https://www.angusj.com/clipper2/Docs/Overview.htm"><b>Extensive HTML documentation</b></a>
<br><br>

### Examples

**Clipping**

<details open>
  <summary><b>C++</b></summary>
https://github.com/AngusJohnson/Clipper2/blob/fa7add77364eb3877dd1b8caf9b3cbd5486347f6/CPP/Examples/SimpleClipping/SimpleClipping.cpp#L29-L34
</details>

<details>
  <summary><b>C#</b></summary>
https://github.com/AngusJohnson/Clipper2/blob/fa7add77364eb3877dd1b8caf9b3cbd5486347f6/CSharp/Clipper2Lib.Examples/ConsoleDemo/Main.cs#L112-L116
</details>
 
<details>
  <summary><b>Delphi</b></summary>
https://github.com/AngusJohnson/Clipper2/blob/fa7add77364eb3877dd1b8caf9b3cbd5486347f6/Delphi/Examples/Example1/Example1.dpr#L21-L26
</details>

![clipperB](https://user-images.githubusercontent.com/5280692/178123810-1719a1f5-25c3-4a9e-b419-e575ff056272.svg)

**Inflating (aka Offsetting)**

<details open>
  <summary><b>C++</b></summary>
https://github.com/AngusJohnson/Clipper2/blob/fa7add77364eb3877dd1b8caf9b3cbd5486347f6/CPP/Examples/Inflate/Inflate.cpp#L36-L47
</details>

<details>
  <summary><b>C#</b></summary>
https://github.com/AngusJohnson/Clipper2/blob/fa7add77364eb3877dd1b8caf9b3cbd5486347f6/CSharp/Clipper2Lib.Examples/InflateDemo/Main.cs#L89-L100
</details>
 
<details>
  <summary><b>Delphi</b></summary>
https://github.com/AngusJohnson/Clipper2/blob/fa7add77364eb3877dd1b8caf9b3cbd5486347f6/Delphi/Examples/Example2/Example2.dpr#L20-L31
</details>

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
