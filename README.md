# Clipper2
### A Polygon <a href="http://www.angusj.com/delphi/clipper/documentation/Docs/Units/ClipperLib/Types/ClipType.htm">Clipping</a> and <a href="http://www.angusj.com/delphi/clipper/documentation/Docs/Units/ClipperLib/Classes/ClipperOffset/_Body.htm">Offsetting</a> library (in C++, C# &amp; Delphi)<br>
[![GitHub Actions C++ status](https://github.com/AngusJohnson/Clipper2/actions/workflows/actions_cpp.yml/badge.svg)](https://github.com/AngusJohnson/Clipper2/actions/workflows/actions_cpp.yml)&nbsp;[![C#](https://github.com/AngusJohnson/Clipper2/actions/workflows/actions_csharp.yml/badge.svg)](https://github.com/AngusJohnson/Clipper2/actions/workflows/actions_csharp.yml)&nbsp;[![License](https://img.shields.io/badge/License-Boost_1.0-lightblue.svg)](https://www.boost.org/LICENSE_1_0.txt)

<b>Clipper2</b> is a major update of my original <a href="https://sourceforge.net/projects/polyclipping/"><b>Clipper</b></a> library which I'm now calling <b>Clipper1</b>. Clipper1 was written over 10 years ago and, while it still works very well, Clipper2 is better in just about every way: 
<ul>
<li>The code is cleaner and faster.</li>
<li>Clipper2 is easier to use with simple functions that handle all common uses.</li> 
<li>Floating point coordinates no longer need to be converted to integers before passing them into the library.</li>
<li>And micro self-intersections (integer rounding artifacts) and polygons with touching edges are now rarely seen in solutions.</li>
</ul>
</p>
<p class="Body"> 
And Clipper2 has just about all the features of Clipper1 that sets Clipper apart from other polygon clipping libraries, including: 
<ul>
<li>being able to clip complex self-intersecting polygons</li>
<li>support polygons with multiple filling rules (EvenOdd, NonZero, Positive, Negative)</li>
<li>is numerically robust, and</li>
<li>is free to use even in commercial applications with a very simple license</li>
</ul>
<br>

### Documentation

There's more detail in the <a href="https://github.com/AngusJohnson/Clipper2/tree/main/Docs"><b>Docs</b></a> folder above and in the partially completed online <a href="http://www.angusj.com/clipper2/Docs/Overview.htm"><b>HTML help</b></a>.
<br><br>

### Example

<pre>
      //C++
      Paths64 subject, clip, solution;
      subject.push_back(MakePath("100, 50, 10, 79, 65, 2, 65, 98, 10, 21"));
      clip.push_back(MakePath("98, 63, 4, 68, 77, 8, 52, 100, 19, 12"));
      solution = Intersect(subject, clip, FillRule::NonZero);

      //C#
      Paths64 subj = new Paths64();
      Paths64 clip = new Paths64();
      subj.Add(Clipper.MakePath(new int[] { 100, 50, 10, 79, 65, 2, 65, 98, 10, 21 }));
      clip.Add(Clipper.MakePath(new int[] { 98, 63, 4, 68, 77, 8, 52, 100, 19, 12 }));
      Paths64 solution = Clipper.Intersect(subj, clip, FillRule.NonZero);
      
      //Delphi
      var 
        subject, clip, solution: TPaths64;
      begin
        SetLength(subject, 1);
        subject[0] := MakePath([100, 50, 10, 79, 65, 2, 65, 98, 10, 21]);
        SetLength(clip, 1);
        clip[0] := MakePath([98, 63, 4, 68, 77, 8, 52, 100, 19, 12]);
        solution := Intersect( subject, clip, frNonZero);</pre>
![clipperB](https://user-images.githubusercontent.com/5280692/178123810-1719a1f5-25c3-4a9e-b419-e575ff056272.svg)


### Clipper1 vs Clipper2 performance
![clipper_performance2](https://user-images.githubusercontent.com/5280692/178123605-62e7ad87-cd71-4365-9296-c6e7ebfc7218.png)
