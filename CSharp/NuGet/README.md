# Clipper2
### A Polygon Clipping, Offsetting, and Triangulation library (in C++, C# &amp; Delphi)
[![License](https://img.shields.io/badge/License-Boost_1.0-lightblue.svg)](https://www.boost.org/LICENSE_1_0.txt)
[![documentation](https://user-images.githubusercontent.com/5280692/187832279-b2a43890-da80-4888-95fe-793f092be372.svg)](http://www.angusj.com/clipper2/Docs/Overview.htm)

**Clipper2** is a major update of my original [**Clipper library**](https://sourceforge.net/projects/polyclipping/) which I'm now calling **Clipper1**. Clipper1 was written 15 years ago and still works very well, but Clipper2 is better.

 
### Example

      Paths64 subj = new Paths64();
      Paths64 clip = new Paths64();
      subj.Add(Clipper.MakePath(new int[] { 100, 50, 10, 79, 65, 2, 65, 98, 10, 21 }));
      clip.Add(Clipper.MakePath(new int[] { 98, 63, 4, 68, 77, 8, 52, 100, 19, 12 }));
      Paths64 solution = Clipper.Intersect(subj, clip, FillRule.NonZero);
      
![clipperB](https://user-images.githubusercontent.com/5280692/178123810-1719a1f5-25c3-4a9e-b419-e575ff056272.svg)
