# Clipper2

This is a **pre-release beta version** of Clipper2. While my original <a href="https://sourceforge.net/projects/polyclipping/">**Clipper Library**</a> (i.e. versions 1 through to 6.4.2) was very functional, in many places it is/was messy. Clipper2 is a major rewrite. New features include:
<ul>
  <li>Support for floating point coordinates. While the library still performs all clipping operations using integer coordinates that preserve numerical robustness, floating point conversions are now managed internally.
  <li>There's more complete and more efficient removal of spikes and micro-self-intersections, and better merging of touching polygons from clipping solutions. (The latter has just been added to the Delphi code).
  <li>And it's even a bit faster.
</ul> 
<br>

These changes also affect how the library is used, including:
1. The <code>cInt</code> type used for path coordinates has been replaced with native 64bit integer types (<code>long</code>, <code>Int64</code> or <code>int64_t</code>).
2. The <code>IntPoint</code> and <code>IntRect</code> types have also been renamed <code>Point64</code> and <code>Rect64</code> respectively. There is new support for floating point coordinates, with the new PointD and RectD classes indicating double float values.
3. The <code>PolyFillType</code> enumeration has been renamed <code>FillRule</code>.
4. The Clipper class no longer has <code>AddPath</code> and <code>AddPaths</code> methods. These have been replaced with <code>AddSubject</code>, <code>AddOpenSubject</code> and <code>AddClip</code> methods.
5. The <code>Clipper</code> class's <code>Execute</code> parameters have changed with the removal of the second FillRule parameter, and addition of an optional OpenSolutions parameter. (The second FillRule parameter was almost never needed and probably confusing).
6. Collinear vertices are retained by default, in contrast to the default behavior of Clipper1.
7. While the Clipper class remains integral to all clipping routines, most clipping can now be performed using simple functions that hide Clipper class construction and use.
8. The <code>Polytree</code> class now only contains closed paths (ie polygons) since open paths can't contain polygons and open paths in solutions are now returned via Execute's OpenSolutions parameter.
9. When offestting open paths, the meaning of the offset <code>delta</code> has changed. This value now represents the <i>total</i> offset width and makes it equivalent to <code>line width</code>.
 

## Additional notes:


### Definitions:

Path **segments** are the lines between path vertices. In closed paths (polygons), these are commonly referred to as **edges**.<br>
Segments are **touching** when they are collinear and at least partially overlap one another.<br>
Polygons are touching when they contain touching edges.<br>
**Clipping** initially referred to the process of removing or "cutting away" everything in an image that was outside a rectangular *clipping* window. However, this process has now been generalized to include *clipping* with non-rectangular windows, and to include union, difference and XOR boolean operations too. In this library, *images* are defined by one or more **subject** vector paths, where these paths may be open or closed. **Clip** paths, as opposed to subject paths, must be closed. (The library doesn't support clipping with open paths.)<br><br>


### Clipped Solutions:

Clipped solutions are not guaranteed to be in their simplest forms. For example, solutions may on occasions have "touching" edges, possibly within the same polygon.<br><br>


### Clipping open paths:

The Clipper library clips **subject** paths which may be closed (polygons) or open (polylines). To fully understand the library's open path clipping, it's important to note that clipping is performed using a *sweep line* algorithm that progressing top-down in a Cartesian plane (ie from vertices with the largest Y coordinates to those with the least Y coordinates). Open path segments that touch a clipping boundary may or may not be part of the clipped solution. This will depend on which side of the clip boundary the path was on *prior to touching*. However *prior* in this case isn't referring to the segment that's closer to the start of the path. Instead prior is *relative to the sweep's direction*. While sweeping top-down in the Cartesian plane, *prior* refers to the segment immediately above. So:
<ul>
<li>when an open path prior to touching a clipping boundary lies outside the clipping region, the touching segment will <i>not</i> be part of the clipping solution</li>
<li>when an open path prior to touching a clipping boundary lies inside the clipping region, the touching segment will be part of the clipping solution</li>
<li>when an open path segment starts or ends while touching a boundary and also has no <i>prior</i> segment, its placement with regard to the boundary will depend on the heading of the following segment (ie immediately below in a Cartesian plane). If the following segment heads inside the clipping boundary, the terminating segment will be included in the clipping solution, otherwise it will be excluded.</li>
<li>when an open path touches a clipping boundary along its entire length, whether this path becomes part of the clipping solution or not is undefined</li>
</ul>
Examples:

	subj_open = new Paths64();
	clip = new Paths64();
	subj_open.Add(MakePath(new int[] { 20,40, 20,30, 10,30 })); //will not be part of the solution
	clip.Add(MakePath(new int[] { 20,20, 40,20, 40,40, 20,40 }));
	
	subj_open.Add(MakePath(new int[] { 45,40, 45,30, 55,30 })); //will be part of the solution
	clip.Add(MakePath(new int[] { 45,20, 65,20, 65,40, 45,40 }));
	subj_open.Add(MakePath(new int[] { 70,35, 70,25 })); //undefined
	clip.Add(MakePath(new int[] { 70,20, 90,20, 90,40, 70,40 }));


### PreserveCollinear property:

This property only pertains to **closed paths**. Paths will sometimes have consecutive segments that make a single straight edge, and where shared vertices can be removed without altering path shape. This removal simplifies path definitions and is usually the preferred option, though not always. However whenever solutions contain **spikes** from to consecutive edges reversing back on each other, these spikes will always be removed irrespective of whether ``PreserveCollinear`` is enabled or disabled.<br>

Example:

	Path64 badPath = MakePath(new int[] { 270, 230160, 230160, 160270, 160270, 2010, 2010, 7060, 7060, 14010, 14010, 20270, 20 })); 


### Examples of using library functions:

**Delphi:**<br>
    
    uses Clipper;

    //code main entry 
    var
      subject, clip: TPaths64;      
    begin
      SetLength(subject, 1);
      subject[0] := MakePath([100, 50, 10, 79, 65, 2, 65, 98, 10, 21]);
      setLength(clip, 1);
      clip[0] := MakePath([80, 50, 69, 73, 43, 79, 23, 63, 23, 37, 43, 21, 69, 27]);
      var solution1 := Union(subject, nil, frEvenOdd);            
      var solution2 := Union(subject, nil, frNonZero);      
      var solution3 := Union(subject, clip, frNonZero);      
      var solution4 := Intersect(subject, clip, frNonZero);
      var solution5 := Intersect(subject, clip, frEvenOdd);
      
      subject[0] := MakePath([93,50,77,84,40,92,11,69,11,31,40,8,77,16]);
      var solution6 := InflatePaths(subject, 5, jtMiter, etPolygon);
      var solution7 := InflatePaths(subject, -5, jtMiter, etPolygon);
      var solution8 := InflatePaths(subject, 10, jtMiter, etSquare);
      var solution9 := InflatePaths(subject, 10, jtMiter, etJoined);
    end;

![union_evenodd](https://user-images.githubusercontent.com/5280692/159103161-4c4e0c51-98f3-4ff4-9364-4c2cc57af774.png)
![union_nonzero](https://user-images.githubusercontent.com/5280692/159098650-9923ffe9-a5fb-49a4-9d34-9ef1e5aae8e4.png)
![union2_nonzero](https://user-images.githubusercontent.com/5280692/159098431-378a4a85-be83-4412-a6f0-09e88234928f.png)
![intersect_nonzero](https://user-images.githubusercontent.com/5280692/159098290-85c67eec-04a2-4ea7-9b5c-b2120d3cf5bd.png)
![intersect_evenodd](https://user-images.githubusercontent.com/5280692/159103132-153c6428-4962-4240-b7a3-799fc639553c.png)
<br>
![inflate_polygon](https://user-images.githubusercontent.com/5280692/159106061-1d1e14c7-bdff-478d-b5b6-7340478edc5d.png)
![deflate_polygon](https://user-images.githubusercontent.com/5280692/159106069-e01ad782-fd6e-4480-972c-e96db51948f8.png)
![inflate_square](https://user-images.githubusercontent.com/5280692/159106083-f16f5508-7daa-4b0c-a7b3-8a5381a4db51.png)
![inflate_joined](https://user-images.githubusercontent.com/5280692/159106093-ba17fb6a-0adb-4622-b16c-835ac061e3d8.png)


**C#:**<br>
          
    using Clipper2Lib;
    using Path64 = List<Point64>;
    using Paths64 = List<List<Point64>>;
    
    //code main entry 
    Paths64 subj = new Paths64();
    subj.Add(ClipperFunc.MakePath(new int[] { 100, 50, 10, 79, 65, 2, 65, 98, 10, 21 }));
    Paths64 clip = new Paths64();
    clip.Add(ClipperFunc.MakePath(new int[] { 80, 50, 69, 73, 43, 79, 23, 63, 23, 37, 43, 21, 69, 27 }));
    Paths64 solution1 = ClipperFunc.Union(subj, null, FillRule.EvenOdd);
    Paths64 solution2 = ClipperFunc.Union(subj, null, FillRule.NonZero);
    Paths64 solution3 = ClipperFunc.Union(subj, clip, FillRule.NonZero);
    Paths64 solution4 = ClipperFunc.Intersect(subj, clip, FillRule.NonZero);
    Paths64 solution5 = ClipperFunc.Intersect(subj, clip, FillRule.EvenOdd);

    subj.Clear();
    subj.Add(ClipperFunc.MakePath(new int[] { 93, 50, 77, 84, 40, 92, 11, 69, 11, 31, 40, 8, 77, 16 }));
    Paths64 solution6 = ClipperFunc.InflatePaths(subj, 5.0, JoinType.Miter, EndType.Polygon);
    Paths64 solution7 = ClipperFunc.InflatePaths(subj, -5.0, JoinType.Miter, EndType.Polygon);
    Paths64 solution8 = ClipperFunc.InflatePaths(subj, 10.0, JoinType.Miter, EndType.Square);
    Paths64 solution9 = ClipperFunc.InflatePaths(subj, 10.0, JoinType.Miter, EndType.Joined);
