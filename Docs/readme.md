# Clipper2

This is a <b>pre-release beta version</b> of Clipper2. While my original <a href="https://sourceforge.net/projects/polyclipping/"><b>Clipper Library</b></a> (i.e. versions 1 through to 6.4.2) was very functional, in many places it is/was messy. This new version is a significant rewrite so the code should be a little easier to understand, though unfortunately still very complex. However, because this is still a pre-release version, expect some bug, even though both the Delphi code and its C# port have been fairly extensively tested. A C++ port is also in the pipeline.<br>

There are many changes in Clipper2 that affect how it's used. These are the more important ones:
1. The PolyFillType enumeration has been renamed FillRule.
2. The cInt type used for path coordinates has been replaced with native 64bit integer types (long, Int64 or int64_t).
3. The IntPoint and IntRect types have also been renamed Point64 and Rect64 respectively. There is new support for floating point coordinates, with the new PointD and RectD classes indicating double float values.
4. The Clipper class no longer has AddPath and AddPaths methods. These have been replaced with AddSubject, AddOpenSubject and AddClip methods.
5. The Clipper class's Execute parameters have changed with the removal of the second FillRule parameter, and addition of an optional OpenSolutions parameter. (The second FillRule parameter was almost never needed and probably confusing).
6. The Polytree class now only contains closed paths (ie polygons) since open paths can't contain polygons and open paths in solutions are now returned via Execute's OpenSolutions parameter.
7. Clipper2's Offset behavior for open paths has changed compared to Clipper1. The offset value is now the full width of the resulting offset, rather than the per-side value. To get similar results to the Clipper1 behavior, double the offset value.
8. Collinear vertices are retained by default, in contrast to the default behavior of Clipper1.
 
When I originally translated this library from Delphi (Pascal) to C# and C++, I deliberately kept a strong Delphi naming style as I thought this would help with maintenance. In hindsight this was a mistake, and just made the C# andC++ code look odd. With this new version, I've attempted to adopt a more conventional naming style for each language, while admitting that I still have very limited coding experience in both these languages.<br><br> 


<b>Additional notes:</b>

	using Path64 = List<Point64>;
	using Paths64 = List<List<Point64>>;
	using PathD = List<PointD>;
	using PathsD = List<List<PointD>>;
	
	//using integer coordinates
	Clipper clipper = new Clipper(); 
	clipper.AddSubject(subjects);
	clipper.AddClip(clips);
	clipper.Execute(ClipType.Intersection, FillRule.EvenOdd, closedSolution);
	
	//using floating point (double) coordinates
	ClipperD clipper = new ClipperD(); 
	clipper.AddSubject(subjects);
	clipper.AddClip(clips);
	clipper.Execute(ClipType.Intersection, FillRule.EvenOdd, closedSolution);
  
<b>Definitions:</b><br>
Path "segments" are the lines between path vertices. In closed paths these are commonly referred to as "edges".<br>
Segments are "touching" when they are collinear and at least partially overlap one another.<br>
Polygons are touching when they contain touching edges.<br><br>

<b>Clipped Solutions:</b><br>
A clipped solution often won't be in its simplest form. For example, solutions may have "touching" edges.<br><br>

<b>Clipping open paths:</b><br>

The Clipper library clips both polygons (closed paths) and polylines (open paths). However, these <i>subject</i> paths can only be "clipped" with closed paths. In other words polygons and polylines can't be clipped by polylines. Clipping is performed using a "sweep line" algorithm, progressing from vertices with the largest Y coordinates to those with the least Y coordinates (ie top-down in a Cartesian plane).<br>

Open path segments that touch a clipping boundary may or may not be part of the clipped solution. This will depend on which side of the clip boundary the path was on <i>prior to touching</i>. However <i>prior</i> in this case isn't necessarily referring to the segment that's closer to the start of the path. Instead <i>prior is relative to the sweep direction</i>. So while sweeping top-down in the Cartesian plane, prior here refer to a segment immediately above. So, a touching open segment is included in a clipping solution:
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

<br>
<b>PreserveCollinear property:</b><br><br>

This property only pertains to <b>closed paths</b>. Paths commonly have consecutive edges that are collinear, where the shared vertex can be removed (and paths simplified) without altering the shape of these paths. This simplification is commonly but not always preferred for clipping solutions. However, when consecutive edges are collinear AND ALSO change direction 180 degrees, causing spikes, these are rarely desired. 'Spikes' will always be removed from closed path solutions, irrespective of the PreserveCollinear property.<br>

Very occasionally in clipping solutions there will be touching edges within the same polygons. While these solutions are technically correct (in that the polygon filled regions represent correct solutions), I consider this a bug and hope to have this addressed before the formal release of Clipper2.<br>

Example:

	Path64 badPath = MakePath(new int[] { 270, 230160, 230160, 160270, 160270, 2010, 2010, 7060, 7060, 14010, 14010, 20270, 20 })); 

<br><br>
<b>Examples of using library functions:</b>
<br><br>
<b>Delphi:</b><br>
    
    uses Clipper;

    function MakePath(const ints: TArrayOfInteger): TPath64;
    var
      i, len: integer;
    begin
      len := length(ints) div 2;
      SetLength(Result, len);
      for i := 0 to len -1 do
      begin
        Result[i].X := ints[i*2];
        Result[i].Y := ints[i*2 +1];
      end;
    end;

    //code main entry 
    var
      subject, clip, solution: TPaths64;      
    begin
      SetLength(subject, 1);
      subject[0] := MakePath([100, 50, 10, 79, 65, 2, 65, 98, 10, 21]);
      solution := Union(sub, nil, frEvenOdd);      
      
![test](https://user-images.githubusercontent.com/5280692/159098614-bf8dfd82-5c5b-42a4-ae93-d9a5a1b7dd14.png)
      
      solution := Union(sub, nil, frNonZero);      
      
![test](https://user-images.githubusercontent.com/5280692/159098650-9923ffe9-a5fb-49a4-9d34-9ef1e5aae8e4.png)

      SetLength(subject, 1);
      subject[0] := MakePath([100, 50, 10, 79, 65, 2, 65, 98, 10, 21]);
      setLength(clip, 1);
      clip[0] := MakePath([80, 50, 69, 73, 43, 79, 23, 63, 23, 37, 43, 21, 69, 27]);
      solution := Union(sub, clp, Clipper.TFillRule.frNonZero);
      
![test](https://user-images.githubusercontent.com/5280692/159098431-378a4a85-be83-4412-a6f0-09e88234928f.png)
      
      solution := Intersect(sub, clp, Clipper.TFillRule.frNonZero);
      
![test](https://user-images.githubusercontent.com/5280692/159098290-85c67eec-04a2-4ea7-9b5c-b2120d3cf5bd.png)
      
    end;

