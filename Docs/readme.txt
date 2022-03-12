
This is a pre-release beta version of Clipper2. While the code in Clipper1 
(ie versions 1 through to 6.4.2) was functional, in many places it is/was 
downright ugly. This new version is a significant rewrite that should be 
**a little** easier to understand (though unfortunately still very complex). 
There's also a modest performance improvement. And there's much simpler and
more efficient handling of spikes and micro-self-intersections with their 
almost complete removal from clipping solutions.

But since this is a pre-release version ... EXPECT SOME BUGS, even though
both the Delphi code and its C# port have been fairly extensively tested. 
A C++ port is also in the pipeline.

There are many changes to Clipper's interfacem and too numerous to mention 
them all here. Nevertheless, these are the more important ones:
1. The PolyFillType enumeration has been renamed FillRule.
2. The cInt type used for path coordinates has been replaced with native  
   64bit integer types (long, Int64 or int64_t).
3. The IntPoint and IntRect types have also been renamed Point64 and Rect64
   respectively. And with new support for floating point coordinates, there
   are new PointD and RectD classes indicating double float values.
4. The Clipper class no longer has AddPath and AddPaths methods. These have
   been replaced with AddSubject, AddOpenSubject and AddClip methods.
4. The Clipper class's Execute parameters have changed with the removal of 
   the second FillRule parameter, and addition of an optional OpenSolutions 
   parameter. (The second FillRule parameter was almost never needed and 
   probably confusing).
5. The Polytree class now only contains closed paths (ie polygons) since
   open paths can't contain polygons and open paths in solutions are now 
   returned via Execute's OpenSolutions parameter.
   
When I originally translated this Library from Delphi (Pascal) to C# and C++,
I deliberately kept a strong Delphi naming style as I thought this would help
with maintenance. In hindsight this was a mistake, and just made the C# and 
C++ code look odd. With this new version, I've attempted to adopt a more 
conventional naming style for each language, while admitting that I still 
have very limited coding experience in both these languages.

Additional notes:

	using Path64 = List<Point64>;
	using Paths64 = List<List<Point64>>;
	using PathD = List<PointD>;
	using PathsD = List<List<PointD>>;

  //when using integer coordinates
  Clipper clipper = new Clipper();       
  clipper.AddSubject(subjects);          
  clipper.AddClip(clips);                
  clipper.Execute(ClipType.Intersection, FillRule.EvenOdd, closedSolution);

  //when using floating point (double) coordinates
  ClipperD clipper = new ClipperD();       

Documentation definition: 
A "touching" segment (as opposed to a touching vertex) is one that is
collinear with and at least partially overlaps another segment.

A clipping solution will likely NOT be in its simplest form. 
For example, solutions may have "touching" polygons.

Clipping open paths:

The Clipper library clips both closed paths (polygons) and open paths (polylines). However, the clipping path or paths must be closed. In other words polygons and polylines can't be clipped by polylines. 

When an open path segment touches a clipping boundary (without crossing it), it may or may not be part of the clipping solution, and will depend on which side of the boundary the open path was before touching. However, all clipping is performed using a "sweep line" algorithm that passes from the lower-most vertex to the top-most vertex. This means that open paths won't be processed from start to end as one might expect, but bottom to top. Hence, whether an open path is inside or outside a clipping boundary prior to touching will be determined in this manner too. Touching segments don't intersect. So, while sweeping from bottom to top, when an open segment prior to touching a clipping segment was outside the clipping region, then that touching segment will **not** be part of the clipping solution. Likewise when an open segment that prior to touching was inside a clipping region, it **will** be part of the clipping solution. For open path segments that start (or end) "touching" a boundary, their placement (whether inside or outside that boundary) will depend on the heading of their adjacent segment (that's immediately above). They will be placed inside when their adjacent segment heads inside the clipping region. Finally, for segments that touch a clipping boundary and have no adjacent segment, their placement (whether inside or outside that clipping boundary) is undefined.

Examples:
  subj_open = new Paths64();
  clip = new Paths64();

  subj_open.Add(MakePath(new int[] { 20,40, 20,30, 10,30 }));      //will not be part of the solution
  clip.Add(MakePath(new int[] { 20,20, 40,20, 40,40, 20,40 }));

  subj_open.Add(MakePath(new int[] { 45,40, 45,30, 55,30 }));      //will be part of the solution
  clip.Add(MakePath(new int[] { 45,20, 65,20, 65,40, 45,40 }));

  subj_open.Add(MakePath(new int[] { 70,35, 70,25 }));             //undefined
  clip.Add(MakePath(new int[] { 70,20, 90,20, 90,40, 70,40 }));

PreserveCollinear property

This property only pertains to **closed paths**. Paths not uncommonly have consecutive segments that are collinear, where the shared vertex can be removed (and paths simplified) without altering the shape of these paths. This simplification is commonly but not always preferred for clipping solutions. However, when consecutive segments are collinear AND ALSO change direction 180 degrees, causing spikes, these are rarely desired. 'Spikes' will always be removed from closed path solutions, irrespective of the PreserveCollinear property.
