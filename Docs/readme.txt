
This is a pre-release beta version of Clipper2. While the code in Clipper1 
(i.e. versions 1 through to 6.4.2) was functional, in many places it is/was 
downright ugly. This new version is a significant rewrite that should be 
**a little** easier to understand, hough unfortunately still very complex. 
There's also a modest performance improvement, and there's much simpler and
more efficient handling of spikes and micro-self-intersections with their 
almost complete removal from clipping solutions.

However, because this is a pre-release version ... EXPECT SOME BUGS, even though
both the Delphi code and its C# port have been fairly extensively tested. 
A C++ port is also in the pipeline.

There are too many changes to Clipper's interfaces to mention 
them all here. These are the more important ones:
1. The PolyFillType enumeration has been renamed FillRule.
2. The cInt type used for path coordinates has been replaced with native  
   64bit integer types (long, Int64 or int64_t).
3. The IntPoint and IntRect types have also been renamed Point64 and Rect64
   respectively. There is new support for floating point coordinates, with the
   new PointD and RectD classes indicating double float values.
4. The Clipper class no longer has AddPath and AddPaths methods. These have
   been replaced with AddSubject, AddOpenSubject and AddClip methods.
5. The Clipper class's Execute parameters have changed with the removal of 
   the second FillRule parameter, and addition of an optional OpenSolutions 
   parameter. (The second FillRule parameter was almost never needed and 
   probably confusing).
6. The Polytree class now only contains closed paths (ie polygons) since
   open paths can't contain polygons and open paths in solutions are now 
   returned via Execute's OpenSolutions parameter.
7. Clipper2's Offset behavior for open paths has changed compared to Clipper1. The
   offset value is now the full width of the resulting offset, rather than the per-side
   value. To get similar results to the Clipper1 behavior, double the offset value.
8. Collinear vertices are retained by default, in contrast to the default behavior of 
   Clipper1.
   
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
A "touching" segment (as opposed to a touching vertex) is one that is collinear with, and at least partially overlaps, another segment.

A clipping solution will likely NOT be in its simplest form. 
For example, solutions may have "touching" polygons.

Clipping open paths:

The Clipper library clips both closed paths (polygons) and open paths (polylines). However, the clipping path or paths must be closed. In other words polygons and polylines can't be clipped by polylines. 

Clipping is performed using a "sweep line" algorithm, running from lower-most vertex to top-most vertex.

If an open path segment (open segment) touches a clipping boundary, but does not cross (intersect) it, this is defined as a "touching segment".
The presence of a touching segment in the clipping solution will depend on which side of the boundary the path was before touching, evaluated using the sweep line method (bottom-to-top, as noted earlier):

- During sweeping, if an open segment prior to the touching segment lies outside the clipping region, the touching segment will *not* be par t of the clipping solution.
- If an open segment prior to the touching segment lies inside the clipping region, the touching segment will be part of the clipping solution.

If an open path segment that starts or ends when touching a boundary (terminating segment), its placement with regard to the boundary will depend on the heading of the connected segment immediately above (the adjacent segment):

- If the adjacent segment heads inside the clipping boundary, the terminating segment will be included in the clipping solution.


If an open path consists entirely of one or more touching segments, with no crossing of the clipping boundary, the placement of the segments is undefined.

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

This property only pertains to **closed paths**. Paths commonly have consecutive segments that are collinear, where the shared vertex can be removed (and paths simplified) without altering the shape of these paths. This simplification is commonly but not always preferred for clipping solutions. However, when consecutive segments are collinear AND ALSO change direction 180 degrees, causing spikes, these are rarely desired. 'Spikes' will always be removed from closed path solutions, irrespective of the PreserveCollinear property.
