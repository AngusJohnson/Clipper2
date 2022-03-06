
This is a pre-release version of Clipper2. While the code in Clipper1 
(ie versions 1 through to 6.4.2) was functional, in many places it is/was 
downright ugly. This new version is a significant rewrite that should be 
**a little** easier to understand (though unfortunately still very complex). 
There's a modest performance improvement, and much better (simpler, more 
efficient and almost complete) removal of spikes and micro-self-intersections 
from clipping solutions.

This is a pre-release version ... SO EXPECT SOME BUGS. The Delphi code
has been fairly extensively tested for polygons, but less so for open paths.
The C# port has not been as extensively tested as the Delphi code. 
A C++ port is also in the pipeline.

There are many changes to Clipper's interface. While too numerous to mention 
here, these are some notable ones:
1. The PolyFillType enumeration has been renamed FillRule.
2. The cInt type used for path coordinates has been replaced with native  
   64bit integer types (long, Int64 or int64_t).
3. The IntPoint and IntRect types have also been renamed Point64 and Rect64
   respectively. And with new support for floating point coordinates, there
   are new PointD and RectD classes indicating double float values.
4. The Clipper class no longer has AddPath and AddPaths methods. These have
   been replaced with AddSubject, AddOpenSubject and AddClip methods.
4. The Clipper class's Execute parameters have also changed with the addition 
   of an optional OpenSolutions parameter. The second FillRule parameters has
   also been removed.
5. The Polytree class now only contains closed paths (ie polygons) since
   open paths can't contain polygons. Open paths in solutions are now 
   returned via Clipper.Execute's new OpenSolutions parameter.
   
When I originally translated this Library from Delphi (Pascal) to C# and C++,
I deliberately kept a strong Delphi naming style as I thought this would help
with maintenance. In hindsight this was a mistake, and just made the C# and 
C++ code look odd. With this new version, I've attempted to adopt a more 
conventional naming style for each language, while admitting that I still 
have very limited coding experience in both these languages.

Additional notes:

  //notes: 1: IntPoint has been replaced with Point64 (and PointD)
  //       2. IntPath and IntPaths have been replaced with Path64 and  Paths64
  //       3. Two new structures have been added - PathD and PathsD which 
  //          define paths using double floating point coordinates.
	using Path64 = List<Point64>;
	using Paths64 = List<List<Point64>>;
	using PathD = List<PointD>;
	using PathsD = List<List<PointD>>;

  Clipper clipper = new Clipper();       
  //note: The AddPath/AddPaths methods have been replaced with the following:
  clipper.AddSubject(subjects);          
  clipper.AddOpenSubject(openSubjects);  
  clipper.AddClip(clips);                
  //note: solutions with open paths have them returned via Execute's optional 4th parameter
  clipper.Execute(ClipType.Intersection, FillRule.EvenOdd, closedSolution, openSolution);

  //note: added a new clipping class that's a descendant of Clipper and 
  //      accommodates PathD and PathsD structures.
  Clipper clipperD = new ClipperD();   

Documatation definition: 
"Touching" path segments (as opposed to touching vertices) are segments that 
are collinear with and at least partially overlap other path segments.

Clipping solutions are commonly not in their simplest forms, and so solutions may have "touching" polygons.

Open subject paths may have segments that touch clipping segments. These touching segments may or may not be part of the clipping solution. Touching segments won't cross (ie intersect with) other paths. So paths that start outside a clipping region and have segments touching a clipping boundary, those segments will not be part of the solution. Likewise open paths that start inside a clipping region and touch a clipping boundary, those touching segments will be part of the clipping solution. For open path line segments that start on a clipping boundary and are "touching" that boundary, their segment placement will depend on the heading of the next segment.

Examples:
  subj_open = new Paths64();
  clip = new Paths64();

  subj_open.Add(MakePath(new int[] { 20,40, 20,30, 10,30 }));      //will not be part of the solution
  clip.Add(MakePath(new int[] { 20,20, 40,20, 40,40, 20,40 }));

  subj_open.Add(MakePath(new int[] { 45,40, 45,30, 55,30 }));      //will be part of the solution
  clip.Add(MakePath(new int[] { 45,20, 65,20, 65,40, 45,40 }));

  subj_open.Add(MakePath(new int[] { 70,35, 70,25 }));             //undefined
  clip.Add(MakePath(new int[] { 70,20, 90,20, 90,40, 70,40 }));

