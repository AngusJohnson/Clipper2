
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
   been replaced with AddSubject and AddClip methods.
4. The Clipper class's Execute parameters have also changed with the addition 
   of an optional OpenSolutions parameter, and with the removal of one of 
   two FillRule parameters. (Two FillRule parameters was unnecessary given 
   how rarely users would need different fill rules for Subjects and Clips.)
5. The Polytree class will now only contain closed paths (ie polygons) given 
   that open paths can't contain polygons. Any open paths in solutions will 
   now be returned via the OpenSolutions parameter in Clipper.Execute().
   
When I originally translated this Library from Delphi (Pascal) to C# and C++,
I deliberately kept a strong Delphi naming style as I thought this would help
with maintenance. In hindsight this was a mistake, and just made the C# and 
C++ code look odd. With this new version, I've attempted to adopt a more 
conventional naming style for each language, while admitting that I still 
have very limited coding experience in both these languages.
