unit Clipper.DLL.Enums;

(* ******************************************************************************
 * Author    :  Jasper Schellingerhout                                         *
 * Date      :  12 June 2024                                                   *
 * License   :  http://www.boost.org/LICENSE_1_0.txt                           *
 **************************************************************************** *)

{$I Clipper.DLL.inc}   // CLIPPER_USINGZ

interface
type

{$SCOPEDENUMS ON}
{$Z1} 
  TFillRule = ( EvenOdd, NonZero, Positive, Negative );
	TClipType = ( None, Intersection, Union, Difference, Exclusive {XOR} );

  TJoinType = ( Square, Bevel, Round, Miter );
//Square : Joins are 'squared' at exactly the offset distance (more complex code)
//Bevel  : Similar to Square, but the offset distance varies with angle (simple code & faster)

  TEndType = (Polygon, Joined, Butt, Square, Round);
//Butt   : offsets both sides of a path, with square blunt ends
//Square : offsets both sides of a path, with square extended ends
//Round  : offsets both sides of a path, with round extended ends
//Joined : offsets both sides of a path, with joined ends
//Polygon: offsets only one side of a closed path


implementation

end.
