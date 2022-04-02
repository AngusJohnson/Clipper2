
///////////////////////////////////////////////////////////////////////////////
//USER DEFINED PREPROCESSOR DIRECTIVES
///////////////////////////////////////////////////////////////////////////////

//For user defined Z-coordinates, defined in Clipper's 'SetZ' callback event
{.$DEFINE USINGZ}

//Winding direction is based on the Cartesian Plane where the display origin 
//is in the bottom-left corner. For displays with inverted Y-axes, the winding 
//direction will be reversed such that clockwise becomes anti-clockwise and 
//positive winding will become negative. This is important when using either
//FillRule.Positive or FillRule.Negative, and also when calculating areas
//(which may be positive or negative depending on orientation). When 
//developing graphics related software using displays with inverted Y-axes,
//coordinate orientation is reversed which may cause confusion. This can be
//addressed by reversing the normal orientation.
{.$DEFINE REVERSE_ORIENTATION}

///////////////////////////////////////////////////////////////////////////////
//COMPILER DIFINED PREPROCESSOR DIRECTIVES (ie. do not touch ;))
///////////////////////////////////////////////////////////////////////////////

{$IFDEF FPC}
  {$DEFINE INLINING}
  {$MODE DELPHI}
{$ELSE}
  {$IF CompilerVersion >= 18}           //Delphi 2007
    {$DEFINE RECORD_METHODS}            //Delphi 2006 - added records with methods
    //While Inlining has been supported since D2005, both D2005 and D2006
    //have an Inline codegen bug (QC41166) so ignore Inline until D2007.
    {$DEFINE INLINING}
    {$DEFINE STRICT}
    {$IF COMPILERVERSION >= 23}         //Delphi XE2+
      {$DEFINE XPLAT_GENERICS}
      {$IF CompilerVersion >= 25.0}     //Delphi XE4+
        {$LEGACYIFEND ON}
      {$IFEND}
    {$IFEND}
  {$IFEND}
  {$IF CompilerVersion < 14}
    Requires Delphi version 6 or above.
  {$IFEND}
{$ENDIF}

{$IFDEF DEBUG}
  {$UNDEF INLINING}
{$ENDIF}
