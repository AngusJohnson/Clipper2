
///////////////////////////////////////////////////////////////////////////////
//USER DEFINED PREPROCESSOR DIRECTIVES
///////////////////////////////////////////////////////////////////////////////

//For user defined Z-coordinates, defined in Clipper's 'SetZ' callback event
{.$DEFINE USINGZ}

//Winding direction is based on the Cartesian Plane where the display origin 
//is in the bottom-left corner. For displays with inverted Y-axes, the winding 
//direction will be reversed such that clockwise becomes anti-clockwise and 
//positive winding will become negative. This is important when calculating 
//polygon areas, since polygons with vertices progressing in a clockwise 
//direction are usually considered to have positive areas, and polygons with 
//vertices progressing in a counter-clockwise direction are usually considered 
//to have negative areas. winding direction will usually only be of particular 
//concern to the library user if either FillRule.Positive or FillRule.Negative 
//is used, but their use is uncommon.
{$DEFINE INVERTEDYAXIS}

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
