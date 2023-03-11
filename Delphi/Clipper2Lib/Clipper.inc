
///////////////////////////////////////////////////////////////////////////////
//USER DEFINED PREPROCESSOR DIRECTIVES
///////////////////////////////////////////////////////////////////////////////

//For user defined Z-coordinates, defined in Clipper's 'SetZ' callback event
{.$DEFINE USINGZ}

///////////////////////////////////////////////////////////////////////////////
//COMPILER DIFINED PREPROCESSOR DIRECTIVES (ie. do not touch ;))
///////////////////////////////////////////////////////////////////////////////

{$IFDEF FPC}
  {$DEFINE INLINING}
  {$MODE DELPHI}
{$ELSE}
  {$IF CompilerVersion < 14}
    Requires Delphi version 6 or above.
  {$IFEND}
  {$IF CompilerVersion >= 18}             //Delphi 2007
    {$DEFINE RECORD_METHODS}              
    {$DEFINE STRICT}
    {$IF CompilerVersion >= 19}           //Delphi 2009  
      //While "inlining" is supported from D2005, it's buggy (see QC41166) until D2009
      {$DEFINE INLINING}
    {$IFEND}
    {$IF COMPILERVERSION >= 23}           //Delphi XE2+
      {$DEFINE XPLAT_GENERICS}
      {$DEFINE ROUNDINGMODE}   
      {$IF COMPILERVERSION >= 24}         //Delphi XE3+
        {$LEGACYIFEND ON}
      {$IFEND}
    {$IFEND}
  {$IFEND}
{$ENDIF}

{$IFDEF DEBUG}
  {$UNDEF INLINING}
{$ENDIF}
