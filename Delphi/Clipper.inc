
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
