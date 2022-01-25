{== KeytraceTypesU ====================================================}
{! <summary>
This unit defines some common types and constants for the project.
 </summary>
<author>Dr. Peter Below</author>
<history>
<para>Version 1.0 created 2008-08-01</para>
<para>Last modified       2008-08-01</para>
</history>
<remarks>
</remarks>}
{======================================================================}
{$BOOLEVAL OFF}{Unit depends on shortcut boolean evaluation}
unit KeytraceTypesU;

interface

type
  TLogFeature = (lfPreProcessMessage, lfHintMessage, lfWindowproc,
    lfFromIsShortcut, lfFormOnShortcut, lfAppOnShortcut, lfCMChildkey,
    lfCMDialogKey, lfOnKeyDown, lfCMDialogChar, lfOnKeyPress, lfOnKeyUp,
    lfCMWantSpecialKey);
  TLogFeatures = set of TLogFeature;

const
  CLogMenuCaptions : array [TLogFeature] of string =
    ('Preprocess messages', {lfPreProcessMessage}
     'Hint messages',       {lfHintMessage}
     'Edit1 Windowproc',    {lfWindowProc}
     'Forms IsShortcut override', {lfFromIsShortcut}
     'Forms OnShortcut event', {lfFormOnShortcut}
     'Application OnShortcut event', {lfAppOnShortcut}
     'CM_CHILDKEY message', {lfCMChildkey}
     'CM_DIALOGKEY message', {lfCMDialogkey}
     'OnKeyDown event', {lfOnKeyDown}
     'CM_DIALOGCHAR message', {lfCMDialogChar}
     'OnKeyPress event', {lfOnKeyPress}
     'OnKeyUp event', {lfOnKeyDown}
     'CM_WANTSPECIALKEY message' {lfCMWantSpecialKey}
     );


implementation

end.
