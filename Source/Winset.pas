{== Winset ============================================================}
{: Collects routines to save and restore form state
@author Dr. Peter Below
@desc   Version 1.0 created 1997-07-03<BR>
        Version 1.01 created 2002-06-07, changed to use of TcustomInifile
        Version 1.02 created 2007-05-09, added option to save splitter
          positions and removed D1 compatibility.
        Last modified       2007-07-23<P>
   }
{======================================================================}
{$BOOLEVAL OFF} {Unit depends on shortcut boolean evaluation}
unit Winset;

interface

uses
  Inifiles, Forms;

procedure SaveWindowstate(ini: TCustomInifile; form: TForm;
  SaveSplitters: Boolean = false);
procedure RestoreWindowstate(ini: TCustomInifile; form: TForm;
    RestoreSplitters: Boolean = false);
procedure SaveWindowstateEx(ini: TCustomInifile; form: TForm; const
  section: string; SaveSplitters: Boolean = false);
procedure RestoreWindowstateEx(ini: TCustomInifile; form: TForm; const
    section: string; RestoreSplitters: Boolean = false);

implementation

uses
  TypInfo, WinTypes, WinProcs, Messages, Classes, ExtCtrls, Controls,
  Sysutils;

const
  sSettings = 'Settings';
  sLeft = 'Left';
  sTop = 'Top';
  sWidth = 'Width';
  sHeight = 'Height';
  sState = 'State';
  sSplitterPositions = 'SplitterPositions';

{: Create a string describing the splitter positions on the passed form.
 The generated string is composed of a comma-separated list of entries
 of the form controlname=Xnumber, where X is W or H to indicate whether
 the number following is the controls width or height, respectively.
 The control used is the one the splitter controls. We only examine
 splitters owned by the form!
 Note that this function is not compatible with Delphi versions before
 BDS 3.0, since the Align property was not available on the TControl
 class in older versions. One can work around this by using TypInfo
 methods to check whether Control has a published Align property and
 also to get its value. }
function ConstructSplitterString(form: TForm): string;

  function SplitterPosition(Splitter: TSplitter): string;
  var
    K: Integer;
    Parent: TWinControl;
    Control: TControl;
  begin
    Parent := Splitter.Parent;
    Result := '';
    for K := 0 to Parent.Controlcount - 1 do begin
      Control := Parent.Controls[K];
      if (Control <> Splitter) and (Control.Align = Splitter.Align)
      then begin
        case Splitter.Align of
          alTop, alBottom:
            Result := Format('%s=H%d',[Control.Name, Control.Height]);
          alLeft, alRight:
            Result := Format('%s=W%d',[Control.Name, Control.Width]);
        else
          Assert(false, 'Unexpected align value for the splitter');    
        end; {case}
        Break;
      end; {if}
    end; {for}
  end;

var
  SL: TStringList;
  I: Integer;
  Comp: TComponent;
begin
  SL := TStringList.Create();
  try
    for I := 0 to form.ComponentCount - 1 do begin
      Comp := form.Components[I];
      if Comp is TSplitter then
        SL.Add(Splitterposition(TSplitter(Comp)));
    end; {for}
    Result := SL.Commatext;
  finally
    SL.Free;
  end; {finally}
end;

{: Set the positions of splitters on form from the data contained in
 S. S is supposed to have been created by ConstructSplitterString. }
procedure SetSplitters(form: TForm; const S: string);
var
  SL: TStringList;
  I, N: Integer;
  Comp: TComponent;
  Value: string;
begin
  if S <> '' then begin
    SL := TStringList.Create();
    try
      Sl.Commatext := S;
      for I := 0 to SL.Count - 1 do begin
        Comp := form.FindComponent(SL.Names[I]);
        if Assigned(Comp) and (Comp Is TControl) then begin
          Value := SL.ValueFromIndex[I];
          if (Length(Value) > 1)
            and (Value[1] In ['H','W'])
            and TryStrToInt(Copy(Value, 2, Maxint), N)
          then
            case Value[1] of
              'H': TControl(Comp).Height := N;
              'W': TControl(Comp).Width := N;
            end; {case}
        end; {if}
      end; {for}
    finally
      SL.Free;
    end; {finally}
  end; {if}
end;

{-- SaveWindowstateEx -------------------------------------------------}
{: Saves the windows position, size, and (optionally) splitter positions
   to an INI file or equivalent.
@Param ini is the inifile to save the settings in.
@Param form is the form to save the settings for.
@Param section is the section to use for the settings.
@Param SaveSplitters determines whether to save the positions for all
  splitters on the form as well.
@Precondition  ini and form not nil
}{ Created 1997-07-03, modified 2007-05-09 by P. Below
-----------------------------------------------------------------------}
procedure SaveWindowstateEx(ini: TCustomInifile; form: TForm; const
  section: string; SaveSplitters: Boolean = false);
var
  wp: TWindowPlacement;
begin
  Assert(Assigned(ini));
  Assert(Assigned(form));
  wp.length := Sizeof(wp);
  GetWindowPlacement(form.handle, @wp);
  with Ini, wp.rcNormalPosition do begin
    WriteInteger(section, sLeft, Left);
    WriteInteger(section, sTop, Top);
    WriteInteger(section, sWidth, Right - Left);
    WriteInteger(section, sHeight, Bottom - Top);
    WriteString(section, sState,
      GetEnumName(TypeInfo(TwindowState), Ord(form.WindowState)));
  end; { With }
  if SaveSplitters then
    Ini.WriteString(section, sSplitterPositions,
      ConstructSplitterString(form));
end; { SaveWindowStateEx }

{-- RestoreWindowstateEx ----------------------------------------------}
{: Restores the windows position, size, and (optionally) splitter positions
   from an INI file or equivalent.
@Param ini is the inifile to restore the settings from.
@Param form is the form to restore the settings for.
@Param section is the section to use for the settings.
@Param SaveSplitters determines whether to restore the positions for all
  splitters on the form as well.
@Precondition  ini and form not nil
}{ Created 1997-07-03, modified 2007-05-09, 2007-07-23 by P. Below
-----------------------------------------------------------------------}
procedure RestoreWindowstateEx(ini: TCustomInifile; form: TForm; const
    section: string; RestoreSplitters: Boolean = false);
var
  L, T, W, H: Integer;
  WS: TWindowstate;
begin
  Assert(Assigned(ini));
  Assert(Assigned(form));
  with Ini do begin
    L := ReadInteger(section, sLeft, form.Left);
    T := ReadInteger(section, sTop, form.Top);
    W := ReadInteger(section, sWidth, form.Width);
    H := ReadInteger(section, sHeight, form.Height);
    form.SetBounds(L, T, W, H);
    try
      WS :=
        TWindowState(GetEnumValue(TYpeinfo(TWindowState),
        ReadString(section, sState, 'wsNormal')));
      form.WindowState := WS;
//      case WS of
//        wsMaximized:
//          PostMessage(form.Handle, WM_SYSCOMMAND, SC_MAXIMIZE, 0);
//        wsMinimized:
//          PostMessage(form.Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
//      end; { case }
      {*****BUG ALERT! ****
       For some reason sending instead of posting the message to a
       form during the create sequence can lead to the form to come
       up disabled on the API level. Posting the message does not
       have the problem but it has another: if the forms handle is
       recreated before the message is received it will get lost.
       Setting the forms windowstate directly has also problems.}
    except
    end;
  end; { With }
  if RestoreSplitters then
    SetSplitters(form, Ini.ReadString(section, sSplitterPositions, ''));
end; { TEinsendeMain.RestoreWindowStateEx }

{ Save state using the default settings section }
procedure SaveWindowstate(ini: TCustomInifile; form: TForm;
  SaveSplitters: Boolean = false);
begin
  SaveWindowstateEx(ini, form, sSettings, SaveSplitters);
end; { SaveWindowState }

{ Restore state using the default settings section }
procedure RestoreWindowstate(ini: TCustomInifile; form: TForm;
    RestoreSplitters: Boolean = false);
begin
  RestoreWindowStateEx(ini, form, sSettings, RestoreSplitters);
end; { TEinsendeMain.RestoreWindowState }

end.
