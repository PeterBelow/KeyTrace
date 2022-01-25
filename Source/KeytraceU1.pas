{== KeytraceU1 ========================================================}
{: This unit contains the programs main form.
@author Dr. Peter Below
@desc   Version 1.0 created 2007-12-26<BR>
        Last modified       2007-12-26<P>
The forms main purpose is to show what messages are created by
pressing a key and how they flow through the VCLs code.   }
{======================================================================}
{$BOOLEVAL OFF}{Unit depends on shortcut boolean evaluation}
unit KeytraceU1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Forms,  Dialogs, StdCtrls, ActnList, Menus, Buttons, ExtCtrls,
  KeytraceTypesU;

type
  {: Interposer class to replace the standard TEdit class. This way
    we can override methods and add message handlers to this class
    without having to create and install a true TEdit descendant.}
  TEdit = class(StdCtrls.TEdit)
  protected
    procedure CMWantspecialKey(var Message: TCMWantSpecialKey); message
      CM_WANTSPECIALKEY;
  public
    function PreProcessMessage(var Msg: TMsg): Boolean; override;
  end;

  {: Interposer class to replace the standard TButton class. This way
    we can override methods and add message handlers to this class
    without having to create and install a true TButton descendant.}
  TButton = class(StdCtrls.TButton)
  protected
    procedure CMDialogChar(var Message: TCMDialogChar);
      message CM_DIALOGCHAR;
    procedure CMWantspecialKey(var Message: TCMWantSpecialKey); message
      CM_WANTSPECIALKEY;
  public
    function PreProcessMessage(var Msg: TMsg): Boolean; override;
  end;

  TKeytraceMain = class(TForm)
    Logmemo: TMemo;
    Edit1: TEdit;
    Closebutton: TButton;
    MainMenu: TMainMenu;
    Filemenu: TMenuItem;
    CloseMenu: TMenuItem;
    ActionList: TActionList;
    CloseAction: TAction;
    ClearFocusButton: TSpeedButton;
    DisableButton: TSpeedButton;
    ReenableTimer: TTimer;
    GrapCaptureButton: TSpeedButton;
    LogMenu: TMenuItem;
    LogClearMenu: TMenuItem;
    N1: TMenuItem;
    LogSettingsMenu: TMenuItem;
    procedure DisableButtonClick(Sender: TObject);
    procedure ReenableTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ClearFocusButtonClick(Sender: TObject);
    procedure CloseActionClick(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; Shift:
      TShiftState);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure Edit1KeyUp(Sender: TObject; var Key: Word; Shift:
      TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift:
      TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure GrapCaptureButtonClick(Sender: TObject);
    procedure LogClearMenuClick(Sender: TObject);
    procedure LogSettingsMenuClick(Sender: TObject);
  private
    FLogFeatures: TLogFeatures;
    FOldEdit1WndProc: TWndMethod;
    procedure AppShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure BuildLogMenu;
    procedure DisplayOnKeyDown(const Name: string; Key: Word; Shift:
      TShiftState);
    procedure DisplayOnKeyPress(const Name: string; Key: Char);
    procedure DisplayOnKeyUp(const Name: string; Key: Word; Shift:
      TShiftState);
    procedure DisplayWMKey(const Procname: string; const Message: TWMKey);
    procedure ExecuteLogAction(Sender: TObject);
    function Flags(Keydata: longint): string;
    function IsMouseMsg(const Msg: TMessage): Boolean;
    function KeyName(Keydata: longint): string;
    procedure NewEdit1WndProc(var Msg: TMessage);
    procedure PrepareHintMessageLogging;
    function Receivername(wnd: HWnd): string;
    function RepeatCount(Keydata: longint): Integer;
    procedure RestoreFormState;
    procedure SaveFormstate;
    function Scancode(Keydata: longint): Integer;
    procedure SubClassEdit1;
    procedure UpdateActionCheckstate;
    procedure UpdateLogAction(Sender: TObject);
  protected
    procedure AppMessage(var Msg: TMsg; var Handled: Boolean);
    procedure CMChildkey(var Message: TCMChildkey); message CM_CHILDKEY;
    procedure CMDialogKey(var Message: TWMKey); message CM_DIALOGKEY;
    procedure DisplayKeyMenuMsg(const  Msg: TMsg);
    procedure DisplayKeyMsg(const Msg: TMsg);
    procedure CMDialogChar(var Message: TCMDialogChar); message
      CM_DIALOGCHAR;
    property LogFeatures: TLogFeatures read FLogFeatures;
  public
    destructor Destroy; override;
    procedure Display(const S: String); overload;
    procedure Display(const Fmt: string; const A: array of const); overload;
    procedure DisplayCMDialogChar(const Sender: TComponent; const Msg:
      TCMDialogChar);
    procedure DisplayCMWantSpecialKey(const aName: string; const Msg:
      TCMWantSpecialKey);
    function PreProcessMessage(var Msg: TMsg): Boolean; override;
    function IsShortCut(var Message: TWMKey): Boolean; override;
  end;

var
  KeytraceMain: TKeytraceMain;

implementation

uses Math, KeytraceMemoryU, KTLogFeaturesDialogU, TypInfo;

const
  CLogCategory = 'Log';

{$R *.dfm}

type
  {: Custom hint window class used to log calls to IsHintMsg. The main
    forms onCreate event handler defines this class as the global hint
    window to use. }
  TLogHintWindow = class(THintWindow)
  public
    function IsHintMsg(var Msg: TMsg): Boolean; override;
  end;

{: Convert a message constant to a display string. We only expect
  key and mouse messages here, anything else will just return
  the message constants value as a hexadecimal representation.}
function MessageToText(aMsg: Cardinal): string;
const
  WM_XBUTTONDOWN  = $020B;
  WM_XBUTTONUP    = $020C;
  WM_XBUTTONDBLCLK= $020D;
begin
  case aMsg of
    WM_COMMAND              : Result := 'WM_COMMAND';
    WM_SYSCOMMAND           : Result := 'WM_SYSCOMMAND';
    WM_KEYDOWN              : Result := 'WM_KEYDOWN';
    WM_SYSKEYDOWN           : Result := 'WM_SYSKEYDOWN';
    WM_CHAR                 : Result := 'WM_CHAR';
    WM_SYSCHAR              : Result := 'WM_SYSCHAR';
    WM_KEYUP                : Result := 'WM_KEYUP';
    WM_SYSKEYUP             : Result := 'WM_SYSKEYUP';
    WM_SYSDEADCHAR          : Result := 'WM_SYSDEADCHAR';
    WM_DEADCHAR             : Result := 'WM_DEADCHAR';
    WM_UNICHAR              : Result := 'WM_UNICHAR';
    WM_NCMOUSEMOVE          : Result := 'WM_NCMOUSEMOVE';
    WM_NCLBUTTONDOWN        : Result := 'WM_NCLBUTTONDOWN';
    WM_NCLBUTTONUP          : Result := 'WM_NCLBUTTONUP';
    WM_NCLBUTTONDBLCLK      : Result := 'WM_NCLBUTTONDBLCLK';
    WM_NCRBUTTONDOWN        : Result := 'WM_NCRBUTTONDOWN';
    WM_NCRBUTTONUP          : Result := 'WM_NCRBUTTONUP';
    WM_NCRBUTTONDBLCLK      : Result := 'WM_NCRBUTTONDBLCLK';
    WM_NCMBUTTONDOWN        : Result := 'WM_NCMBUTTONDOWN';
    WM_NCMBUTTONUP          : Result := 'WM_NCMBUTTONUP';
    WM_NCMBUTTONDBLCLK      : Result := 'WM_NCMBUTTONDBLCLK';
    WM_NCXBUTTONDOWN        : Result := 'WM_NCXBUTTONDOWN';
    WM_NCXBUTTONUP          : Result := 'WM_NCXBUTTONUP';
    WM_NCXBUTTONDBLCLK      : Result := 'WM_NCXBUTTONDBLCLK';
    WM_MOUSEMOVE            : Result := 'WM_MOUSEMOVE';
    WM_LBUTTONDOWN          : Result := 'WM_LBUTTONDOWN';
    WM_LBUTTONUP            : Result := 'WM_LBUTTONUP';
    WM_LBUTTONDBLCLK        : Result := 'WM_LBUTTONDBLCLK';
    WM_RBUTTONDOWN          : Result := 'WM_RBUTTONDOWN';
    WM_RBUTTONUP            : Result := 'WM_RBUTTONUP';
    WM_RBUTTONDBLCLK        : Result := 'WM_RBUTTONDBLCLK';
    WM_MBUTTONDOWN          : Result := 'WM_MBUTTONDOWN';
    WM_MBUTTONUP            : Result := 'WM_MBUTTONUP';
    WM_MBUTTONDBLCLK        : Result := 'WM_MBUTTONDBLCLK';
    WM_MOUSEWHEEL           : Result := 'WM_MOUSEWHEEL';
    WM_XBUTTONDOWN          : Result := 'WM_XBUTTONDOWN';
    WM_XBUTTONUP            : Result := 'WM_XBUTTONUP';
    WM_XBUTTONDBLCLK        : Result := 'WM_XBUTTONDBLCLK';
    WM_MOUSEHOVER           : Result := 'WM_MOUSEHOVER';
    WM_MOUSELEAVE           : Result := 'WM_MOUSELEAVE';
    WM_NCMOUSEHOVER         : Result := 'WM_NCMOUSEHOVER';
    WM_NCMOUSELEAVE         : Result := 'WM_NCMOUSELEAVE';
    WM_TIMER                : Result := 'WM_TIMER';
    WM_GETDLGCODE           : Result := 'WM_GETDLGCODE';
    CM_ACTIVATE             : Result := 'CM_ACTIVATE';
    CM_DEACTIVATE           : Result := 'CM_DEACTIVATE';
    CM_GOTFOCUS             : Result := 'CM_GOTFOCUS';
    CM_LOSTFOCUS            : Result := 'CM_LOSTFOCUS';
    CM_CANCELMODE           : Result := 'CM_CANCELMODE';
    CM_DIALOGKEY            : Result := 'CM_DIALOGKEY';
    CM_DIALOGCHAR           : Result := 'CM_DIALOGCHAR';
    CM_FOCUSCHANGED         : Result := 'CM_FOCUSCHANGED';
    CM_PARENTFONTCHANGED    : Result := 'CM_PARENTFONTCHANGED';
    CM_PARENTCOLORCHANGED   : Result := 'CM_PARENTCOLORCHANGED';
    CM_HITTEST              : Result := 'CM_HITTEST';
    CM_VISIBLECHANGED       : Result := 'CM_VISIBLECHANGED';
    CM_ENABLEDCHANGED       : Result := 'CM_ENABLEDCHANGED';
    CM_COLORCHANGED         : Result := 'CM_COLORCHANGED';
    CM_FONTCHANGED          : Result := 'CM_FONTCHANGED';
    CM_CURSORCHANGED        : Result := 'CM_CURSORCHANGED';
    CM_CTL3DCHANGED         : Result := 'CM_CTL3DCHANGED';
    CM_PARENTCTL3DCHANGED   : Result := 'CM_PARENTCTL3DCHANGED';
    CM_TEXTCHANGED          : Result := 'CM_TEXTCHANGED';
    CM_MOUSEENTER           : Result := 'CM_MOUSEENTER';
    CM_MOUSELEAVE           : Result := 'CM_MOUSELEAVE';
    CM_MENUCHANGED          : Result := 'CM_MENUCHANGED';
    CM_APPKEYDOWN           : Result := 'CM_APPKEYDOWN';
    CM_APPSYSCOMMAND        : Result := 'CM_APPSYSCOMMAND';
    CM_BUTTONPRESSED        : Result := 'CM_BUTTONPRESSED';
    CM_SHOWINGCHANGED       : Result := 'CM_SHOWINGCHANGED';
    CM_ENTER                : Result := 'CM_ENTER';
    CM_EXIT                 : Result := 'CM_EXIT';
    CM_DESIGNHITTEST        : Result := 'CM_DESIGNHITTEST';
    CM_ICONCHANGED          : Result := 'CM_ICONCHANGED';
    CM_WANTSPECIALKEY       : Result := 'CM_WANTSPECIALKEY';
    CM_INVOKEHELP           : Result := 'CM_INVOKEHELP';
    CM_WINDOWHOOK           : Result := 'CM_WINDOWHOOK';
    CM_RELEASE              : Result := 'CM_RELEASE';
    CM_SHOWHINTCHANGED      : Result := 'CM_SHOWHINTCHANGED';
    CM_PARENTSHOWHINTCHANGED: Result := 'CM_PARENTSHOWHINTCHANGED';
    CM_SYSCOLORCHANGE       : Result := 'CM_SYSCOLORCHANGE';
    CM_WININICHANGE         : Result := 'CM_WININICHANGE';
    CM_FONTCHANGE           : Result := 'CM_FONTCHANGE';
    CM_TIMECHANGE           : Result := 'CM_TIMECHANGE';
    CM_TABSTOPCHANGED       : Result := 'CM_TABSTOPCHANGED';
    CM_UIACTIVATE           : Result := 'CM_UIACTIVATE';
    CM_UIDEACTIVATE         : Result := 'CM_UIDEACTIVATE';
    CM_DOCWINDOWACTIVATE    : Result := 'CM_DOCWINDOWACTIVATE';
    CM_CONTROLLISTCHANGE    : Result := 'CM_CONTROLLISTCHANGE';
    CM_GETDATALINK          : Result := 'CM_GETDATALINK';
    CM_CHILDKEY             : Result := 'CM_CHILDKEY';
    CM_DRAG                 : Result := 'CM_DRAG';
    CM_HINTSHOW             : Result := 'CM_HINTSHOW';
    CM_DIALOGHANDLE         : Result := 'CM_DIALOGHANDLE';
    CM_ISTOOLCONTROL        : Result := 'CM_ISTOOLCONTROL';
    CM_RECREATEWND          : Result := 'CM_RECREATEWND';
    CM_INVALIDATE           : Result := 'CM_INVALIDATE';
    CM_SYSFONTCHANGED       : Result := 'CM_SYSFONTCHANGED';
    CM_CONTROLCHANGE        : Result := 'CM_CONTROLCHANGE';
    CM_CHANGED              : Result := 'CM_CHANGED';
    CM_DOCKCLIENT           : Result := 'CM_DOCKCLIENT';
    CM_UNDOCKCLIENT         : Result := 'CM_UNDOCKCLIENT';
    CM_FLOAT                : Result := 'CM_FLOAT';
    CM_BORDERCHANGED        : Result := 'CM_BORDERCHANGED';
    CM_BIDIMODECHANGED      : Result := 'CM_BIDIMODECHANGED';
    CM_PARENTBIDIMODECHANGED: Result := 'CM_PARENTBIDIMODECHANGED';
    CM_ALLCHILDRENFLIPPED   : Result := 'CM_ALLCHILDRENFLIPPED';
    CM_ACTIONUPDATE         : Result := 'CM_ACTIONUPDATE';
    CM_ACTIONEXECUTE        : Result := 'CM_ACTIONEXECUTE';
    CM_HINTSHOWPAUSE        : Result := 'CM_HINTSHOWPAUSE';
    CM_DOCKNOTIFICATION     : Result := 'CM_DOCKNOTIFICATION';
    CM_MOUSEWHEEL           : Result := 'CM_MOUSEWHEEL';
    CM_ISSHORTCUT           : Result := 'CM_ISSHORTCUT';
    CM_INVALIDATEDOCKHOST   : Result := 'CM_INVALIDATEDOCKHOST';
    CM_SETACTIVECONTROL     : Result := 'CM_SETACTIVECONTROL';
    CM_POPUPHWNDDESTROY     : Result := 'CM_POPUPHWNDDESTROY';
    CM_CREATEPOPUP          : Result := 'CM_CREATEPOPUP';
    CM_DESTROYHANDLE        : Result := 'CM_DESTROYHANDLE';
    CM_MOUSEACTIVATE        : Result := 'CM_MOUSEACTIVATE';
    CM_CONTROLLISTCHANGING  : Result := 'CM_CONTROLLISTCHANGING';
    CM_BUFFEREDPRINTCLIENT  : Result := 'CM_BUFFEREDPRINTCLIENT';
    CM_UNTHEMECONTROL       : Result := 'CM_UNTHEMECONTROL';
    CN_CHARTOITEM           : Result := 'CN_CHARTOITEM';
    CN_COMMAND              : Result := 'CN_COMMAND';
    CN_COMPAREITEM          : Result := 'CN_COMPAREITEM';
    CN_CTLCOLORBTN          : Result := 'CN_CTLCOLORBTN';
    CN_CTLCOLORDLG          : Result := 'CN_CTLCOLORDLG';
    CN_CTLCOLOREDIT         : Result := 'CN_CTLCOLOREDIT';
    CN_CTLCOLORLISTBOX      : Result := 'CN_CTLCOLORLISTBOX';
    CN_CTLCOLORMSGBOX       : Result := 'CN_CTLCOLORMSGBOX';
    CN_CTLCOLORSCROLLBAR    : Result := 'CN_CTLCOLORSCROLLBAR';
    CN_CTLCOLORSTATIC       : Result := 'CN_CTLCOLORSTATIC';
    CN_DELETEITEM           : Result := 'CN_DELETEITEM';
    CN_DRAWITEM             : Result := 'CN_DRAWITEM';
    CN_HSCROLL              : Result := 'CN_HSCROLL';
    CN_MEASUREITEM          : Result := 'CN_MEASUREITEM';
    CN_PARENTNOTIFY         : Result := 'CN_PARENTNOTIFY';
    CN_VKEYTOITEM           : Result := 'CN_VKEYTOITEM';
    CN_VSCROLL              : Result := 'CN_VSCROLL';
    CN_KEYDOWN              : Result := 'CN_KEYDOWN';
    CN_KEYUP                : Result := 'CN_KEYUP';
    CN_CHAR                 : Result := 'CN_CHAR';
    CN_SYSKEYDOWN           : Result := 'CN_SYSKEYDOWN';
    CN_SYSCHAR              : Result := 'CN_SYSCHAR';
    CN_NOTIFY               : Result := 'CN_NOTIFY';
  else
    Result := Format('$%4.4X', [aMsg]);
  end; {case}
end;

{: If enabled log a call to a controls PreProcessMessage method.}
procedure LogPreProcessMessage(const Msg: TMsg; Ctrl: TControl);
begin
  if lfPreProcessMessage in KeytraceMain.LogFeatures then
    if InRange(Msg.message, WM_KEYFIRST, WM_KEYLAST) then
      KeytraceMain.Display(
       'PreProcessMessage called for %s on %s',
       [MessageToText(Msg.message), Ctrl.Name]);
end;

type
  TShifts = (ssShift, ssAlt, ssCtrl, ssLeft, ssRight, ssMiddle, ssDouble);
  TShiftSet = set of TShifts;

function ShiftstateToString( Shift: TShiftState ): String;
var
  state: TShifts;
  sl: TStringlist;
begin
  sl:= Tstringlist.create;
  Try
    For state := Low( state ) to HIgh(state) Do
      If state In TShiftSet( shift ) Then
        sl.Add(GetEnumname( Typeinfo( TShifts ), Ord( state )));
    result := '['+sl.CommaText+']';
  Finally
    sl.free;
  End; { Finally }
end;


destructor TKeytraceMain.Destroy;
begin
  Application.OnShortcut := nil;
  inherited;
end;

{: This method is used as handler for the Application.OnMessage event.
 We use it to display the message parameters for key messages.}
procedure TKeytraceMain.AppMessage(var Msg: TMsg; var Handled: Boolean);
begin
  case Msg.message of
    WM_KEYFIRST..WM_KEYLAST:
      DisplayKeyMsg(Msg);
    WM_SYSCOMMAND:
      if (Msg.wParam and $FFF0) = SC_KEYMENU then
        DisplayKeyMenuMsg(Msg);
  end; {case}
end;

procedure TKeytraceMain.AppShortCut(var Msg: TWMKey; var Handled:
  Boolean);
begin
  if lfAppOnShortcut in LogFeatures then
    DisplayWMKey('Application.OnShortCut', Msg);
end;

{: Dynamically build actions and menu items for the user to modify the
  set of log features. This method is called once by the forms onCreate
  event handler. We use the actions Tag property to identify the feature
  the action is to handle.}
procedure TKeytraceMain.BuildLogMenu;
var
  lf: TLogFeature;
  Action: TAction;
  MItem: TMenuItem;
begin
  for lf := Low(TLogFeature) to High(TLogFeature) do begin
    Action := TAction.Create(Self);
    Action.Tag := Ord(lf);
    Action.Caption := CLogMenuCaptions[lf];
    Action.OnUpdate := UpdateLogAction;
    Action.OnExecute:= ExecuteLogAction;
    Action.Category := CLogCategory;
    Action.ActionList := ActionList;
    MItem := TMenuItem.Create(Self);
    LogMenu.Add(MItem);
    MItem.Action := Action;
  end; {for}
end;

procedure TKeytraceMain.CloseActionClick(Sender: TObject);
begin
  Close;
  
end;

{: Disable the form for one tick of the ReenableTimer}
procedure TKeytraceMain.DisableButtonClick(Sender: TObject);
begin
  ClearFocusButton.Click;
  Enabled := false;
  ReenableTimer.Enabled := true;
  Display('Window disabled for %d milliseconds', [ReenableTimer.Interval]);
end;

{: Add the passed string as new line to the log memo.}
procedure TKeytraceMain.Display(const S: String);
begin
  Logmemo.Lines.Add(S);
end;

{: Add a formatted line to the logmemo. See Format routine for a
 description of the placeholders allowed in Fmt.}
procedure TKeytraceMain.Display(const Fmt: string; const A: array of const);
begin
  Display(Format(Fmt,A));
end;

{: Display a menu key message. Seems to be pretty impossible to
 generate one in a Delphi app, though. }
procedure TKeytraceMain.DisplayKeyMenuMsg(const  Msg: TMsg);
begin
  Display(
    'Message: WM_SYSCOMMAND, Receiver: %s, Charcode: $%x (%s)',
    [Receivername(Msg.hwnd), Lo(Msg.lParam), Chr(Lo(Msg.lParam))]);
end;

{: Display the information contained in the message parameters
 for a key message.
 @param Msg is the original message record.
 @desc See the MS Platform SDK docs for one of the key messsages for
   a description of the message parameters. The decoding of the
   individual parts is left to helper methods. }
procedure TKeytraceMain.DisplayKeyMsg(const Msg: TMsg);
var
  PWmKey: ^TWMKey;

  {: Returns the key label, usually matching the text shown on the key
    caps, or the character created by the key.}
  function KeyAsstring: string;
  begin
    case Msg.message of
      WM_KEYDOWN, WM_KEYUP, WM_SYSKEYDOWN, WM_SYSKEYUP:
        Result := KeyName(pWMKey^.KeyData);
    else
      Result := Chr(pWmKey^.CharCode);
    end; {case}
  end;

  {: Returns a prefix to use in the display string for key or character
    messages.}
  function CharOrKey: string;
  begin
    case Msg.message of
      WM_KEYDOWN, WM_KEYUP, WM_SYSKEYDOWN, WM_SYSKEYUP:
        Result := 'Key';
    else
      Result := 'Char';
    end; {case}
  end;

begin
  pWMKey := @Msg.Message;
  Display(
    'Message: %s, Receiver: %s, %scode: $%x (%s), '+
     'Repeat count %d, Scancode $%x, Flags: %s',
    [MessageToText(Msg.message), Receivername(Msg.hwnd),
     CharOrKey, PWmKey^.Charcode,
     KeyAsString, RepeatCount(Msg.lparam),
     Scancode(Msg.lparam), Flags(Msg.lparam)]);
end;

{: Returns a string representing a set of flag bits in the key
  messages lparam. The flags are stored in the highmost byte of
  the lparam. Not all bits are used by the API.}
function TKeytraceMain.Flags(Keydata: longint): string;
var
  FlagByte: Byte;
  sl: TStringlist;
begin
  FlagByte := Hi(Hiword(Keydata));
  if FlagByte <> 0 then begin
    sl:= TStringlist.Create;
    try
      {Bit 0 identifies an extended key like the right Ctrl and Alt}
      if (Flagbyte and 1) <> 0  then
        sl.Add('ExtendedKey');
      {Bit 5 is set when the Alt key is down}
      if (Flagbyte and $20) <> 0  then
        sl.Add('AltDown');
      {Bit 6 is the previous state of the key, down (0) or up (1)}
      if (Flagbyte and $40) <> 0  then
        sl.Add('KeyIsDown');
      {Bit 7 is the transition state, 0 if the key went down, 1 if it
       went up.}
      if (Flagbyte and $80) <> 0  then
        sl.Add('KeyUp');
      Result := Format('[%s]',[sl.Commatext]);
    finally
      sl.Free;
    end; {finally}
  end {if}
  else
    Result := '[]';
end;

{: Hook the Application.OnMessage event when the form is created and
  clear design-time captions for edit and memo. Prepare for logging
  the diverse types of input events we can show. The default set
  is determined by the conditional symbols define near the start
  of the unit, but the user can modify this via the forms Log
  menu.}
procedure TKeytraceMain.FormCreate(Sender: TObject);
begin
  RestoreFormState;
  BuildLogMenu;
  Application.OnMessage := AppMessage;
  Logmemo.Clear;
  Edit1.Clear;
  PrepareHintMessageLogging;
  SubClassEdit1;
  Application.OnShortcut := AppShortCut;
  Display('Ready, hit keys at leisure');
end;

{: Returns the label for the key identified by the passed message
  lparam. There is an API function to get the key label. This string
  is localized, so different for different keyboard languages. }
function TKeytraceMain.KeyName(Keydata: longint): string;
var
  Buffer: array [0..64] of char;
begin
  Buffer[0] := #0;
  GetKeyNameText(Keydata, Buffer, Sizeof(Buffer));
  Result := Buffer;
end;

{: Overrides the TWinControl.PreProcessMessage method to log the call.}
function TKeytraceMain.PreProcessMessage(var Msg: TMsg): Boolean;
begin
  Result := inherited PreProcessMessage(Msg);
  LogPreProcessMessage(Msg, Self);
end;

{: Return the name of the control represented by the passed HWND. If the
  window handle does not belong to a VCL control we return a hexadecimal
  representation of the window handle.}
function TKeytraceMain.Receivername(wnd: HWnd): string;
var
  ctrl: TWinControl;
begin
  ctrl:= FindControl(wnd);
  if Assigned(ctrl) then
    Result:= ctrl.Name
  else
    Result := IntToHex(Cardinal(wnd),8);
end;

{: Handles the OnTimer event of the timer used to re-enable the
  form after it has been disabled by the DisableButton.}
procedure TKeytraceMain.ReenableTimerTimer(Sender: TObject);
begin
  ReenableTimer.Enabled:= false;
  Enabled := true;
  Display('Window enabled');
  CloseButton.Setfocus;
end;

{: Returns the key repeat count from the passed key message lparam.
  The repeat count is stored in the loword of the parameter. }
function TKeytraceMain.RepeatCount(Keydata: longint): Integer;
begin
  Result := Keydata and $FFFF;
end;

{: Returns the key scancode from the passed key message lparam. The
  scancode is stored in the third byte of the parameter. Note that
  scancodes for some of the keys can vary between different keyboard
  layouts. }
function TKeytraceMain.Scancode(Keydata: longint): Integer;
begin
  Result := HiWord(Keydata) and $FF;
end;

{: Remove focus from this applications controls. The user can put
  the focus back by clcking on a control.}
procedure TKeytraceMain.ClearFocusButtonClick(Sender: TObject);
begin
  Display('Clearing focus...');
  Windows.SetFocus(0);
  Display('Click on a control to give it focus, current focus is on %s',
    [Receivername(Windows.GetFocus)]);
end;

procedure TKeytraceMain.CMChildkey(var Message: TCMChildkey);
begin
  inherited;
  if lfCMChildkey in LogFeatures then
    Display('CM_Childkey: key code %4.4X, sender: %s',
      [Message.Charcode, Message.Sender.Name]);
end;

procedure TKeytraceMain.CMDialogChar(var Message: TCMDialogChar);
begin
  inherited;
  DisplayCMDialogChar(Self, Message);
end;

procedure TKeytraceMain.CMDialogKey(var Message: TWMKey);
begin
  inherited;
  if lfCMDialogKey IN LogFeatures then
    DisplayWMKey('CM_DialogKey', Message);
end;

procedure TKeytraceMain.DisplayCMDialogChar(const Sender: TComponent;
  const Msg: TCMDialogChar);

  function Character: string;
  begin
    if Msg.CharCode > 32 then
      Result := Chr(Msg.Charcode)
    else
      Result := '#'+IntToStr(Msg.Charcode);
  end;

begin
  if lfCMDialogChar In LogFeatures then;
    Display('%s.CMDialogChar: Charcode %x (%s), Handled: %s',
      [Sender.Name, Msg.CharCode, Character,
       BoolToStr(Msg.Result <> 0, true)]);
end;

procedure TKeytraceMain.DisplayCMWantSpecialKey(const aName: string;
  const Msg: TCMWantSpecialKey);
const
  StateStrings: array [boolean] of string = ('down', 'up');
begin
  if lfCMWantSpecialKey in Logfeatures then
    Display('%s.CMWantSpecialKey: Key: %4.4X, key is %s',
      [aName, Msg.CharCode,
       StateStrings[GetKeyState(Msg.Charcode) >= 0]]);
end;

procedure TKeytraceMain.DisplayOnKeyDown(const Name: string; Key: Word;
  Shift: TShiftState);
begin
  Display('%s.OnKeyDown:, Key: %4.4x, Shiftstate: %s',
    [Name, Key, ShiftstateToString(Shift)]);
end;

procedure TKeytraceMain.DisplayOnKeyPress(const Name: string; Key:
  Char);
begin
  Display('%s.OnKeyPress:, Key: "%s"',
    [Name, Key]);
end;

procedure TKeytraceMain.DisplayOnKeyUp(const Name: string; Key: Word;
  Shift: TShiftState);
begin
  Display('%s.OnKeyUp:, Key: %4.4x, Shiftstate: %s',
    [Name, Key, ShiftstateToString(Shift)]);
end;

procedure TKeytraceMain.DisplayWMKey(const Procname: string; const
  Message: TWMKey);
begin
  Display(
      '%s: msg:%s, charcode:%4.4X, Keydata:%8.8X, Result: %d',
      [Procname, MessageToText(Message.Msg), Message.charcode,
       Message.KeyData, Message.Result]);
end;

procedure TKeytraceMain.Edit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if lfOnKeyDown in LogFeatures then
    DisplayOnKeyDown((Sender as TControl).Name, Key, Shift)
end;

procedure TKeytraceMain.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if lfOnKeyPress in LogFeatures then
    DisplayOnKeyPress((Sender as TControl).Name, Key)
end;

procedure TKeytraceMain.Edit1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if lfOnKeyUp in LogFeatures then
    DisplayOnKeyUp((Sender as TControl).Name, Key, Shift)
end;

{: Handles the OnExecute event of the actions we created on startup
  for the user to modify the log feature set.}
procedure TKeytraceMain.ExecuteLogAction(Sender: TObject);
var
  Action: TAction;
  lf: TLogFeature;
begin
  Action := nil;
  if Sender is TAction then
    Action := TAction(Sender)
  else if Sender is TMenuitem then
    Action := TMenuitem(Sender).Action as TAction
  else
    Assert(false, Format('ExecuteLogAction: unexpected sender, class %s',
      [Sender.Classname]));

  Assert(InRange(Action.Tag, Ord(Low(TLogFeature)), Ord(High(TLogFeature))),
    'ExecuteLogAction: Action tag out of range');
  lf := TLogFeature(Action.Tag);
  if lf In LogFeatures then
    Exclude(FLogFeatures, lf)
  else
    Include(FLogFeatures, lf);
  Action.Checked := lf In LogFeatures;
end;

procedure TKeytraceMain.FormCloseQuery(Sender: TObject; var CanClose:
  Boolean);
begin
  SaveFormstate;
end;

procedure TKeytraceMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if lfOnKeyDown in LogFeatures then
    DisplayOnKeyDown(Name, Key, Shift)
end;

procedure TKeytraceMain.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if lfOnKeyPress in LogFeatures then
    DisplayOnKeyPress(Name, Key)
end;

procedure TKeytraceMain.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if lfOnKeyUp in LogFeatures then
    DisplayOnKeyUp(Name, Key, Shift)
end;

procedure TKeytraceMain.FormShortCut(var Msg: TWMKey; var Handled:
  Boolean);
begin
  if lfFormOnShortcut in LogFeatures then
    DisplayWMKey('TKeytraceMain.OnShortCut', Msg);
end;

{: Sets the mouse capture to the form. Used to show how PreProcessMessage
  depends on the capture state. }
procedure TKeytraceMain.GrapCaptureButtonClick(Sender: TObject);
begin
  Mouse.Capture := Self.Handle;
  Display('Mouse capture set to the form');
end;

function TKeytraceMain.IsMouseMsg(const Msg: TMessage): Boolean;
begin
  Result := (Msg.Msg >= WM_MOUSEFIRST) and (Msg.Msg <= WM_MOUSELAST);
end;

function TKeytraceMain.IsShortCut(var Message: TWMKey): Boolean;
begin
  if lfFromIsShortcut in LogFeatures then
    DisplayWMKey('TKeytraceMain.IsShortCut', Message);

  Result := inherited IsShortCut(Message);
end;

{: Handles the OnClick event of the LogClearMenu to wipe the log. }
procedure TKeytraceMain.LogClearMenuClick(Sender: TObject);
begin
  LogMemo.Clear;
end;

procedure TKeytraceMain.LogSettingsMenuClick(Sender: TObject);
begin
  TKTLogFeaturesDialog.Execute(FLogFeatures);
  UpdateActionCheckstate;
end;

procedure TKeytraceMain.NewEdit1WndProc(var Msg: TMessage);
begin
  FOldEdit1WndProc(Msg);
  if (lfWindowproc in LogFeatures) and not IsMouseMsg(Msg) then
    Display(
      'NewEdit1WndProc(msg:%s, wparam:%8.8X, lparam:%8.8X)= %d',
      [MessageToText(Msg.Msg), Msg.wParam, Msg.lParam,
       Msg.Result]);

end;

{: Sets up the preconditions for logging calls to IsHintMsg. Called
  once by the forms OnCreate event handler, defines the hint window
  class to use and sets the Hint properties of all controls to the
  control names.}
procedure TKeytraceMain.PrepareHintMessageLogging;

  procedure SetSampleHint(aControl: TControl);
  var
    I: Integer;
  begin
    aControl.Hint := aControl.Name;
    if aControl is TwinControl then
      for I := 0 to TWinControl(aControl).ControlCount - 1 do
        SetSampleHint(TWinControl(aControl).Controls[I]);
  end;

begin
  HintWindowClass := TLogHintWindow;
  SetSampleHint(self);
  ShowHint := true;
end;

procedure TKeytraceMain.RestoreFormState;
begin
  FLogFeatures := AppMemory.LogFeatures;
  AppMemory.RestoreFormState(Self);
end;

procedure TKeytraceMain.SaveFormstate;
begin
  AppMemory.SaveFormState(Self );
  AppMemory.LogFeatures := LogFeatures;
end;

procedure TKeytraceMain.SubClassEdit1;
begin
  FOldEdit1WndProc := Edit1.Windowproc;
  Edit1.WindowProc := NewEdit1WndProc;
end;

procedure TKeytraceMain.UpdateActionCheckstate;
var
  I: Integer;
  A: TAction;
  N: TLogFeature;
begin
  for I := 0 to ActionList.ActionCount - 1 do begin
    A := Actionlist.Actions[I] as TAction;
    if A.Category = CLogCategory then begin
      N := TLogFeature(A.Tag);
      A.Checked := N in LogFeatures;
    end; {if}
  end; {for}
end;

{: Check or uncheck the action based on its tag. }
procedure TKeytraceMain.UpdateLogAction(Sender: TObject);
var
  lf: TLogFeature;
  Action: TAction;
begin
  Action := Sender as TAction;
  Assert(InRange(Action.Tag, Ord(Low(TLogFeature)), Ord(High(TLogFeature))),
    'UpdateLogAction: Action tag out of range');
  lf := TLogFeature(Action.Tag);
  Action.Checked := lf In LogFeatures;
end;

{== TEdit interposer class ============================================}

procedure TEdit.CMWantspecialKey(var Message: TCMWantSpecialKey);
begin
  inherited;
  KeytraceMain.DisplayCMWantSpecialKey(Name, Message);
end;

function TEdit.PreProcessMessage(var Msg: TMsg): Boolean;
begin
  Result := inherited PreProcessMessage(Msg);
  LogPreProcessMessage(Msg, Self);
end;

{== TButton interposer class ==========================================}

procedure TButton.CMDialogChar(var Message: TCMDialogChar);
begin
  inherited;
  KeytraceMain.DisplayCMDialogChar(Self, Message);
end;

procedure TButton.CMWantspecialKey(var Message: TCMWantSpecialKey);
begin
  inherited;
  KeytraceMain.DisplayCMWantSpecialKey(Name, Message);
end;

function TButton.PreProcessMessage(var Msg: TMsg): Boolean;
begin
  Result := inherited PreProcessMessage(Msg);
  LogPreProcessMessage(Msg, Self);
end;

{== TLogHintWindow ====================================================}
var
  LastMsg: Cardinal = 0;

function TLogHintWindow.IsHintMsg(var Msg: TMsg): Boolean;

  function IsLikeLastMessage:Boolean;
  begin
    case Msg.Message of
      WM_MOUSEMOVE, WM_NCMOUSEMOVE, $0118 {undocumented but frequent}
        : Result := LastMsg = Msg.Message
    else
      Result := false;
    end; {case}
    LastMsg := Msg.Message;
  end;

begin
  Result := inherited IsHintMsg(Msg);
  if (lfHintMessage in KeytraceMain.LogFeatures)
    and not IsLikeLastMessage
  then
    KeytraceMain.Display(
      'IsHintMsg(msg:%s, wparam:%8.8X, lparam:%8.8X)= %s',
      [MessageToText(Msg.message), Msg.wParam, Msg.lParam,
       BoolToStr(Result, true)]);
end;

end.
