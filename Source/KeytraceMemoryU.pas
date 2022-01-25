{== KeytraceMemoryU ===================================================}
{! <summary>
 This unit implements a singleton that manages the applications
 settings.
 </summary>
<author>Dr. Peter Below</author>
<history>
<para>Version 1.0 created 2008-08-01</para>
<para>Last modified       2008-08-01</para>
</history>
<remarks></remarks>}
{======================================================================}
unit KeytraceMemoryU;

interface

uses Classes, Forms, KeytraceTypesU;

type
  IAppMemory = interface(IInterface)
  ['{F6B57687-AD8B-485A-BB58-958828221E6F}']
    function GetLogFeatures: TLogFeatures;
    procedure RestoreFormState(aform: TForm);
    procedure SaveFormState(aForm: TForm);
    procedure SetLogFeatures(const Value: TLogFeatures);
    property LogFeatures: TLogFeatures read GetLogFeatures write
      SetLogFeatures;
  end;

function AppMemory: IAppMemory;

implementation

uses Windows, Sysutils, Registry, Inifiles, WinSet, InterlockedOpsU,
  TypInfo, Math;


const
  sAppRegKey = 'Software\PeterBelow\Keytrace';
  sSettings  = 'Settings';
  sLogfeatures= 'Logfeatures';

type
  TAppMemory = class(TInterfacedObject,
      IAppMemory)
  private
    FMemory: TCustomInifile;
    function GetLogFeatures: TLogFeatures;
    procedure SetLogFeatures(const Value: TLogFeatures);
  protected
    procedure RestoreFormState(aform: TForm);
    procedure SaveFormState(aForm: TForm);
  public
    constructor Create;
    destructor Destroy; override;
    property Memory: TCustomInifile read FMemory;
  end;

{== TAppMemory ============================================}

constructor TAppMemory.Create;
begin
  inherited;
  FMemory:= TRegistryInifile.Create(sAppRegKey, KEY_READ or KEY_WRITE);
end;

destructor TAppMemory.Destroy;
begin
  FMemory.Free;
  inherited;
end;

function TAppMemory.GetLogFeatures: TLogFeatures;
var
  SL: TStringList;
  S: string;
  I, N: Integer;
begin
  Result := [];
  S := Memory.ReadString(sSettings, sLogfeatures, '');
  if S <> '' then begin
    SL := TStringList.Create();
    try
      SL.Commatext := S;
      for I := 0 to SL.Count - 1 do begin
        N:= GetEnumValue(Typeinfo(TLogFeature), SL[I]);
        if InRange(N, Ord(Low(TLogFeature)), Ord(High(TLogFeature)))
        then
          Include(Result, TLogFeature(N));
      end; {for}
    finally
      SL.Free;
    end; {finally}
  end; {if}
end;

procedure TAppMemory.RestoreFormState(aform: TForm);
begin
  WinSet.RestoreWindowstateEx(FMemory, aForm, aForm.Name);
end;

procedure TAppMemory.SaveFormState(aForm: TForm);
begin
  WinSet.SaveWindowstateEx(FMemory, aForm, aForm.Name);
end;

procedure TAppMemory.SetLogFeatures(const Value: TLogFeatures);
var
  SL: TStringlist;
  N: TLOgFeature;
begin
  SL := TStringlist.Create();
  try
    for N := LOw(TLOgFeature) to High(TLogFeature) do
      if N In Value then
        SL.Add(GetEnumName(TypeInfo(TLogFeature), Ord(N)));
    Memory.WriteString(sSettings, sLogfeatures, SL.Commatext);
  finally
    SL.Free;
  end; {finally}
end;

var
  InternalAppMemory: IAppMemory;

function AppMemory: IAppMemory;
var
  P: TObject;
begin
  if Assigned(InternalAppMemory) then
    Result := InternalAppMemory
  else begin
    Result := TAppMemory.Create;
    Result._AddRef; // the call below does not increment the refcount!
    P:= InterlockedCompareExchangeObject(InternalAppMemory,
      TObject(Result), nil);
    if P <> nil then begin
      Result._Release;
      Result := InternalAppMemory;
    end; {if}
  end; {else}
end; {AppMemory}

initialization
finalization
  InternalAppMemory := nil;
end.
