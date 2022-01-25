unit KTLogFeaturesDialogU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, CheckLst, ExtCtrls, KeytraceTypesU;

type
  TKTLogFeaturesDialog = class(TForm)
    ButtonPanel: TPanel;
    LogFeaturesList: TCheckListBox;
    OKButton: TBitBtn;
    CancelButton: TBitBtn;
    Label1: TLabel;
    procedure ButtonPanelResize(Sender: TObject);
  protected
    function GetLogFeatures: TLogFeatures;
    procedure SetLogFeatures(const Value: TLogFeatures);
  public
    class procedure Execute(var aLogFeatures: TLogFeatures);
    property LogFeatures: TLogFeatures read GetLogFeatures write
      SetLogFeatures;
  end;

implementation

{$R *.dfm}

procedure TKTLogFeaturesDialog.ButtonPanelResize(Sender: TObject);
begin
  OKButton.Left := ButtonPanel.ClientWidth div 2 - 4 - OKButton.Width;
  CancelButton.Left := OKButton.Left + OKButton.Width + 8;
end;

class procedure TKTLogFeaturesDialog.Execute(var aLogFeatures:
  TLogFeatures);
begin
  with TKTLogFeaturesDialog.Create(nil) do
  try
    LogFeatures := aLogFeatures;
    if ShowModal = mrOK then
      aLogFeatures := LogFeatures;
  finally
    Free;
  end;
end;

function TKTLogFeaturesDialog.GetLogFeatures: TLogFeatures;
var
  I: Integer;
begin
  Result := [];
  for I := 0 to LogFeaturesList.Count - 1 do
    if LogFeaturesList.Checked[I] then
      Include(Result, TLogFeature(Integer(LogFeaturesList.Items.Objects[I])));
end;

procedure TKTLogFeaturesDialog.SetLogFeatures(const Value:
  TLogFeatures);
var
  N: TLogFeature;
  I: Integer;
begin
  LogFeaturesList.Items.BeginUpdate;
  try
    LogFeaturesList.Clear;
    for N := Low(TLogFeature) to High(TLogFeature) do begin
      I:= LogFeatureslist.Items.AddObject(CLogMenuCaptions[N], Pointer(Ord(N)));
      LogFeaturesList.Checked[I] := N in Value;
    end; {for}
  finally
    LogFeaturesList.Items.EndUpdate;
  end; {finally}
end;

end.
