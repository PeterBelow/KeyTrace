program Keytrace;

uses
  Forms,
  KeytraceU1 in 'KeytraceU1.pas' {KeytraceMain},
  KeytraceTypesU in 'KeytraceTypesU.pas',
  KTLogFeaturesDialogU in 'KTLogFeaturesDialogU.pas' {KTLogSettingsDialog};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TKeytraceMain, KeytraceMain);
  Application.Run;
end.
