unit Initializer.CriticalWarning;

interface

uses
  SysUtils,
  OS.EnvironmentVariable, Global.LanguageString, Device.PhysicalDrive,

  Support;

type
  TMainformCriticalWarningApplier = class
  private
    procedure FillSectorLabel;
    procedure ShowCriticalWarning;
  public
    procedure ApplyMainformCriticalWarning;
  end;

implementation

uses Form.Main;

procedure TMainformCriticalWarningApplier.ApplyMainformCriticalWarning;
begin
  FillSectorLabel;
end;

procedure TMainformCriticalWarningApplier.FillSectorLabel;
begin
  ShowCriticalWarning;
end;

procedure TMainformCriticalWarningApplier.ShowCriticalWarning;
begin
  if fMain.SelectedDrive.SupportStatus.Supported = CDIInsufficient then
    fMain.lSectors.Caption :=
      CriticalWarning[CurrLang] + CapUnknown[CurrLang]
  else if not fMain.SelectedDrive.SMARTInterpreted.SMARTAlert.CriticalError then
    fMain.lSectors.Caption :=
      CriticalWarning[CurrLang] + SafeWithoutDot[CurrLang]
  else
    fMain.lSectors.Caption :=
      CriticalWarning[CurrLang] + Bad[CurrLang];
end;

end.
