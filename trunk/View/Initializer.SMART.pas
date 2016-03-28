unit Initializer.SMART;

interface

uses
  SysUtils,
  Global.LanguageString, Device.PhysicalDrive, Support;

type
  TMainformSMARTApplier = class
  private
    ReadEraseError: TReadEraseError;
    procedure ApplyOnTime;
    procedure ApplyReadEraseError;
    procedure SetLabelByTrueReadErrorFalseEraseError(
      TrueReadErrorFalseEraseError: Boolean);
  public
    procedure ApplyMainformSMART;
  end;

implementation

uses Form.Main;

procedure TMainformSMARTApplier.ApplyOnTime;
begin
  fMain.lOntime.Caption :=
    CapPowerTime[CurrLang] +
    UIntToStr(fMain.SelectedDrive.SMARTInterpreted.UsedHour) +
    CapHour[CurrLang];
end;

procedure TMainformSMARTApplier.SetLabelByTrueReadErrorFalseEraseError(
  TrueReadErrorFalseEraseError: Boolean);
begin
  if ReadEraseError.TrueReadErrorFalseEraseError then
    fMain.lPError.Caption := CapReadError[CurrLang]
  else
    fMain.lPError.Caption := CapWriteError[CurrLang];
  fMain.lPError.Caption := fMain.lPError.Caption +
    UIntToStr(ReadEraseError.Value) +
    CapCount[CurrLang];
end;

procedure TMainformSMARTApplier.ApplyReadEraseError;
begin
  ReadEraseError := fMain.SelectedDrive.SMARTInterpreted.ReadEraseError;
  SetLabelByTrueReadErrorFalseEraseError(
    ReadEraseError.TrueReadErrorFalseEraseError);
end;


procedure TMainformSMARTApplier.ApplyMainformSMART;
begin
  ApplyOnTime;
  ApplyReadEraseError;
end;

end.