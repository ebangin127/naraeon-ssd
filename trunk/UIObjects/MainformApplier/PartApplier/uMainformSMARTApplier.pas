unit uMainformSMARTApplier;

interface

uses
  uPhysicalDrive, uListChangeGetter;

type
  TMainformSMARTApplier = class
  private
    ReadEraseError: UInt64;
  public
    procedure ApplyMainformSMART;
  end;

implementation

uses uMain;

procedure TMainformSMARTApplier.ApplyOnTime;
begin
  fMain.lOntime.Caption :=
    CapPowerTime[CurrLang] +
    UIntToStr(fMain.PhysicalDrive.SMARTInterpreted.UsedHour) +
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
  ReadEraseError := fMain.PhysicalDrive.SMARTInterpreted.ReadEraseError;
  SetLabelByTrueReadErrorFalseEraseError(
    ReadEraseError.TrueReadErrorFalseEraseError);
end;


procedure TMainformSMARTApplier.ApplyMainformSMART;
begin
  ApplyOnTime;
  ApplyReadEraseError;
end;

end.