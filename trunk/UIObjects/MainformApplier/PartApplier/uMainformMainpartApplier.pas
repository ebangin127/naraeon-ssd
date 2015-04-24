unit uMainformReplacedSectorApplier;

interface

uses
  uPhysicalDrive, uListChangeGetter;

type
  TMainformReplacedSectorApplier = class
  public
    procedure ApplyMainformMainpart(
      IsSerialOpened: Boolean);
  end;

implementation

uses uMain;

function TMainformReplacedSectorApplier.BinaryKiBToDenaryKB(Size: Double):
  Double;
begin
  exit(Size * (1024 / 1000));
end;

function TMainformReplacedSectorApplier.KiBtoDenaryMB(SizeInBinaryKiB: Double):
  Double;
var
  KBtoMB: TDatasizeUnitChangeSetting;
begin
  KBtoMB.FNumeralSystem := Denary;
  KBtoMB.FFromUnit := KiloUnit;
  KBtoMB.FToUnit := MegaUnit;

  result :=
    ChangeDatasizeUnit(
      BinaryKiBToDenaryKB(SizeInBinaryKiB),
      KBtoMB);
end;

function TMainformReplacedSectorApplier.FormatSizeInDenaryIntegerMB(
  SizeInDenaryMB: Double): String;
var
  DenaryInteger: FormatSizeSetting;
begin
  DenaryInteger.FNumeralSystem := Denary;
  DenaryInteger.FPrecision := 0;
  
  result := FormatSizeInMB(SizeInDenaryMB, DenaryInteger);
end;

procedure TMainformReplacedSectorApplier.ApplyModelAndSize;
var
  DenaryUserSizeInMB: Double;
begin
  DenaryUserSizeInMB := KiBtoDenaryMB(
    fMain.PhysicalDrive.IdentifyDeviceResult.UserSizeInKB);

  fMain.lName.Caption :=
    fMain.PhysicalDrive.IdentifyDeviceResult.Model + ' ' +
    FormatSizeInDenaryIntegerMB(DenaryUserSizeInMB);
end;

procedure TMainformReplacedSectorApplier.ApplyFirmware;
begin
  fMain.lFirmware.Caption :=
    CapFirmware[CurrLang] + fMain.PhysicalDrive.IdentifyDeviceResult.Firmware;
end;

function TMainformReplacedSectorApplier.IsUnknownConnection: Boolean;
begin
  result := 
    fMain.PhysicalDrive.IdentifyDeviceResult.SATASpeed <=
    TSATASpeed.UnknownSATASpeed;
end;

function TMainformReplacedSectorApplier.GetConnectionSpeedString: String;
var
  SpeedStarts: Integer;
begin
  SpeedStarts :=
    Integer(TSATASpeed.UnknownSATASpeed) + 1;
  lConnState.Caption := lConnState.Caption +
    CapConnSpeed[Integer(fMain.PhysicalDrive.IdentifyDeviceResult.SATASpeed) 
      - SpeedStarts];
end;

function TMainformReplacedSectorApplier.GetNCQAvailabilityString: String;
var
  NCQAvailability: TNCQAvailability;
begin  
  NCQAvailability := fMain.PhysicalDrive.NCQAvailability;
  case NCQAvailability of
    TNCQAvailability.Unknown:
      result := CapUnknown[CurrLang];
  
    TNCQAvailability.Disabled:
      result := CapNonSupNCQ[CurrLang];
  
    TNCQAvailability.Enabled:
      result := CapSupportNCQ[CurrLang];
  end;
end;

procedure TMainformReplacedSectorApplier.GetConnectionStateString: String;
const
  CloseState = ')';
begin
  if IsUnknownConnection then
    result := CapUnknown[CurrLang]
  else
    result := GetConnectionSpeedString + GetNCQAvailabilityString + CloseState;
end;

procedure TMainformReplacedSectorApplier.ApplyConnectionState;
begin
  lConnState.Caption := CapConnState[CurrLang] +
    GetConnectionStateString;
end;

procedure TMainformReplacedSectorApplier.ApplySerial(
  IsSerialOpened: Boolean);
var
  CurrentSerial: Integer;
begin
  fMain.lSerial.Caption := CapSerial[CurrLang];
  if not IsSerialOpened then
    for CurrentSerial := 0 to
      Length(fMain.PhysicalDrive.IdentifyDeviceResult.Serial) - 1 do
        fMain.lSerial.Caption := fMain.lSerial.Caption + 'X'
  else
    fMain.lSerial.Caption := fMain.lSerial.Caption +
      fMain.PhysicalDrive.IdentifyDeviceResult.Serial;
end;

function TMainformReplacedSectorApplier.GetFirmwareQueryResult: 
  TFirmwareQueryResult;
var
  Query: TFirmwareQuery;
begin
  Query.Model := fMain.PhysicalDrive.IdentifyDeviceResult.Model;
  Query.Firmware := fMain.PhysicalDrive.IdentifyDeviceResult.Firmware;
  
  result := fMain.FirmwareGetter.CheckFirmware(Query);
end;

procedure TMainformReplacedSectorApplier.IfOldFirmwareSetLabelBoldAndRed(
  IsOldFirmware: Boolean);
begin    
  if  then
  begin
    lFirmware.Caption := lFirmware.Caption + CapOldVersion[CurrLang];
    lFirmware.Font.Color := clRed;
    lFirmware.Font.Style := [fsBold];
  end;
end;

procedure TMainformReplacedSectorApplier.SetFirmwareCaption(
  LatestVersion: String);
begin
  lNewFirm.Caption :=
    CapNewFirm[CurrLang] + LatestVersion;
end;

procedure TMainformReplacedSectorApplier.ApplyFirmware;
var
  QueryResult: TFirmwareQueryResult;
begin
  QueryResult := GetFirmwareQueryResult
  SetFirmwareCaption(QueryResult.LatestVersion);
  IfOldFirmwareSetLabelBoldAndRed(
    QueryResult.CurrentVersion = TFirmwareVersion.OldVersion);
end;

procedure TMainformReplacedSectorApplier.ApplyMainformMainpart(
  IsSerialOpened: Boolean);
begin
  ApplyModelAndSize;
  ApplySerial(IsSerialOpened);
  ApplyConnectionState;
  ApplyFirmware;
end;

end.