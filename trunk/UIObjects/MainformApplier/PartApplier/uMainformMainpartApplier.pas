unit uMainformMainpartApplier;

interface

uses
  Graphics,
  uLanguageSettings, uPhysicalDrive, uSSDLabelListRefresher, uFirmwareGetter,
  uDatasizeUnit, uStrFunctions, uBufferInterpreter, uNCQAvailabilityGetter;

type
  TMainformMainpartApplier = class
  private
    procedure ApplyConnectionState;
    procedure ApplyFirmware;
    procedure ApplyModelAndSize;
    procedure ApplySerial(IsSerialOpened: Boolean);
    function BinaryKiBToDenaryKB(Size: Double): Double;
    function FormatSizeInDenaryIntegerMB(SizeInDenaryMB: Double): String;
    function GetConnectionSpeedString: String;
    function GetConnectionStateString: String;
    function GetFirmwareQueryResult: TFirmwareQueryResult;
    function GetNCQAvailabilityString: String;
    procedure IfOldFirmwareSetLabelBoldAndRed(IsOldFirmware: Boolean);
    function IsUnknownConnection: Boolean;
    function KiBtoDenaryMB(SizeInBinaryKiB: Double): Double;
    procedure SetFirmwareLabel;
    procedure SetNewFirmwareCaption(LatestVersion: String);
  public
    procedure ApplyMainformMainpart(
      IsSerialOpened: Boolean);
  end;

implementation

uses uMain;

function TMainformMainpartApplier.BinaryKiBToDenaryKB(Size: Double):
  Double;
begin
  exit(Size * (1024 / 1000));
end;

function TMainformMainpartApplier.KiBtoDenaryMB(SizeInBinaryKiB: Double):
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

function TMainformMainpartApplier.FormatSizeInDenaryIntegerMB(
  SizeInDenaryMB: Double): String;
var
  DenaryInteger: FormatSizeSetting;
begin
  DenaryInteger.FNumeralSystem := Denary;
  DenaryInteger.FPrecision := 0;
  
  result := FormatSizeInMB(SizeInDenaryMB, DenaryInteger);
end;

procedure TMainformMainpartApplier.ApplyModelAndSize;
var
  DenaryUserSizeInMB: Double;
begin
  DenaryUserSizeInMB := KiBtoDenaryMB(
    fMain.PhysicalDrive.IdentifyDeviceResult.UserSizeInKB);

  fMain.lName.Caption :=
    fMain.PhysicalDrive.IdentifyDeviceResult.Model + ' ' +
    FormatSizeInDenaryIntegerMB(DenaryUserSizeInMB);
end;

procedure TMainformMainpartApplier.SetFirmwareLabel;
begin
  fMain.lFirmware.Caption :=
    CapFirmware[CurrLang] + fMain.PhysicalDrive.IdentifyDeviceResult.Firmware;
end;

function TMainformMainpartApplier.IsUnknownConnection: Boolean;
begin
  result := 
    fMain.PhysicalDrive.IdentifyDeviceResult.SATASpeed <=
    TSATASpeed.UnknownSATASpeed;
end;

function TMainformMainpartApplier.GetConnectionSpeedString: String;
var
  SpeedStarts: Integer;
begin
  SpeedStarts :=
    Integer(TSATASpeed.UnknownSATASpeed) + 1;
  result :=
    CapConnSpeed[Integer(fMain.PhysicalDrive.IdentifyDeviceResult.SATASpeed) -
      SpeedStarts];
end;

function TMainformMainpartApplier.GetNCQAvailabilityString: String;
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

function TMainformMainpartApplier.GetConnectionStateString: String;
const
  CloseState = ')';
begin
  if IsUnknownConnection then
    result := CapUnknown[CurrLang]
  else
    result := GetConnectionSpeedString + GetNCQAvailabilityString + CloseState;
end;

procedure TMainformMainpartApplier.ApplyConnectionState;
begin
  fMain.lConnState.Caption := CapConnState[CurrLang] +
    GetConnectionStateString;
end;

procedure TMainformMainpartApplier.ApplySerial(
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

function TMainformMainpartApplier.GetFirmwareQueryResult:
  TFirmwareQueryResult;
var
  Query: TFirmwareQuery;
begin
  Query.Model := fMain.PhysicalDrive.IdentifyDeviceResult.Model;
  Query.Firmware := fMain.PhysicalDrive.IdentifyDeviceResult.Firmware;
  
  result := fMain.FirmwareGetter.CheckFirmware(Query);
end;

procedure TMainformMainpartApplier.IfOldFirmwareSetLabelBoldAndRed(
  IsOldFirmware: Boolean);
begin    
  if IsOldFirmware then
  begin
    fMain.lFirmware.Caption := fMain.lFirmware.Caption +
      CapOldVersion[CurrLang];
    fMain.lFirmware.Font.Color := clRed;
    fMain.lFirmware.Font.Style := [fsBold];
  end;
end;

procedure TMainformMainpartApplier.SetNewFirmwareCaption(
  LatestVersion: String);
begin
  fMain.lNewFirm.Caption :=
    CapNewFirm[CurrLang] + LatestVersion;
end;

procedure TMainformMainpartApplier.ApplyFirmware;
var
  QueryResult: TFirmwareQueryResult;
begin
  QueryResult := GetFirmwareQueryResult;
  SetFirmwareLabel;
  SetNewFirmwareCaption(QueryResult.LatestVersion);
  IfOldFirmwareSetLabelBoldAndRed(
    QueryResult.CurrentVersion = TFirmwareVersion.OldVersion);
end;

procedure TMainformMainpartApplier.ApplyMainformMainpart(
  IsSerialOpened: Boolean);
begin
  ApplyModelAndSize;
  ApplySerial(IsSerialOpened);
  ApplyConnectionState;
  ApplyFirmware;
end;

end.