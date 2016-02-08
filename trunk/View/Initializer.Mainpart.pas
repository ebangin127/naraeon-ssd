unit Initializer.Mainpart;

interface

uses
  SysUtils, Graphics,
  Global.LanguageString, Device.PhysicalDrive, Initializer.SSDLabelListRefresh,
  Getter.LatestFirmware, MeasureUnit.DataSize, BufferInterpreter,
  Getter.PhysicalDrive.NCQAvailability, Device.SlotSpeed;

type
  TMainformMainpartApplier = class
  private
    procedure ApplyConnectionState;
    procedure ApplyFirmware;
    procedure ApplyModelAndSize;
    procedure ApplySerial(IsSerialOpened: Boolean);
    function GetConnectionSpeedString: String;
    function GetConnectionStateString: String;
    function GetFirmwareQueryResult: TFirmwareQueryResult;
    function GetNCQAvailabilityString: String;
    procedure IfOldFirmwareSetLabelBoldAndRed(IsOldFirmware: Boolean);
    function IsUnknownConnection: Boolean;
    procedure SetFirmwareLabel;
    procedure SetNewFirmwareCaption(LatestVersion: String);
    function IsNVMe: Boolean;
    function GetNVMeConnectionSpeedString: String;
  public
    procedure ApplyMainformMainpart(
      IsSerialOpened: Boolean);
  end;

implementation

uses Form.Main;

function DenaryByteToMB(SizeInByte: Double): Double;
var
  DenaryByteToMB: TDatasizeUnitChangeSetting;
begin
  DenaryByteToMB.FNumeralSystem := Denary;
  DenaryByteToMB.FFromUnit := ByteUnit;
  DenaryByteToMB.FToUnit := MegaUnit;

  result :=
    ChangeDatasizeUnit(SizeInByte, DenaryByteToMB);
end;

function FormatSizeInMBAsDenaryInteger(SizeInDenaryFloat: Double): String;
var
  DenaryInteger: TFormatSizeSetting;
begin
  DenaryInteger.FNumeralSystem := Denary;
  DenaryInteger.FPrecision := 0;

  result := FormatSizeInMB(SizeInDenaryFloat, DenaryInteger);
end;

procedure TMainformMainpartApplier.ApplyModelAndSize;
var
  DiskSizeInMB: Double;
begin
  DiskSizeInMB := DenaryByteToMB(fMain.PhysicalDrive.DiskSizeInByte);
  fMain.lName.Caption :=
    fMain.PhysicalDrive.IdentifyDeviceResult.Model + ' ' +
    FormatSizeInMBAsDenaryInteger(DiskSizeInMB);
end;

procedure TMainformMainpartApplier.SetFirmwareLabel;
begin
  fMain.lFirmware.Caption :=
    CapFirmware[CurrLang] + fMain.PhysicalDrive.IdentifyDeviceResult.Firmware;
end;

function TMainformMainpartApplier.IsUnknownConnection: Boolean;
begin
  result :=
    (fMain.PhysicalDrive.IdentifyDeviceResult.SATASpeed <=
    TSATASpeed.UnknownSATASpeed) and
    (not IsNVMe);
end;

function TMainformMainpartApplier.IsNVMe: Boolean;
begin
  result := fMain.PhysicalDrive.IdentifyDeviceResult.StorageInterface =
    TStorageInterface.NVMe;
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

function TMainformMainpartApplier.GetNVMeConnectionSpeedString: String;
begin
  if fMain.PhysicalDrive.IdentifyDeviceResult.SlotSpeed.SpecVersion =
    TPCIeSpecification.PCIeUnknownSpec then
      exit(CapUnknown[CurrLang]);

  result :=
    SlotSpecificationString[
      fMain.PhysicalDrive.IdentifyDeviceResult.SlotSpeed.SpecVersion] + ' x' +
    IntToStr(Ord(
      fMain.PhysicalDrive.IdentifyDeviceResult.SlotSpeed.LinkWidth));
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
  else if IsNVMe then
    result := GetNVMeConnectionSpeedString
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
  fMain.OnlineFirmwareUpdateAvailable :=
    (QueryResult.CurrentVersion = TFirmwareVersion.OldVersion) or
    (QueryResult.CurrentVersion = TFirmwareVersion.NewVersion);
  SetFirmwareLabel;

  if not fMain.OnlineFirmwareUpdateAvailable then
    exit;

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