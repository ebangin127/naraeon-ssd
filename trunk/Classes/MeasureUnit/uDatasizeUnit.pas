unit uDatasizeUnit;

interface

uses
  SysUtils, Math;

type
  TNumeralSystem = (Denary, Binary);

  TDatasizeUnit = record
    FPowerOfKUnit: Integer;
    FUnitName: String;
  end;

  TDatasizeUnitChangeSetting = record
    FNumeralSystem: TNumeralSystem;
    FFromUnit: TDatasizeUnit;
    FToUnit: TDatasizeUnit;
  end;

  TNumeralSystemChangeSetting = record
    FFromNumeralSystem: TNumeralSystem;
    FToNumeralSystem: TNumeralSystem;
  end;

  TFormatSizeSetting = record
    FNumeralSystem: TNumeralSystem;
    FPrecision: Integer;
  end;

const
  PetaUnit: TDatasizeUnit = (FPowerOfKUnit: 4; FUnitName: 'PB');
  TeraUnit: TDatasizeUnit = (FPowerOfKUnit: 3; FUnitName: 'TB');
  GigaUnit: TDatasizeUnit = (FPowerOfKUnit: 2; FUnitName: 'GB');
  MegaUnit: TDatasizeUnit = (FPowerOfKUnit: 1; FUnitName: 'MB');
  KiloUnit: TDatasizeUnit = (FPowerOfKUnit: 0; FUnitName: 'KB');
  ByteUnit: TDatasizeUnit = (FPowerOfKUnit: -1; FUnitName: 'B');

function GetDivideUnitSize(DivideUnitType: TNumeralSystem): Integer;
function ChangeDatasizeUnit
  (Size: Double; Setting: TDatasizeUnitChangeSetting): Double;
function LiteONUnitToMB(Size: UInt64): UInt64;
function MBToLiteONUnit(Size: UInt64): UInt64;
function FormatSizeInMB(SizeInMB: Double; Setting: TFormatSizeSetting): String;

implementation

function GetDivideUnitSize(DivideUnitType: TNumeralSystem): Integer;
const
  DenaryKilo = 1000;
  BinaryKibi = 1024;
begin
  case DivideUnitType of
    Denary:
      exit(DenaryKilo);
    Binary:
      exit(BinaryKibi);
  end;
  exit(1);
end;

function ChangeDatasizeUnit
  (Size: Double; Setting: TDatasizeUnitChangeSetting): Double;
begin
  result := Size *
    Power(GetDivideUnitSize(Setting.FNumeralSystem),
      Setting.FFromUnit.FPowerOfKUnit -
      Setting.FToUnit.FPowerOfKUnit);
end;

function LiteONUnitToMB(Size: UInt64): UInt64;
const
  LiteONUnitInMB = 64;
begin
  result := Size * LiteONUnitInMB;
end;

function MBToLiteONUnit(Size: UInt64): UInt64;
const
  LiteONUnitInMB = 64;
begin
  result := Size div LiteONUnitInMB;
end;

function GetSizeUnitString
  (SizeInMB: Double;
   UnitToTest: TDatasizeUnit;
   Setting: TFormatSizeSetting): String;
const
  Threshold = 0.75;
var
  DivideUnitSize: Integer;
  UnitSizeInMB: Int64;
begin
  DivideUnitSize := GetDivideUnitSize(Setting.FNumeralSystem);
  UnitSizeInMB :=
    Floor(Power(DivideUnitSize, UnitToTest.FPowerOfKUnit - 1));

  if SizeInMB >
    (UnitSizeInMB * Threshold) then
  begin
    result :=
      Format('%.' + IntToStr(Setting.FPrecision) + 'f' + UnitToTest.FUnitName,
        [SizeInMB / UnitSizeInMB]);
  end;
end;

function FormatSizeInMB(SizeInMB: Double; Setting: TFormatSizeSetting): String;
begin
  result := GetSizeUnitString(SizeInMB, PetaUnit, Setting);
  if result <> '' then exit;

  result := GetSizeUnitString(SizeInMB, TeraUnit, Setting);
  if result <> '' then exit;

  result := GetSizeUnitString(SizeInMB, GigaUnit, Setting);
  if result <> '' then exit;

  result := GetSizeUnitString(SizeInMB, MegaUnit, Setting);
  if result = '' then
    result := '0MB';
end;

end.
