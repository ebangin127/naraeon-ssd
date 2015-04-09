unit uDatasizeUnit;

interface

uses
  Math;

type
  NumeralSystem = (Denary, Binary);

  DatasizeUnit = record
    FPowerOfKUnit: Integer;
    FUnitName: String;
  end;

  DatasizeUnitChangeSetting = record
    FNumeralSystem: NumeralSystem;
    FFromUnit: DatasizeUnit;
    FToUnit: DatasizeUnit;
  end;

  NumeralSystemChangeSetting = record
    FFromNumeralSystem: NumeralSystem;
    FToNumeralSystem: NumeralSystem;
  end;

const
  PetaUnit: DatasizeUnit = (FPowerOfKUnit: 4; FUnitName: 'PB');
  TeraUnit: DatasizeUnit = (FPowerOfKUnit: 3; FUnitName: 'TB');
  GigaUnit: DatasizeUnit = (FPowerOfKUnit: 2; FUnitName: 'GB');
  MegaUnit: DatasizeUnit = (FPowerOfKUnit: 1; FUnitName: 'MB');
  KiloUnit: DatasizeUnit = (FPowerOfKUnit: 0; FUnitName: 'KB');
  ByteUnit: DatasizeUnit = (FPowerOfKUnit: -1; FUnitName: 'B');

function GetDevideUnitSize(DivideUnitType: NumeralSystem): Integer;
function ChangeDatasizeUnit
  (Size: Double; Setting: DatasizeUnitChangeSetting): Double;
function LiteONUnitToMB(Size: UInt64): UInt64;
function MBToLiteONUnit(Size: UInt64): UInt64;

implementation

function GetDevideUnitSize(DivideUnitType: NumeralSystem): Integer;
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
  (Size: Double; Setting: DatasizeUnitChangeSetting): Double;
begin
  result := Size *
    Power(GetDevideUnitSize(Setting.FNumeralSystem),
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
end.
