unit uDiskFunctions;

interface

uses
  Windows, SysUtils,
  uStrFunctions, uDatasizeUnit;

function GetVolumeLabel(AltName: String; DriveName: String): string;

const
  SMART_READ_ATTRIBUTE_VALUES = $D0;
  SMART_CYL_LOW = $4F;
  SMART_CYL_HI = $C2;
  SMART_CMD = $B0;

  ATAMode = false;
  SCSIMode = true;

  VolumeNames = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';

implementation

function GetSizeOfDiskInMB(DriveName: String): Double;
var
  SizeOfDiskInByte: Int64;
  ByteToMega: TDatasizeUnitChangeSetting;
begin
  SizeOfDiskInByte := DiskSize(Pos(DriveName[1], VolumeNames));

  ByteToMega.FNumeralSystem := Denary;
  ByteToMega.FFromUnit := ByteUnit;
  ByteToMega.FToUnit := MegaUnit;

  exit(ChangeDatasizeUnit(SizeOfDiskInByte, ByteToMega));
end;

function GetVolumeLabel(AltName: String; DriveName: String): string;
const
  VolumeLabelSetting: FormatSizeSetting =
    (FNumeralSystem: Denary; FPrecision: 0);
var
  NotUsed: DWORD;
  VolumeFlags: DWORD;
  VolumeSerialNumber: DWORD;
  Buf: array [0..MAX_PATH] of Char;
  SizeOfDiskInMB: Double;
begin
  FillMemory(@Buf, Length(Buf) * SizeOf(Char), 0);
  GetVolumeInformation(PChar(DriveName), Buf, SizeOf(Buf), @VolumeSerialNumber,
                        NotUsed, VolumeFlags, nil, 0);

  if Buf[0] = #0 then
    CopyMemory(@Buf, @AltName[1],
               Length(AltName) * SizeOf(Char));

  SizeOfDiskInMB := GetSizeOfDiskInMB(DriveName);
  Result :=
    DriveName + ' (' + Buf + ' - ' +
      FormatSizeInMB(SizeOfDiskInMB, VolumeLabelSetting) + ')';
end;

end.

