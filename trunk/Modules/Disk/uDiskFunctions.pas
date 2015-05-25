unit uDiskFunctions;

interface

uses
  Windows, SysUtils, uDatasizeUnit;

function GetVolumeLabel(VolumeName: String; DrivePath: String): string;

const
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

function GetVolumeLabel(VolumeName: String; DrivePath: String): string;
const
  VolumeLabelSetting: TFormatSizeSetting =
    (FNumeralSystem: Denary; FPrecision: 0);
var
  NotUsed: DWORD;
  VolumeFlags: DWORD;
  VolumeSerialNumber: DWORD;
  Buf: array [0..MAX_PATH] of Char;
  SizeOfDiskInMB: Double;
begin
  FillMemory(@Buf, Length(Buf) * SizeOf(Char), 0);
  GetVolumeInformation(PChar(DrivePath), Buf, SizeOf(Buf), @VolumeSerialNumber,
    NotUsed, VolumeFlags, nil, 0);

  if Buf[0] = #0 then
    CopyMemory(@Buf, @VolumeName[1], Length(VolumeName) * SizeOf(Char));

  SizeOfDiskInMB := GetSizeOfDiskInMB(DrivePath);
  Result :=
    DrivePath + ' (' + Buf + ' - ' +
      FormatSizeInMB(SizeOfDiskInMB, VolumeLabelSetting) + ')';
end;

end.

