unit uVolumeLabelGetter;

interface

uses
  Classes, Windows, SysUtils,
  MeasureUnit.DataSize, OSFile;

type
  TVolumeLabelGetter = class(TOSFile)
  private
    type
      TNTBSVolumeName = Array[0..MAX_PATH] of Char;
  private
    VolumeLabelInNTBS: TNTBSVolumeName;
    procedure SetVolumeLabelInNTBS;
    procedure IfVolumeLabelIsNullUseAlternative(AlternativeName: String);
    function GetSizeOfDiskInMB: Double;
    function AppendSizeOfDiskInMB(AlternativeName: String): String;
    function GetByteToMega: TDatasizeUnitChangeSetting;
  public
    function GetVolumeLabel(AlternativeName: String): String;
  end;

procedure PathListToVolumeLabel(PathList: TStrings;
  AlternativeName: String);

implementation

function TVolumeLabelGetter.GetByteToMega: TDatasizeUnitChangeSetting;
begin
  result.FNumeralSystem := Denary;
  result.FFromUnit := ByteUnit;
  result.FToUnit := MegaUnit;
end;

function TVolumeLabelGetter.GetSizeOfDiskInMB: Double;
const
  VolumeNames = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
var
  SizeOfDiskInByte: Int64;
begin
  SizeOfDiskInByte := DiskSize(Pos(GetPathOfFileAccessing[1], VolumeNames));
  exit(ChangeDatasizeUnit(SizeOfDiskInByte, GetByteToMega));
end;

procedure TVolumeLabelGetter.SetVolumeLabelInNTBS;
var
  MaximumComponentLength: DWORD;
  VolumeFlags: DWORD;
  VolumeSerialNumber: DWORD;
begin
  ZeroMemory(@VolumeLabelInNTBS, SizeOf(VolumeLabelInNTBS));
  GetVolumeInformation(PChar(GetPathOfFileAccessing), VolumeLabelInNTBS,
    SizeOf(VolumeLabelInNTBS), @VolumeSerialNumber, MaximumComponentLength,
    VolumeFlags, nil, 0);
end;

procedure TVolumeLabelGetter.IfVolumeLabelIsNullUseAlternative(
  AlternativeName: String);
begin
  if VolumeLabelInNTBS[0] = #0 then
    CopyMemory(@VolumeLabelInNTBS, @AlternativeName[1],
      Length(AlternativeName) * SizeOf(Char));
end;

function TVolumeLabelGetter.AppendSizeOfDiskInMB(
  AlternativeName: String): String;
const
  DenaryInteger: TFormatSizeSetting =
    (FNumeralSystem: Denary; FPrecision: 0);
var
  SizeOfDiskInMB: Double;
begin
  SizeOfDiskInMB := GetSizeOfDiskInMB;
  result :=
    GetPathOfFileAccessing + ' (' + VolumeLabelInNTBS + ' - ' +
      FormatSizeInMB(SizeOfDiskInMB, DenaryInteger) + ')';
end;

function TVolumeLabelGetter.GetVolumeLabel(AlternativeName: String): String;
begin
  SetVolumeLabelInNTBS;
  IfVolumeLabelIsNullUseAlternative(AlternativeName);
  exit(AppendSizeOfDiskInMB(AlternativeName));
end;

procedure PathListToVolumeLabel(PathList: TStrings;
  AlternativeName: String);
var
  VolumeLabelGetter: TVolumeLabelGetter;
  CurrentPath: Integer;
begin
  for CurrentPath := 0 to PathList.Count - 1 do
  begin
    VolumeLabelGetter := TVolumeLabelGetter.Create(PathList[CurrentPath]);
    PathList[CurrentPath] := VolumeLabelGetter.GetVolumeLabel(AlternativeName);
    FreeAndNil(VolumeLabelGetter);
  end;
end;
end.

