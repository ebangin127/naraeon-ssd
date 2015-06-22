unit uVolumeLabelGetter;

interface

uses
  uOSFile;

type
  TVolumeLabelGetter = class(TOSFile)
  private
    type
      TNTBSVolumeName: Array[0..MAX_PATH] of Char;
  private
    VolumeLabelInNTBS: TNTBSVolumeName;
    procedure SetVolumeLabelInNTBS;
    procedure IfVolumeLabelIsNullUseAlternative(AlternativeName: String);
    function GetSizeOfDiskInMB: Double;
    procedure AppendSizeOfDiskInMB(AlternativeName: String): String;
  public
    function GetVolumeLabel(AlternativeName: String): String;
    procedure PathListToVolumeLabel(PathList: TString;
      AlternativeName: String); static;
  end;

implementation

function TVolumeLabelGetter.GetSizeOfDiskInMB: Double;
const
  ByteToMega: TDatasizeUnitChangeSetting =
    (FNumeralSystem: Denary; FFromUnit: ByteUnit; FToUnit: MegaUnit);
  VolumeNames = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
var
  SizeOfDiskInByte: Int64;
begin
  SizeOfDiskInByte := DiskSize(Pos(GetPathOfFileAccessing[1], VolumeNames));
  exit(ChangeDatasizeUnit(SizeOfDiskInByte, ByteToMega));
end;

procedure TVolumeLabelGetter.SetVolumeLabelInNTBS;
var
  MaximumComponentLength: DWORD;
  VolumeFlags: DWORD;
  VolumeSerialNumber: DWORD;
begin
  ZeroMemory(@VolumeLabelInNTBS, SizeOf(VolumeLabelInNTBS));
  GetVolumeInformation(PChar(DrivePath), VolumeLabelInNTBS,
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

procedure TVolumeLabelGetter.AppendSizeOfDiskInMB(
  AlternativeName: String): String;
const
  DenaryInteger: TFormatSizeSetting =
    (FNumeralSystem: Denary; FPrecision: 0);
var
  SizeOfDiskInMB: Double;
begin
  SizeOfDiskInMB := GetSizeOfDiskInMB(GetPathOfFileAccessing);
  result :=
    GetPathOfFileAccessing + ' (' + VolumeLabelInNTBS + ' - ' +
      FormatSizeInMB(SizeOfDiskInMB, DenaryInteger) + ')';
end;

function TVolumeLabelGetter.GetVolumeLabel(AlternativeName: String): String;
begin
  SetVolumeLabelInNTBS;
  IfVolumeLabelIsNullUseAlternative(AlternativeName);
  exit(AppendSizeOfDiskInMB);
end;

procedure TVolumeLabelGetter.PathListToVolumeLabel(PathList: TString;
  AlternativeName: String); static;
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

