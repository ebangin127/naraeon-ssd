unit Getter.PhysicalDriveList.OS.Path;

interface

uses
  SysUtils, Windows, Generics.Collections;

type
  TDrivePathNumberList = class(TList<Integer>);
  TOSPhysicalDrivePathGetter = class
  public
    function GetPhysicalDriveNames: TDrivePathNumberList;
  private
    function IsPhysicalDrive(const Path: PChar): Boolean;
    function ParseSeparatedName(const Path: PChar): Integer;
    type
      TVolumeNameBuffer = Array[0..65535] of Char;
      PTVolumeNameBuffer  = ^TVolumeNameBuffer;
    function GetAllPath: PTVolumeNameBuffer;
    function ParseVolumeNameIntoList(
      const AllDrives: PTVolumeNameBuffer): TDrivePathNumberList;
  end;

implementation

{ TOSPhysicalDrivePathGetter }

function TOSPhysicalDrivePathGetter.GetAllPath: PTVolumeNameBuffer;
begin
  GetMem(result, SizeOf(TVolumeNameBuffer));
  ZeroMemory(result, SizeOf(result));
  QueryDosDevice(nil, result^, SizeOf(result^));
end;

function TOSPhysicalDrivePathGetter.IsPhysicalDrive(const Path: PChar):
  Boolean;
const
  PhysicalDrivePre = 'PHYSICALDRIVE';
begin
  result := UpperCase(Copy(Path, 1, Length(PhysicalDrivePre))) =
     PhysicalDrivePre;
end;

function TOSPhysicalDrivePathGetter.ParseSeparatedName(const Path: PChar):
  Integer;
const
  PhysicalDrivePre = 'PHYSICALDRIVE';
begin
  result :=
    StrToInt(Copy(Path, Length(PhysicalDrivePre) + 1,
      Length(Path) - Length(PhysicalDrivePre)));
end;

function TOSPhysicalDrivePathGetter.ParseVolumeNameIntoList(
  const AllDrives: PTVolumeNameBuffer): TDrivePathNumberList;
var
  CurrentCharIndex: Integer;
  CurrentName: PChar;
begin
  CurrentCharIndex := 0;
  result := TDrivePathNumberList.Create;
  while CurrentCharIndex < Length(AllDrives^) do
  begin
    CurrentName := @(AllDrives^)[CurrentCharIndex];
    if CurrentName = '' then
      break;
    if IsPhysicalDrive(CurrentName) then
      result.Add(ParseSeparatedName(CurrentName));
    Inc(CurrentCharIndex, Length(CurrentName) + 1);
  end;
end;

function TOSPhysicalDrivePathGetter.GetPhysicalDriveNames: TDrivePathNumberList;
var
  AllDrives: PTVolumeNameBuffer;
begin
  AllDrives := GetAllPath;
  try
    result := ParseVolumeNameIntoList(AllDrives);
  finally
    FreeMem(AllDrives);
  end;
end;

end.
