unit uPartitionListGetter;

interface

uses
  Windows, SysUtils, Generics.Collections,
  uOSFile, uFixedDriveListGetter, uMotherDriveGetter;

type
  TPartitionEntry = record
    Letter: String;
    StartingOffset: TLargeInteger;
  end;

  TDriveNumberQueryResult = record
    Found: Boolean;
    Position: Cardinal;
  end;

  TPartitionList = TList<TPartitionEntry>;

  TPartitionListGetter = class sealed(TOSFile)
  public
    function GetPartitionList: TPartitionList;

  private
    PartitionList: TPartitionList;
    MotherDriveList: TMotherDriveList;
    FixedDriveList: TFixedDriveList;
    DriveNumber: Cardinal;

    function GetFixedDrives: TFixedDriveList;
    procedure AddThisDriveToList
      (CurrentDrive: Integer; MotherDrivePosition: Cardinal);
    function FindDiskNumberInMotherDriveEntry: TDriveNumberQueryResult;
    procedure IfPartitionOfThisDriveAddToList(CurrentDrive: Integer);
    function IsThisExtentDriveOfThisDriveNumber(
      CurrentExtent: Cardinal): Boolean;
    function DeleteLastBackslash(Source: String): String;
  end;

implementation

function TPartitionListGetter.GetFixedDrives: TFixedDriveList;
var
  FixedDriveListGetter: TFixedDriveListGetter;
begin
  try
    FixedDriveListGetter := TFixedDriveListGetter.Create
      (GetPathOfFileAccessing);
    result := FixedDriveListGetter.GetFixedDriveList;
  finally
    FreeAndNil(FixedDriveListGetter);
  end;
end;

function TPartitionListGetter.IsThisExtentDriveOfThisDriveNumber
  (CurrentExtent: Cardinal): Boolean;
begin
  result := MotherDriveList[CurrentExtent].DriveNumber = DriveNumber;
end;

function TPartitionListGetter.FindDiskNumberInMotherDriveEntry:
  TDriveNumberQueryResult;
var
  CurrentExtent: Cardinal;
begin
  result.Found := false;
  for CurrentExtent := 0 to (MotherDriveList.Count - 1) do
  begin
    if IsThisExtentDriveOfThisDriveNumber(CurrentExtent) then
    begin
      result.Found := true;
      result.Position := CurrentExtent;
      exit(result);
    end;
  end;
end;

procedure TPartitionListGetter.AddThisDriveToList
  (CurrentDrive: Integer; MotherDrivePosition: Cardinal);
var
  PartitionToAdd: TPartitionEntry;
begin
  PartitionToAdd.Letter := FixedDriveList[CurrentDrive];
  PartitionToAdd.StartingOffset :=
    MotherDriveList[MotherDrivePosition].StartingOffset;
  PartitionList.Add(PartitionToAdd);
end;

function TPartitionListGetter.DeleteLastBackslash
  (Source: String): String;
begin
  result := Source;
  if Source[Length(Source)] = '\' then
    result := Copy(result, 1, Length(result) - 1);
end;

procedure TPartitionListGetter.IfPartitionOfThisDriveAddToList
  (CurrentDrive: Integer);
var
  MotherDriveGetter: TMotherDriveGetter;
  DriveNumberQueryResult: TDriveNumberQueryResult;
begin
  try
    MotherDriveGetter := TMotherDriveGetter.Create
      (DeleteLastBackslash
        (ThisComputerPrefix + FixedDriveList[CurrentDrive]));
    MotherDriveList := MotherDriveGetter.GetMotherDriveList;

    DriveNumberQueryResult := FindDiskNumberInMotherDriveEntry;
    if DriveNumberQueryResult.Found then
      AddThisDriveToList(CurrentDrive, DriveNumberQueryResult.Position);
  finally
    FreeAndNil(MotherDriveList);
    FreeAndNil(MotherDriveGetter);
  end;
end;

function TPartitionListGetter.GetPartitionList: TPartitionList;
var
  CurrentDrive: Integer;
begin
  try
    FixedDriveList := GetFixedDrives;
    DriveNumber := StrToInt(GetPathOfFileAccessingWithoutPrefix);
    PartitionList := TPartitionList.Create;

    for CurrentDrive := 0 to (FixedDriveList.Count - 1) do
      IfPartitionOfThisDriveAddToList(CurrentDrive);

    result := PartitionList;
  finally
    FreeAndNil(FixedDriveList);
  end;
end;

end.
