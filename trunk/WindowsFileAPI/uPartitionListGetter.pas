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

  TPhysicalDriveNumberQueryResult = record
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
    PhysicalDriveNumber: Cardinal;

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

function TPartitionListGetter.FindPhysicalDriveNumberInMotherDriveEntry:
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
  PhysicalDriveNumberQueryResult: TPhysicalDriveNumberQueryResult;
begin
  try
    MotherDriveGetter := TMotherDriveGetter.Create
      (DeleteLastBackslash
        (ThisComputerPrefix + FixedDriveList[CurrentDrive]));
    MotherDriveList := MotherDriveGetter.GetMotherDriveList;

    PhysicalDriveNumberQueryResult := FindPhysicalDriveNumberInMotherDriveEntry;
    if PhysicalDriveNumberQueryResult.Found then
      AddThisDriveToList(CurrentDrive, PhysicalDriveNumberQueryResult.Position);
  finally
    FreeAndNil(MotherDriveList);
    FreeAndNil(MotherDriveGetter);
  end;
end;

function TPartitionListGetter.CreateAndReturnThisDrivesPartitionList:
  TPartitionList;
var
  CurrentDrive: Integer;
begin
  PartitionList := TPartitionList.Create;
  for CurrentDrive := 0 to (FixedDriveList.Count - 1) do
    IfPartitionOfThisDriveAddToList(CurrentDrive);
  result := PartitionList;
end;

function TPartitionListGetter.TryToGetPartitionList: TPartitionList;
begin
  FixedDriveList := GetFixedDrives;
  PhysicalDriveNumber := StrToInt(GetPathOfFileAccessingWithoutPrefix);
  result := CreateAndReturnThisDrivesPartitionList;
end;

function TPartitionListGetter.GetPartitionList: TPartitionList;
begin
  try
    result := TryToGetPartitionList;
  finally
    FreeAndNil(FixedDriveList);
    result := nil;
  end;
end;

end.
