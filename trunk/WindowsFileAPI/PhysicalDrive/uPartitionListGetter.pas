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
    MotherDriveGetter: TMotherDriveGetter;

    function GetFixedDrives: TFixedDriveList;
    procedure AddThisDriveToList
      (CurrentDrive: Integer; MotherDrivePosition: Cardinal);
    procedure IfPartitionOfThisDriveAddToList(CurrentDrive: Integer);
    function IsThisExtentDriveOfThisDriveNumber(
      CurrentExtent: Cardinal): Boolean;
    function DeleteLastBackslash(Source: String): String;
    function CreateAndReturnThisDrivesPartitionList: TPartitionList;
    function TryToGetPartitionList: TPartitionList;
    function FindPhysicalDriveNumberInMotherDriveEntry:
      TPhysicalDriveNumberQueryResult;
    procedure TryIfPartitionOfThisDriveAddToList(CurrentDrive: Integer);
    function TryAndIfFailReturnNil: TPartitionList;
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
  result :=
    MotherDriveList[CurrentExtent].DriveNumber =
    PhysicalDriveNumber;
end;

function TPartitionListGetter.FindPhysicalDriveNumberInMotherDriveEntry:
  TPhysicalDriveNumberQueryResult;
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

procedure TPartitionListGetter.TryIfPartitionOfThisDriveAddToList
  (CurrentDrive: Integer);
var
  PhysicalDriveNumberQueryResult: TPhysicalDriveNumberQueryResult;
begin
  MotherDriveGetter := TMotherDriveGetter.Create
    (DeleteLastBackslash
      (ThisComputerPrefix + FixedDriveList[CurrentDrive]));
  MotherDriveList := MotherDriveGetter.GetMotherDriveList;

  PhysicalDriveNumberQueryResult := FindPhysicalDriveNumberInMotherDriveEntry;
  if PhysicalDriveNumberQueryResult.Found then
    AddThisDriveToList(CurrentDrive, PhysicalDriveNumberQueryResult.Position);
end;

procedure TPartitionListGetter.IfPartitionOfThisDriveAddToList
  (CurrentDrive: Integer);
begin
  try
    TryIfPartitionOfThisDriveAddToList(CurrentDrive);
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

function TPartitionListGetter.TryAndIfFailReturnNil: TPartitionList;
begin
  try
    result := TryToGetPartitionList;
  except
    result := nil;
  end;
end;

function TPartitionListGetter.GetPartitionList: TPartitionList;
begin
  try
    result := TryAndIfFailReturnNil;
  finally
    FreeAndNil(FixedDriveList);
  end;
end;

end.
