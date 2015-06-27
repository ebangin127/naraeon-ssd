unit uPartitionListGetter;

interface

uses
  Windows, SysUtils, Generics.Collections,
  uOSFile, uDriveListGetter, uFixedDriveListGetter, uPartitionExtentGetter;

type
  TPartitionEntry = record
    Letter: String;
    StartingOffset: TLargeInteger;
  end;

  TPartitionList = TList<TPartitionEntry>;

  TPartitionListGetter = class sealed(TOSFile)
  public
    function GetPartitionList: TPartitionList;

  private
    type
      TPhysicalDriveNumberQueryResult = record
        Found: Boolean;
        Position: Cardinal;
      end;
      
  private
    PartitionList: TPartitionList;
    PartitionExtentList: TPartitionExtentList;
    FixedDriveList: TDriveList;
    PhysicalDriveNumber: Cardinal;
    PartitionExtentGetter: TPartitionExtentGetter;

    function GetFixedDrives: TDriveList;
    procedure AddThisDriveToList
      (CurrentDrive: Integer; PartitionExtentPosition: Cardinal);
    procedure IfPartitionOfThisDriveAddToList(CurrentDrive: Integer);
    function IsThisExtentDriveOfThisDriveNumber(
      CurrentExtent: Cardinal): Boolean;
    function DeleteLastBackslash(Source: String): String;
    function CreateAndReturnThisDrivesPartitionList: TPartitionList;
    function TryToGetPartitionList: TPartitionList;
    function FindPhysicalDriveNumberInPartitionExtentEntry:
      TPhysicalDriveNumberQueryResult;
    procedure TryIfPartitionOfThisDriveAddToList(CurrentDrive: Integer);
    function TryAndIfFailReturnNil: TPartitionList;
    function PreparePhysicalDriveNumberQuery(CurrentDrive: Integer): Boolean;
  end;

implementation

function TPartitionListGetter.GetFixedDrives: TDriveList;
var
  FixedDriveListGetter: TFixedDriveListGetter;
begin
  try
    FixedDriveListGetter := TFixedDriveListGetter.Create
      (GetPathOfFileAccessing);
    result := FixedDriveListGetter.GetDriveList;
  finally
    FreeAndNil(FixedDriveListGetter);
  end;
end;

function TPartitionListGetter.IsThisExtentDriveOfThisDriveNumber
  (CurrentExtent: Cardinal): Boolean;
begin
  result :=
    PartitionExtentList[CurrentExtent].DriveNumber =
    PhysicalDriveNumber;
end;

function TPartitionListGetter.FindPhysicalDriveNumberInPartitionExtentEntry:
  TPhysicalDriveNumberQueryResult;
var
  CurrentExtent: Cardinal;
begin
  result.Found := false;
  for CurrentExtent := 0 to (PartitionExtentList.Count - 1) do
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
  (CurrentDrive: Integer; PartitionExtentPosition: Cardinal);
var
  PartitionToAdd: TPartitionEntry;
begin
  PartitionToAdd.Letter := FixedDriveList[CurrentDrive];
  PartitionToAdd.StartingOffset :=
    PartitionExtentList[PartitionExtentPosition].StartingOffset;
  PartitionList.Add(PartitionToAdd);
end;

function TPartitionListGetter.DeleteLastBackslash
  (Source: String): String;
begin
  result := Source;
  if Source[Length(Source)] = '\' then
    result := Copy(result, 1, Length(result) - 1);
end;

function TPartitionListGetter.PreparePhysicalDriveNumberQuery
  (CurrentDrive: Integer): Boolean;
begin
  PartitionExtentGetter := TPartitionExtentGetter.Create
    (DeleteLastBackslash
      (ThisComputerPrefix + FixedDriveList[CurrentDrive]));
  PartitionExtentList := PartitionExtentGetter.GetPartitionExtentList;
  result := PartitionExtentList <> nil;
end;

procedure TPartitionListGetter.TryIfPartitionOfThisDriveAddToList
  (CurrentDrive: Integer);
var
  PhysicalDriveNumberQueryResult: TPhysicalDriveNumberQueryResult;
begin
  if not PreparePhysicalDriveNumberQuery(CurrentDrive) then
    exit;
  PhysicalDriveNumberQueryResult :=
    FindPhysicalDriveNumberInPartitionExtentEntry;
  if PhysicalDriveNumberQueryResult.Found then
    AddThisDriveToList(CurrentDrive, PhysicalDriveNumberQueryResult.Position);
end;

procedure TPartitionListGetter.IfPartitionOfThisDriveAddToList
  (CurrentDrive: Integer);
begin
  try
    TryIfPartitionOfThisDriveAddToList(CurrentDrive);
  finally
    FreeAndNil(PartitionExtentList);
    FreeAndNil(PartitionExtentGetter);
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
