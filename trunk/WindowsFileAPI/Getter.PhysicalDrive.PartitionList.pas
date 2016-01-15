unit Getter.PhysicalDrive.PartitionList;

interface

uses
  Windows, SysUtils, Generics.Collections,
  OSFile, Getter.DriveList, Getter.DriveList.Fixed, PartitionExtentGetter;

type
  TPartitionEntry = record
    Letter: String;
    StartingOffset: TLargeInteger;
  end;

  TPartitionList = TList<TPartitionEntry>;

  TGetter.PhysicalDrive.PartitionList = class sealed(TOSFile)
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

function TGetter.PhysicalDrive.PartitionList.GetFixedDrives: TDriveList;
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

function TGetter.PhysicalDrive.PartitionList.IsThisExtentDriveOfThisDriveNumber
  (CurrentExtent: Cardinal): Boolean;
begin
  result :=
    PartitionExtentList[CurrentExtent].DriveNumber =
    PhysicalDriveNumber;
end;

function TGetter.PhysicalDrive.PartitionList.FindPhysicalDriveNumberInPartitionExtentEntry:
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

procedure TGetter.PhysicalDrive.PartitionList.AddThisDriveToList
  (CurrentDrive: Integer; PartitionExtentPosition: Cardinal);
var
  PartitionToAdd: TPartitionEntry;
begin
  PartitionToAdd.Letter := FixedDriveList[CurrentDrive];
  PartitionToAdd.StartingOffset :=
    PartitionExtentList[PartitionExtentPosition].StartingOffset;
  PartitionList.Add(PartitionToAdd);
end;

function TGetter.PhysicalDrive.PartitionList.DeleteLastBackslash
  (Source: String): String;
begin
  result := Source;
  if Source[Length(Source)] = '\' then
    result := Copy(result, 1, Length(result) - 1);
end;

function TGetter.PhysicalDrive.PartitionList.PreparePhysicalDriveNumberQuery
  (CurrentDrive: Integer): Boolean;
begin
  PartitionExtentGetter := TPartitionExtentGetter.Create
    (DeleteLastBackslash
      (ThisComputerPrefix + FixedDriveList[CurrentDrive]));
  PartitionExtentList := PartitionExtentGetter.GetPartitionExtentList;
  result := PartitionExtentList <> nil;
end;

procedure TGetter.PhysicalDrive.PartitionList.TryIfPartitionOfThisDriveAddToList
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

procedure TGetter.PhysicalDrive.PartitionList.IfPartitionOfThisDriveAddToList
  (CurrentDrive: Integer);
begin
  try
    TryIfPartitionOfThisDriveAddToList(CurrentDrive);
  finally
    FreeAndNil(PartitionExtentList);
    FreeAndNil(PartitionExtentGetter);
  end;
end;

function TGetter.PhysicalDrive.PartitionList.CreateAndReturnThisDrivesPartitionList:
  TPartitionList;
var
  CurrentDrive: Integer;
begin
  PartitionList := TPartitionList.Create;
  for CurrentDrive := 0 to (FixedDriveList.Count - 1) do
    IfPartitionOfThisDriveAddToList(CurrentDrive);
  result := PartitionList;
end;

function TGetter.PhysicalDrive.PartitionList.TryToGetPartitionList: TPartitionList;
begin
  FixedDriveList := GetFixedDrives;
  PhysicalDriveNumber := StrToInt(GetPathOfFileAccessingWithoutPrefix);
  result := CreateAndReturnThisDrivesPartitionList;
end;

function TGetter.PhysicalDrive.PartitionList.TryAndIfFailReturnNil: TPartitionList;
begin
  try
    result := TryToGetPartitionList;
  except
    result := nil;
  end;
end;

function TGetter.PhysicalDrive.PartitionList.GetPartitionList: TPartitionList;
begin
  try
    result := TryAndIfFailReturnNil;
  finally
    FreeAndNil(FixedDriveList);
  end;
end;

end.
