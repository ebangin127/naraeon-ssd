unit Getter.PhysicalDriveList.OS;

interface

uses
  Classes, SysUtils, Threading, Dialogs, Generics.Collections,
  Device.PhysicalDrive, Getter.PhysicalDriveList,
  Device.PhysicalDrive.List, CommandSet.Factory,
  Getter.PhysicalDriveList.OS.Path;

type
  TOSPhysicalDriveListGetter = class sealed(TPhysicalDriveListGetter)
  public
    function GetPhysicalDriveList: TPhysicalDriveList; override;
  private
    PhysicalDriveList: TThreadedPhysicalDriveList;
    procedure AddDriveToList(CurrentDrive: Integer);
    procedure IfThisDriveAccessibleAddToList(CurrentDrive: Integer);
    function TryToGetIsDriveAccessible(CurrentDrive: Integer): Boolean;
    procedure TryToGetPhysicalDriveList;
    function IsDriveAccessible(CurrentDrive: Integer): Boolean;
    function LockAndTransfer: TPhysicalDriveList;
    function GetDrivePathList: TDrivePathNumberList;
    {$HINTS OFF} // False warning because each setting uses only one of them
    procedure ParallelGetDriveList(const DrivePathList: TDrivePathNumberList);
    procedure SerialGetDriveList(const DrivePathList: TDrivePathNumberList);
    {$HINTS ON}
  end;

implementation

{ TOSPhysicalDriveListGetter }

procedure TOSPhysicalDriveListGetter.AddDriveToList
  (CurrentDrive: Integer);
begin
  try
    PhysicalDriveList.Add(
      TPhysicalDrive.Create(
        TPhysicalDrive.BuildFileAddressByNumber(CurrentDrive)));
  except
    on E: ENoCommandSetException do;
    on E: ENoNVMeDriverException do;
    on OSError: EOSError do
      if OSError.ErrorCode <> 2 then raise;
    else raise;
  end;
end;

function TOSPhysicalDriveListGetter.TryToGetIsDriveAccessible
  (CurrentDrive: Integer): Boolean;
var
  PhysicalDrive: IPhysicalDrive;
begin
  PhysicalDrive :=
    TPhysicalDrive.Create(
      TPhysicalDrive.BuildFileAddressByNumber(CurrentDrive));
  result := PhysicalDrive.IsDriveAvailable;
end;

function TOSPhysicalDriveListGetter.IsDriveAccessible
  (CurrentDrive: Integer): Boolean;
begin
  try
    result := TryToGetIsDriveAccessible(CurrentDrive);
  except
    result := false;
  end;
end;

procedure TOSPhysicalDriveListGetter.IfThisDriveAccessibleAddToList
  (CurrentDrive: Integer);
begin
  if IsDriveAccessible(CurrentDrive) then
    AddDriveToList(CurrentDrive);
end;

function TOSPhysicalDriveListGetter.GetDrivePathList: TDrivePathNumberList;
var
  PathGetter: TOSPhysicalDrivePathGetter;
begin
  PathGetter := TOSPhysicalDrivePathGetter.Create;
  try
    result := PathGetter.GetPhysicalDriveNames;
  finally
    PathGetter.Free;
  end;
end;

procedure TOSPhysicalDriveListGetter.SerialGetDriveList(
  const DrivePathList: TDrivePathNumberList);
var
  CurrentDrive: Integer;
begin
  for CurrentDrive := 0 to DrivePathList.Count - 1 do
  begin
    try
      IfThisDriveAccessibleAddToList(DrivePathList[CurrentDrive]);
    except
      on E: Exception do
        raise;
    end;
  end;
end;

procedure TOSPhysicalDriveListGetter.ParallelGetDriveList(
  const DrivePathList: TDrivePathNumberList);
begin
  TParallel.For(0, DrivePathList.Count - 1, procedure (CurrentDrive: Integer)
  begin
    try
      IfThisDriveAccessibleAddToList(DrivePathList[CurrentDrive]);
    except
      on E: Exception do
        raise;
    end;
  end);
end;

procedure TOSPhysicalDriveListGetter.TryToGetPhysicalDriveList;
var
  DrivePathList: TDrivePathNumberList;
begin
  DrivePathList := GetDrivePathList;
  try
    {$IFDEF SERVICE}
    SerialGetDriveList(DrivePathList);
    {$ELSE}
    ParallelGetDriveList(DrivePathList);
    {$ENDIF}
  finally
    FreeAndNil(DrivePathList);
  end;
end;

function TOSPhysicalDriveListGetter.LockAndTransfer:
  TPhysicalDriveList;
var
  LockedList: TList<IPhysicalDrive>;
begin
  LockedList := PhysicalDriveList.LockList;
  result := TPhysicalDriveList.Create;
  result.AddRange(LockedList.ToArray);
  PhysicalDriveList.UnlockList;
end;

function TOSPhysicalDriveListGetter.GetPhysicalDriveList:
  TPhysicalDriveList;
begin
  PhysicalDriveList := TThreadedPhysicalDriveList.Create;
  try
    TryToGetPhysicalDriveList;
  except
    PhysicalDriveList.Clear;
  end;
  result := LockAndTransfer;
  FreeAndNil(PhysicalDriveList);
end;

end.
