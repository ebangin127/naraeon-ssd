unit Getter.PhysicalDriveList.BruteForce;

interface

uses
  SysUtils, Threading,
  OSFile, Device.PhysicalDrive, Getter.PhysicalDriveList,
  Device.PhysicalDrive.List, CommandSet.Factory;

type
  TBruteForcePhysicalDriveListGetter = class sealed(TPhysicalDriveListGetter)
  public
    function GetPhysicalDriveList: TPhysicalDriveList; override;
  private
    PhysicalDriveList: TPhysicalDriveList;
    procedure AddDriveToList(CurrentDrive: Integer);
    procedure IfThisDriveAccessibleAddToList(CurrentDrive: Integer);
    function TryToGetIsDriveAccessible(CurrentDrive: Integer): Boolean;
    procedure TryToGetPhysicalDriveList;
    function IsDriveAccessible(CurrentDrive: Integer): Boolean;
  end;

implementation

{ TBruteForcePhysicalDriveGetter }

procedure TBruteForcePhysicalDriveListGetter.AddDriveToList
  (CurrentDrive: Integer);
begin
  try
    PhysicalDriveList.Add(
      TPhysicalDrive.Create(
        TPhysicalDrive.BuildFileAddressByNumber(CurrentDrive)));
  except
    on E: ENoCommandSetException do
    else raise;
  end;
end;

function TBruteForcePhysicalDriveListGetter.TryToGetIsDriveAccessible
  (CurrentDrive: Integer): Boolean;
var
  PhysicalDrive: IPhysicalDrive;
begin
  PhysicalDrive :=
    TPhysicalDrive.Create(
      TPhysicalDrive.BuildFileAddressByNumber(CurrentDrive));
  result := PhysicalDrive.IsDriveAvailable;
end;

function TBruteForcePhysicalDriveListGetter.IsDriveAccessible
  (CurrentDrive: Integer): Boolean;
begin
  try
    result := TryToGetIsDriveAccessible(CurrentDrive);
  except
    result := false;
  end;
end;

procedure TBruteForcePhysicalDriveListGetter.IfThisDriveAccessibleAddToList
  (CurrentDrive: Integer);
begin
  if IsDriveAccessible(CurrentDrive) then
    AddDriveToList(CurrentDrive);
end;

procedure TBruteForcePhysicalDriveListGetter.TryToGetPhysicalDriveList;
const
  PHYSICALDRIVE_MAX = 99;
begin
  TParallel.For(0, PHYSICALDRIVE_MAX, procedure (CurrentDrive: Integer)
  begin
    IfThisDriveAccessibleAddToList(CurrentDrive)
  end);
end;

function TBruteForcePhysicalDriveListGetter.GetPhysicalDriveList:
  TPhysicalDriveList;
begin
  PhysicalDriveList := TPhysicalDriveList.Create;
  try
    TryToGetPhysicalDriveList;
  except
    PhysicalDriveList.Clear;
  end;
  result := PhysicalDriveList;
end;

end.
