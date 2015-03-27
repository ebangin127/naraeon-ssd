unit uBruteForcePhysicalDriveGetter;

interface

uses
  uOSFile, uPhysicalDrive, uPhysicalDriveGetter, uPhysicalDriveList;

type
  TBruteForcePhysicalDriveGetter = class(TPhysicalDriveGetter)
  public
    function GetPhysicalDriveList: TPhysicalDriveList; override;
  end;

implementation

{ TBruteForcePhysicalDriveGetter }

procedure TBruteForcePhysicalDriveGetter.AddDriveToList
  (CurrentDrive: Integer);
var
  EntryToAdd: TPhysicalDriveEntry;
begin
  CurrEntry.DeviceName := IntToStr(CurrentDrive);
  CurrEntry.IsUSBDevice := false;
  PhysicalDriveList.Add(EntryToAdd);
end;

function TBruteForcePhysicalDriveGetter.IsDriveAccessible
  (CurrentDrive: Integer): Boolean;
var
  PhysicalDrive: TPhysicalDrive;
begin
  PhysicalDrive := TPhysicalDrive.Create(CurrentDrive);
  result := PhysicalDrive.GetIsDriveAvailable;
  FreeAndNil(PhysicalDrive);
end;

procedure TBruteForcePhysicalDriveGetter.IfThisDriveAccessibleAddToList
  (CurrentDrive: Integer);
var
  TryingDriveName: String;
begin
  TryingDriveName := BuildDriveNameWithDriveNumber(CurrentDrive);
  if IsDriveAccessible(TryingDriveName) then
    AddDriveToList(CurrentDrive);
end;

function TBruteForcePhysicalDriveGetter.BuildDriveNameWithDriveNumber
  (CurrentDrive: Integer): String;
begin
  result := 
    ThisComputerPrefix +
    PhysicalDrivePrefix +
    IntToStr(CurrentDrive);
end;

function TBruteForcePhysicalDriveGetter.TryToGetPhysicalDriveList:
  TPhysicalDriveList;
const
  PHYSICALDRIVE_MAX = 99;
var
  TryingDriveName: String;
  CurrentDrive: Integer;
begin
  for CurrentDrive := 0 to PHYSICALDRIVE_MAX do
    IfThisDriveAccessibleAddToList(CurrentDrive);
end;

function TBruteForcePhysicalDriveGetter.GetPhysicalDriveList:
  TPhysicalDriveList;
begin
  try
    PhysicalDriveList := TPhysicalDriveList.Create;
    TryToGetPhysicalDriveList;
  except
    FreeAndNil(PhysicalDriveList);
  end;
  result := PhysicalDriveList;
end;

end.
