unit uBruteForcePhysicalDriveListGetter;

interface

uses
  SysUtils,
  uOSFile, uPhysicalDrive, uPhysicalDriveGetter, uPhysicalDriveList;

type
  TBruteForcePhysicalDriveListGetter = class(TPhysicalDriveGetter)
  private
    procedure AddDriveToList(CurrentDrive: Integer);
    procedure IfThisDriveAccessibleAddToList(CurrentDrive: Integer);
    function IsDriveAccessible(CurrentDrive: Integer): Boolean;
    procedure TryToGetPhysicalDriveList;
  public
    function GetPhysicalDriveList: TPhysicalDriveList; override;
  end;

implementation

{ TBruteForcePhysicalDriveGetter }

procedure TBruteForcePhysicalDriveListGetter.AddDriveToList
  (CurrentDrive: Integer);
begin
  PhysicalDriveList.Add(TPhysicalDrive.Create(CurrentDrive));
end;

function TBruteForcePhysicalDriveListGetter.IsDriveAccessible
  (CurrentDrive: Integer): Boolean;
var
  PhysicalDrive: TPhysicalDrive;
begin
  PhysicalDrive := TPhysicalDrive.Create(CurrentDrive);
  result := PhysicalDrive.GetIsDriveAvailable;
  FreeAndNil(PhysicalDrive);
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
var
  CurrentDrive: Integer;
begin
  for CurrentDrive := 0 to PHYSICALDRIVE_MAX do
    IfThisDriveAccessibleAddToList(CurrentDrive);
end;

function TBruteForcePhysicalDriveListGetter.GetPhysicalDriveList:
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
