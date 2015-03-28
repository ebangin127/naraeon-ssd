unit uMixedPhysicalDriveListGetter;

interface

uses
  SysUtils,
  uPhysicalDriveGetter, uBruteForcePhysicalDriveListGetter,
  uWMIPhysicalDriveListGetter, uPhysicalDriveList;

type
  TMixedPhysicalDriveListGetter = class(TPhysicalDriveGetter)
  public
    function GetPhysicalDriveList: TPhysicalDriveList; override;
  private
    PhysicalDriveList: TPhysicalDriveList;
    function IsMoreTrialNeeded: Boolean;
    procedure LetBruteForceGetterFillTheList;
    procedure LetBruteForceGetterFillTheListIfNeeded;
    procedure LetWMIGetterFillTheList;
    procedure LetWMIGetterFillTheListIfNeeded;
  end;

implementation

{ TMixedPhysicalDriveListGetter }
function TMixedPhysicalDriveListGetter.IsMoreTrialNeeded: Boolean;
begin
  result := 
    (PhysicalDriveList = nil) or
    (PhysicalDriveList.Count = 0);
end;

procedure TMixedPhysicalDriveListGetter.LetBruteForceGetterFillTheList;
var
  BruteForcePhysicalDriveListGetter: TBruteForcePhysicalDriveListGetter;
begin
  BruteForcePhysicalDriveListGetter := TBruteForcePhysicalDriveListGetter.Create;
  PhysicalDriveList :=
    BruteForcePhysicalDriveListGetter.GetPhysicalDriveList;
  FreeAndNil(BruteForcePhysicalDriveListGetter);
end;

procedure TMixedPhysicalDriveListGetter.LetWMIGetterFillTheList;
var
  WMIPhysicalDriveListGetter: TWMIPhysicalDriveListGetter;
begin
  WMIPhysicalDriveListGetter := TWMIPhysicalDriveListGetter.Create;
  PhysicalDriveList :=
    WMIPhysicalDriveListGetter.GetPhysicalDriveList;
  FreeAndNil(WMIPhysicalDriveListGetter);
end;

procedure TMixedPhysicalDriveListGetter.LetBruteForceGetterFillTheListIfNeeded;
begin
  if IsMoreTrialNeeded = false then
    exit;
  if PhysicalDriveList <> nil then
    FreeAndNil(PhysicalDriveList);
  LetBruteForceGetterFillTheList;
end;

procedure TMixedPhysicalDriveListGetter.LetWMIGetterFillTheListIfNeeded;
begin
  if IsMoreTrialNeeded = false then
    exit;
  if PhysicalDriveList <> nil then
    FreeAndNil(PhysicalDriveList);
  LetWMIGetterFillTheList;
end;

function TMixedPhysicalDriveListGetter.GetPhysicalDriveList:
  TPhysicalDriveList;
begin
  LetWMIGetterFillTheListIfNeeded;
  LetBruteForceGetterFillTheListIfNeeded;
  result := PhysicalDriveList;
end;

end.