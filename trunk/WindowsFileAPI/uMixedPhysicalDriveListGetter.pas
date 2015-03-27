unit uMixedPhysicalDriveListGetter;

interface

uses
  uPhysicalDriveGetter, uBruteForcePhysicalDriveGetter,
  uWMIPhysicalDriveListGetter, uPhysicalDriveList;

type
  TMixedPhysicalDriveListGetter = class(TPhysicalDriveGetter)
  public
    function GetPhysicalDriveList: TPhysicalDriveList; override;
  private
    PhysicalDriveList: TPhysicalDriveList;
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
  BruteForcePhysicalDriveGetter: TBruteForcePhysicalDriveGetter;
begin
  BruteForcePhysicalDriveGetter := TBruteForcePhysicalDriveGetter.Create;
  PhysicalDriveList :=
    BruteForcePhysicalDriveGetter.GetPhysicalDriveList;
end;

procedure TMixedPhysicalDriveListGetter.LetWMIGetterFillTheList;
var
  BruteForcePhysicalDriveGetter: TBruteForcePhysicalDriveGetter;
begin
  BruteForcePhysicalDriveGetter := TBruteForcePhysicalDriveGetter.Create;
  PhysicalDriveList :=
    BruteForcePhysicalDriveGetter.GetPhysicalDriveList;
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