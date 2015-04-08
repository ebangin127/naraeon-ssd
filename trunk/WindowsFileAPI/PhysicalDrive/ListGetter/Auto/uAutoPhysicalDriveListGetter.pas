unit uAutoPhysicalDriveListGetter;

interface

uses
  SysUtils,
  uPhysicalDriveListGetter, uBruteForcePhysicalDriveListGetter,
  uWMIPhysicalDriveListGetter, uPhysicalDriveList;

type
  TAutoPhysicalDriveListGetter = class(TPhysicalDriveListGetter)
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

{ TAutoPhysicalDriveListGetter }
function TAutoPhysicalDriveListGetter.IsMoreTrialNeeded: Boolean;
begin
  result := 
    (PhysicalDriveList = nil) or
    (PhysicalDriveList.Count = 0);
end;

procedure TAutoPhysicalDriveListGetter.LetBruteForceGetterFillTheList;
var
  BruteForcePhysicalDriveListGetter: TBruteForcePhysicalDriveListGetter;
begin
  BruteForcePhysicalDriveListGetter := TBruteForcePhysicalDriveListGetter.Create;
  PhysicalDriveList :=
    BruteForcePhysicalDriveListGetter.GetPhysicalDriveList;
  FreeAndNil(BruteForcePhysicalDriveListGetter);
end;

procedure TAutoPhysicalDriveListGetter.LetWMIGetterFillTheList;
var
  WMIPhysicalDriveListGetter: TWMIPhysicalDriveListGetter;
begin
  WMIPhysicalDriveListGetter := TWMIPhysicalDriveListGetter.Create;
  PhysicalDriveList :=
    WMIPhysicalDriveListGetter.GetPhysicalDriveList;
  FreeAndNil(WMIPhysicalDriveListGetter);
end;

procedure TAutoPhysicalDriveListGetter.LetBruteForceGetterFillTheListIfNeeded;
begin
  if IsMoreTrialNeeded = false then
    exit;
  if PhysicalDriveList <> nil then
    FreeAndNil(PhysicalDriveList);
  LetBruteForceGetterFillTheList;
end;

procedure TAutoPhysicalDriveListGetter.LetWMIGetterFillTheListIfNeeded;
begin
  if IsMoreTrialNeeded = false then
    exit;
  if PhysicalDriveList <> nil then
    FreeAndNil(PhysicalDriveList);
  LetWMIGetterFillTheList;
end;

function TAutoPhysicalDriveListGetter.GetPhysicalDriveList:
  TPhysicalDriveList;
begin
  LetWMIGetterFillTheListIfNeeded;
  LetBruteForceGetterFillTheListIfNeeded;
  result := PhysicalDriveList;
end;

end.