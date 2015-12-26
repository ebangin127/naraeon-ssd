unit Getter.PhysicalDriveList.Auto;

interface

uses
  SysUtils,
  Getter.PhysicalDriveList, Getter.PhysicalDriveList.BruteForce,
  Getter.PhysicalDriveList.WMI, Device.PhysicalDrive.List;

type
  TMetaPhysicalDriveListGetter = class of TPhysicalDriveListGetter;

  TAutoPhysicalDriveListGetter = class
  private
    function TestCompatibilityAndGet(
      TPhysicalDriveListGetterToTry: TMetaPhysicalDriveListGetter;
      LastResult: TPhysicalDriveList): TPhysicalDriveList;
  public
    function GetPhysicalDriveList: TPhysicalDriveList;
    function GetPhysicalDriveListInService: TPhysicalDriveList;
    class function Create: TAutoPhysicalDriveListGetter;
  end;

var
  AutoPhysicalDriveListGetter: TAutoPhysicalDriveListGetter;

implementation

{ TAutoPhysicalDriveListGetter }

class function TAutoPhysicalDriveListGetter.Create:
  TAutoPhysicalDriveListGetter;
begin
  if AutoPhysicalDriveListGetter = nil then
    result := inherited Create as self
  else
    result := AutoPhysicalDriveListGetter;
end;

function TAutoPhysicalDriveListGetter.GetPhysicalDriveList:
  TPhysicalDriveList;
begin
  result := nil;
  result := TestCompatibilityAndGet(TWMIPhysicalDriveListGetter,
    result);
  result := TestCompatibilityAndGet(TBruteForcePhysicalDriveListGetter,
    result);
  if result = nil then
    result := TPhysicalDriveList.Create;
end;

function TAutoPhysicalDriveListGetter.GetPhysicalDriveListInService:
  TPhysicalDriveList;
begin
  result := nil;
  result := TestCompatibilityAndGet(TBruteForcePhysicalDriveListGetter,
    result);
end;

function TAutoPhysicalDriveListGetter.TestCompatibilityAndGet(
  TPhysicalDriveListGetterToTry: TMetaPhysicalDriveListGetter;
  LastResult: TPhysicalDriveList): TPhysicalDriveList;
var
  PhysicalDriveListGetter: TPhysicalDriveListGetter;
begin
  if LastResult <> nil then
    exit;

  PhysicalDriveListGetter := TPhysicalDriveListGetterToTry.Create;
  result := PhysicalDriveListGetter.GetPhysicalDriveList;

  if (result = nil) or (result.Count = 0) then
    FreeAndNil(result);

  FreeAndNil(PhysicalDriveListGetter);
end;

initialization
  AutoPhysicalDriveListGetter := TAutoPhysicalDriveListGetter.Create;
finalization
  AutoPhysicalDriveListGetter.Free;
end.