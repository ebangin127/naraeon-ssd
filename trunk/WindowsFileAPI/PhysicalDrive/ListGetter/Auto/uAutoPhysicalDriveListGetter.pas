unit uAutoPhysicalDriveListGetter;

interface

uses
  SysUtils,
  uPhysicalDriveListGetter, uBruteForcePhysicalDriveListGetter,
  uWMIPhysicalDriveListGetter, uPhysicalDriveList;

type
  TMetaPhysicalDriveListGetter = class of TPhysicalDriveListGetter;

  TAutoPhysicalDriveListGetter = class
  private
    class function TestCompatibilityAndGet(
      TPhysicalDriveListGetterToTry: TMetaPhysicalDriveListGetter;
      LastResult: TPhysicalDriveList): TPhysicalDriveList; static;
  public
    constructor Create;
    class function GetPhysicalDriveList: TPhysicalDriveList;
    class function GetPhysicalDriveListInService: TPhysicalDriveList;
  end;

implementation

{ TAutoPhysicalDriveListGetter }

constructor TAutoPhysicalDriveListGetter.Create;
begin

end;

class function TAutoPhysicalDriveListGetter.GetPhysicalDriveList:
  TPhysicalDriveList;
begin
  result := nil;
  result := TestCompatibilityAndGet(TWMIPhysicalDriveListGetter,
    result);
  result := TestCompatibilityAndGet(TBruteForcePhysicalDriveListGetter,
    result);
end;

class function TAutoPhysicalDriveListGetter.GetPhysicalDriveListInService:
  TPhysicalDriveList;
begin
  result := nil;
  result := TestCompatibilityAndGet(TBruteForcePhysicalDriveListGetter,
    result);
end;

class function TAutoPhysicalDriveListGetter.TestCompatibilityAndGet(
  TPhysicalDriveListGetterToTry: TMetaPhysicalDriveListGetter;
  LastResult: TPhysicalDriveList): TPhysicalDriveList;
var
  PhysicalDriveListGetter: TPhysicalDriveListGetter;
begin
  if LastResult <> nil then
    exit;

  PhysicalDriveListGetter := TPhysicalDriveListGetterToTry.Create;
  result := PhysicalDriveListGetter.GetPhysicalDriveList;

  if (result = nil) or
    (result.Count = 0) then
      FreeAndNil(result);

  FreeAndNil(PhysicalDriveListGetter);
end;

end.