unit uAutoNSTSupport;

interface

uses
  SysUtils, Math,
  uNSTSupport, uSMARTValueList, uSandforceNSTSupport,
  //General//
  uCrucialNSTSupport, uLiteonNSTSupport, uPlextorNSTSupport, uSandiskNSTSupport,
  uSeagateNSTSupport, uToshibaNSTSupport, uSamsungNSTSupport,
  uMachXtremeNSTSupport, uPhisonNSTSupport, uADATANSTSupport,
  //Sandforce//
  uToshibaSandforceNSTSupport, uHynixSandforceNSTSupport,
  uOCZSandforceNSTSupport, uPatriotSandforceNSTSupport,
  uMachXtremeSandforceNSTSupport,
  uADATASandforceNSTSupport;

type
  TAutoNSTSupport = class sealed(TNSTSupport)
  private
    NSTSupport: TNSTSupport;
    function TestNSTSupportCompatibilityAndReturnSupportStatus
      <T: TNSTSupport, constructor> (LastResult: TSupportStatus):
      TSupportStatus;
  public
    function GetSupportStatus: TSupportStatus; override;
    function GetSMARTInterpreted(SMARTValueList: TSMARTValueList):
      TSMARTInterpreted; override;
    function IsSandforce: Boolean;
    destructor Destroy; override;
  end;

implementation

{ TAutoNSTSupport }

function TAutoNSTSupport.TestNSTSupportCompatibilityAndReturnSupportStatus
  <T> (LastResult: TSupportStatus): TSupportStatus;
var
  NSTSupportToTry: T;
begin
  if LastResult.Supported then
    exit(LastResult);

  NSTSupportToTry := T.Create;
  NSTSupportToTry.SetModelAndFirmware(Model, Firmware);
  result := NSTSupportToTry.GetSupportStatus;

  if result.Supported then
    NSTSupport := NSTSupportToTry
  else
    FreeAndNil(NSTSupportToTry);
end;

function TAutoNSTSupport.GetSupportStatus: TSupportStatus;
begin
  result.Supported := false;
  if NSTSupport <> nil then
    FreeAndNil(NSTSupport);

  result :=
    TestNSTSupportCompatibilityAndReturnSupportStatus
      <TCrucialNSTSupport>(result);
  result :=
    TestNSTSupportCompatibilityAndReturnSupportStatus
      <TLiteonNSTSupport>(result);
  result :=
    TestNSTSupportCompatibilityAndReturnSupportStatus
      <TPlextorNSTSupport>(result);
  result :=
    TestNSTSupportCompatibilityAndReturnSupportStatus
      <TSandiskNSTSupport>(result);
  result :=
    TestNSTSupportCompatibilityAndReturnSupportStatus
      <TSeagateNSTSupport>(result);
  result :=
    TestNSTSupportCompatibilityAndReturnSupportStatus
      <TToshibaNSTSupport>(result);
  result :=
    TestNSTSupportCompatibilityAndReturnSupportStatus
      <TSamsungNSTSupport>(result);
  result :=
    TestNSTSupportCompatibilityAndReturnSupportStatus
      <TMachXtremeNSTSupport>(result);
  result :=
    TestNSTSupportCompatibilityAndReturnSupportStatus
      <TPhisonNSTSupport>(result);
  result :=
    TestNSTSupportCompatibilityAndReturnSupportStatus
      <TADATANSTSupport>(result);
  result :=
    TestNSTSupportCompatibilityAndReturnSupportStatus
      <TToshibaSandforceNSTSupport>(result);
  result :=
    TestNSTSupportCompatibilityAndReturnSupportStatus
      <THynixSandforceNSTSupport>(result);
  result :=
    TestNSTSupportCompatibilityAndReturnSupportStatus
      <TOCZSandforceNSTSupport>(result);
  result :=
    TestNSTSupportCompatibilityAndReturnSupportStatus
      <TPatriotSandforceNSTSupport>(result);
  result :=
    TestNSTSupportCompatibilityAndReturnSupportStatus
      <TMachXtremeSandforceNSTSupport>(result);
  result :=
    TestNSTSupportCompatibilityAndReturnSupportStatus
      <TADATASandforceNSTSupport>(result);
end;

destructor TAutoNSTSupport.Destroy;
begin
  if NSTSupport <> nil then
    FreeAndNil(NSTSupport);
  inherited;
end;

function TAutoNSTSupport.GetSMARTInterpreted(
  SMARTValueList: TSMARTValueList): TSMARTInterpreted;
begin
  if NSTSupport = nil then
    GetSupportStatus;
  result := NSTSupport.GetSMARTInterpreted(SMARTValueList);
end;

function TAutoNSTSupport.IsSandforce: Boolean;
begin
  result := NSTSupport is TSandforceNSTSupport;
end;

end.
