unit uAutoNSTSupport;

interface

uses
  SysUtils, Math,
  uNSTSupport, uSMARTValueList,
  uCrucialNSTSupport, uLiteonNSTSupport, uPlextorNSTSupport, uSandiskNSTSupport,
  uSeagateNSTSupport, uToshibaNSTSupport;

type
  TAutoNSTSupport = class sealed(TNSTSupport)
  private
    InterpretingSMARTValueList: TSMARTValueList;
    function TestNSTSupportCompatibilityAndReturnSupportStatus
      <T: TNSTSupport> (LastResult: TSupportStatus): TSupportStatus;
  public
    NSTSupport: TNSTSupport;
    function GetSupportStatus: TSupportStatus; override;
    function GetSMARTInterpreted(SMARTValueList: TSMARTValueList):
      TSMARTInterpreted; override;
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

  NSTSupportToTry := T(TNSTSupport(T).Create(Model, Firmware));
  result := NSTSupportToTry.GetSupportStatus;

  if result.Supported then
    NSTSupport := NSTSupportToTry
  else
    FreeAndNil(NSTSupportToTry);
end;

function TAutoNSTSupport.GetSupportStatus: TSupportStatus;
begin
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
end;

function TAutoNSTSupport.GetSMARTInterpreted(
  SMARTValueList: TSMARTValueList): TSMARTInterpreted;
begin
  result := NSTSupport.GetSMARTInterpreted(SMARTValueList);
end;

end.
