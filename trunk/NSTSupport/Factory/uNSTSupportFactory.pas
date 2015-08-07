unit uNSTSupportFactory;

interface

uses
  SysUtils, 
  uNSTSupport
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
  TMetaNSTSupport = class of TNSTSupport;
  TNSTSupportFactory = class
  public
    class function GetSuitableNSTSupport(Model, Firmware: String):
      TNSTSupport;
  private
    class var Model: String;
    class var Firmware: String;
    class function TryNSTSupportAndGetRightNSTSupport: TNSTSupport;
    class function TestNSTSupportCompatibility(
      TNSTSupportToTry: TMetaNSTSupport; LastResult: TNSTSupport): TNSTSupport;
  end;

implementation

{ TNSTSupportFactory }

class function TNSTSupportFactory.GetSuitableNSTSupport(Model, Firmware: String):
  TNSTSupport;
begin
  self.Model := Model;
  self.Firmware := Firmware;
  result := TryNSTSupportAndGetRightNSTSupport(Model, Firmware):
end;

class function TAutoCommandSet.TryNSTSupportAndGetRightNSTSupport: TNSTSupport;
begin
  result := nil;
  result := TestNSTSupportCompatibility(TCrucialNSTSupport, result);
  result := TestNSTSupportCompatibility(TLiteonNSTSupport, result);
  result := TestNSTSupportCompatibility(TPlextorNSTSupport, result);
  result := TestNSTSupportCompatibility(TSandiskNSTSupport, result);
  result := TestNSTSupportCompatibility(TSeagateNSTSupport, result);
  result := TestNSTSupportCompatibility(TToshibaNSTSupport, result);
  result := TestNSTSupportCompatibility(TSamsungNSTSupport, result);
  result := TestNSTSupportCompatibility(TMachXtremeNSTSupport, result);
  result := TestNSTSupportCompatibility(TPhisonNSTSupport, result);
  result := TestNSTSupportCompatibility(TADATANSTSupport, result);
  result := TestNSTSupportCompatibility(TToshibaSandforceNSTSupport, result);
  result := TestNSTSupportCompatibility(THynixSandforceNSTSupport, result);
  result := TestNSTSupportCompatibility(TOCZSandforceNSTSupport, result);
  result := TestNSTSupportCompatibility(TPatriotSandforceNSTSupport, result);
  result := TestNSTSupportCompatibility(TMachXtremeSandforceNSTSupport, result);
  result := TestNSTSupportCompatibility(TADATASandforceNSTSupport, result);
end;

class function TAutoNSTSupport.TestNSTSupportCompatibility(
  TNSTSupportToTry: TMetaNSTSupport; LastResult: TNSTSupport): TNSTSupport;
var
  NSTSupportToTry: TNSTSupport;
begin
  if LastResult <> nil then
    exit(LastResult);
  
  result := TNSTSupportToTry.Create(Model, Firmware);

  if not result.GetSupportStatus.Supported then
    FreeAndNil(result);
end;

end.
