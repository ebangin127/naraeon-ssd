unit uNSTSupportFactory;

interface

uses
  SysUtils, 
  uNSTSupport,
  //General//
  uCrucialNSTSupport, uLiteonNSTSupport, uPlextorNSTSupport, uSandiskNSTSupport,
  uSeagateNSTSupport, uToshibaNSTSupport, uSamsungNSTSupport,
  uMachXtremeNSTSupport, uPhisonNSTSupport, uADATANSTSupport,
  //Sandforce//
  uToshibaSandforceNSTSupport, uHynixSandforceNSTSupport,
  uOCZSandforceNSTSupport, uPatriotSandforceNSTSupport,
  uMachXtremeSandforceNSTSupport,
  uADATASandforceNSTSupport,
  //NVMe//
  Support.Samsung.NVMe, Support.Intel.NVMe;

type
  TMetaNSTSupport = class of TNSTSupport;
  TNSTSupportFactory = class
  public
    function GetSuitableNSTSupport(Model, Firmware: String):
      TNSTSupport;
    class function Create: TNSTSupportFactory;
  private
    function TryNSTSupportAndGetRightNSTSupport: TNSTSupport;
    function TestNSTSupportCompatibility(
      TNSTSupportToTry: TMetaNSTSupport; LastResult: TNSTSupport): TNSTSupport;
    var
      FModel: String;
      FFirmware: String;
  end;

var
  NSTSupportFactory: TNSTSupportFactory;

implementation

{ TNSTSupportFactory }

class function TNSTSupportFactory.Create: TNSTSupportFactory;
begin
  if NSTSupportFactory = nil then
    result := inherited Create as self
  else
    result := NSTSupportFactory;
end;

function TNSTSupportFactory.GetSuitableNSTSupport(Model, Firmware: String):
  TNSTSupport;
begin
  FModel := Model;
  FFirmware := Firmware;
  result := TryNSTSupportAndGetRightNSTSupport;
end;

function TNSTSupportFactory.TryNSTSupportAndGetRightNSTSupport: TNSTSupport;
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
  result := TestNSTSupportCompatibility(TSamsungNVMeSupport, result);
  result := TestNSTSupportCompatibility(TIntelNVMeSupport, result);
end;

function TNSTSupportFactory.TestNSTSupportCompatibility(
  TNSTSupportToTry: TMetaNSTSupport; LastResult: TNSTSupport): TNSTSupport;
begin
  if LastResult <> nil then
    exit(LastResult);
  
  result := TNSTSupportToTry.Create(FModel, FFirmware);

  if not result.GetSupportStatus.Supported then
    FreeAndNil(result);
end;

initialization
  NSTSupportFactory := TNSTSupportFactory.Create;
finalization
  NSTSupportFactory.Free;
end.
