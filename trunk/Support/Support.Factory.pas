unit Support.Factory;

interface

uses
  SysUtils, 
  Support,
  //General//
  Support.Crucial, Support.Liteon, Support.Plextor, Support.Sandisk,
  Support.Seagate, Support.Toshiba, Support.Samsung,
  Support.MachXtreme, Support.Phison, Support.ADATA,
  //Sandforce//
  Support.Sandforce.Toshiba, Support.Sandforce.Hynix,
  Support.Sandforce.OCZ, Support.Sandforce.Patriot,
  Support.Sandforce.MachXtreme,
  Support.Sandforce.ADATA,
  //NVMe//
  Support.NVMe.Samsung, Support.NVMe.Intel;

type
  TMetaNSTSupport = class of TNSTSupport;
  TNSTSupport.Factory = class
  public
    function GetSuitableNSTSupport(Model, Firmware: String):
      TNSTSupport;
    class function Create: TNSTSupport.Factory;
  private
    function TryNSTSupportAndGetRightNSTSupport: TNSTSupport;
    function TestNSTSupportCompatibility(
      TNSTSupportToTry: TMetaNSTSupport; LastResult: TNSTSupport): TNSTSupport;
    var
      FModel: String;
      FFirmware: String;
  end;

var
  NSTSupport.Factory: TNSTSupport.Factory;

implementation

{ TNSTSupport.Factory }

class function TNSTSupport.Factory.Create: TNSTSupport.Factory;
begin
  if NSTSupport.Factory = nil then
    result := inherited Create as self
  else
    result := NSTSupport.Factory;
end;

function TNSTSupport.Factory.GetSuitableNSTSupport(Model, Firmware: String):
  TNSTSupport;
begin
  FModel := Model;
  FFirmware := Firmware;
  result := TryNSTSupportAndGetRightNSTSupport;
end;

function TNSTSupport.Factory.TryNSTSupportAndGetRightNSTSupport: TNSTSupport;
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

function TNSTSupport.Factory.TestNSTSupportCompatibility(
  TNSTSupportToTry: TMetaNSTSupport; LastResult: TNSTSupport): TNSTSupport;
begin
  if LastResult <> nil then
    exit(LastResult);
  
  result := TNSTSupportToTry.Create(FModel, FFirmware);

  if not result.GetSupportStatus.Supported then
    FreeAndNil(result);
end;

initialization
  NSTSupport.Factory := TNSTSupport.Factory.Create;
finalization
  NSTSupport.Factory.Free;
end.
