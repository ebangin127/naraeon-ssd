unit Support.Factory;

interface

uses
  SysUtils,
  BufferInterpreter, Device.SMART.List,
  Support,
  //General//
  Support.Crucial, Support.Liteon, Support.Plextor, Support.Sandisk,
  Support.Seagate, Support.Toshiba, Support.Samsung, Support.Sandisk.USB,
  Support.MachXtreme, Support.Phison, Support.ADATA, Support.Transcend,
  //Sandforce//
  Support.Sandforce.Toshiba, Support.Sandforce.Hynix,
  Support.Sandforce.OCZ, Support.Sandforce.Patriot,
  Support.Sandforce.MachXtreme,
  Support.Sandforce.ADATA,
  //CDI//
  Support.Fallback.CDI,
  //NVMe//
  Support.NVMe.Samsung, Support.NVMe.Intel;

type
  TMetaNSTSupport = class of TNSTSupport;
  TNSTSupportFactory = class
  public
    function GetSuitableNSTSupport(
      const IdentifyDevice: TIdentifyDeviceResult;
      const SMARTList: TSMARTValueList): TNSTSupport;
  private
    FIdentifyDevice: TIdentifyDeviceResult;
    FSMARTList: TSMARTValueList;
    function TryNSTSupportAndGetRightNSTSupport: TNSTSupport;
    function TestNSTSupportCompatibility(
      TNSTSupportToTry: TMetaNSTSupport; LastResult: TNSTSupport): TNSTSupport;
  end;

implementation

{ TNSTSupportFactory }

function TNSTSupportFactory.GetSuitableNSTSupport(
  const IdentifyDevice: TIdentifyDeviceResult;
  const SMARTList: TSMARTValueList): TNSTSupport;
begin
  FIdentifyDevice := IdentifyDevice;
  FSMARTList := SMARTList;
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
  result := TestNSTSupportCompatibility(TTranscendNSTSupport, result);
  result := TestNSTSupportCompatibility(TSamsungNSTSupport, result);
  result := TestNSTSupportCompatibility(TMachXtremeNSTSupport, result);
  result := TestNSTSupportCompatibility(TPhisonNSTSupport, result);
  result := TestNSTSupportCompatibility(TADATANSTSupport, result);
  result := TestNSTSupportCompatibility(TSandiskUSBNSTSupport, result);
  result := TestNSTSupportCompatibility(TToshibaSandforceNSTSupport, result);
  result := TestNSTSupportCompatibility(THynixSandforceNSTSupport, result);
  result := TestNSTSupportCompatibility(TOCZSandforceNSTSupport, result);
  result := TestNSTSupportCompatibility(TPatriotSandforceNSTSupport, result);
  result := TestNSTSupportCompatibility(TMachXtremeSandforceNSTSupport, result);
  result := TestNSTSupportCompatibility(TADATASandforceNSTSupport, result);
  result := TestNSTSupportCompatibility(TSamsungNVMeSupport, result);
  result := TestNSTSupportCompatibility(TIntelNVMeSupport, result);
  result := TestNSTSupportCompatibility(TFallbackCDISupport, result);
end;

function TNSTSupportFactory.TestNSTSupportCompatibility(
  TNSTSupportToTry: TMetaNSTSupport; LastResult: TNSTSupport): TNSTSupport;
begin
  if LastResult <> nil then
    exit(LastResult);
  
  result := TNSTSupportToTry.Create(FIdentifyDevice, FSMARTList);

  if result.GetSupportStatus.Supported = NotSupported then
    FreeAndNil(result);
end;

end.
