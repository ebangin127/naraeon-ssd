unit SMARTSupport.Factory;

interface

uses
  SysUtils,
  BufferInterpreter, Device.SMART.List, SMARTSupport,
  SMARTSupport.Seagate.NotSSD, SMARTSupport.Seagate.SSD, SMARTSupport.WD,
  SMARTSupport.Mtron, SMARTSupport.JMicron60x, SMARTSupport.JMicron61x,
  SMARTSupport.Indilinx, SMARTSupport.Intel, SMARTSupport.Samsung,
  SMARTSupport.Micron.New, SMARTSupport.Micron.Old, SMARTSupport.Sandforce,
  SMARTSupport.OCZ, SMARTSupport.OCZ.Vector, SMARTSupport.Plextor,
  SMARTSupport.Sandisk, SMARTSupport.Sandisk.GB, SMARTSupport.Kingston,
  SMARTSupport.Toshiba, SMARTSupport.Corsair, SMARTSupport.Fallback.SSD,
  SMARTSupport.Fallback, SMARTSupport.NVMe.Intel, SMARTSupport.NVMe;

type
  TMetaSMARTSupport = class of TSMARTSupport;
  TSMARTSupportFactory = class
  public
    function GetSuitableSMARTSupport(
      const IdentifyDevice: TIdentifyDeviceResult;
      const SMARTList: TSMARTValueList): TSMARTSupport;
  private
    FIdentifyDevice: TIdentifyDeviceResult;
    FSMARTList: TSMARTValueList;
    function TrySMARTSupportAndGetRightSMARTSupport: TSMARTSupport;
    function TestSMARTSupportCompatibility(
      TSMARTSupportToTry: TMetaSMARTSupport; LastResult: TSMARTSupport):
      TSMARTSupport;
    function TryFallbackSMARTSupports(
      const LastResult: TSMARTSupport): TSMARTSupport;
    function TryNVMeSMARTSupports(
      const LastResult: TSMARTSupport): TSMARTSupport;
    function TryATASMARTSupports(
      const LastResult: TSMARTSupport): TSMARTSupport;
  end;

implementation

{ TSMARTSupportFactory }

function TSMARTSupportFactory.GetSuitableSMARTSupport(
  const IdentifyDevice: TIdentifyDeviceResult;
  const SMARTList: TSMARTValueList): TSMARTSupport;
begin
  FIdentifyDevice := IdentifyDevice;
  FSMARTList := SMARTList;
  result := TrySMARTSupportAndGetRightSMARTSupport;
end;

function TSMARTSupportFactory.TrySMARTSupportAndGetRightSMARTSupport:
  TSMARTSupport;
begin
  result := nil;
  result := TryNVMeSMARTSupports(result);
  result := TryATASMARTSupports(result);
  result := TryFallbackSMARTSupports(result);
end;

function TSMARTSupportFactory.TryNVMeSMARTSupports(
  const LastResult: TSMARTSupport):
  TSMARTSupport;
begin
  result := LastResult;
  result := TestSMARTSupportCompatibility(
    TIntelNVMeSMARTSupport, result);
  result := TestSMARTSupportCompatibility(
    TNVMeSMARTSupport, result);
end;

function TSMARTSupportFactory.TryATASMARTSupports(
  const LastResult: TSMARTSupport):
  TSMARTSupport;
begin
  result := LastResult;
  result := TestSMARTSupportCompatibility(
    TSeagateSSDSMARTSupport, result);
  result := TestSMARTSupportCompatibility(
    TSeagateNotSSDSMARTSupport, result);
  result := TestSMARTSupportCompatibility(
    TWDSMARTSupport, result);
  result := TestSMARTSupportCompatibility(
    TMtronSMARTSupport, result);
  result := TestSMARTSupportCompatibility(
    TJMicron60xSMARTSupport, result);
  result := TestSMARTSupportCompatibility(
    TJMicron61xSMARTSupport, result);
  result := TestSMARTSupportCompatibility(
    TIndilinxSMARTSupport, result);
  result := TestSMARTSupportCompatibility(
    TIntelSMARTSupport, result);
  result := TestSMARTSupportCompatibility(
    TSamsungSMARTSupport, result);
  result := TestSMARTSupportCompatibility(
    TNewMicronSMARTSupport, result);
  result := TestSMARTSupportCompatibility(
    TOldMicronSMARTSupport, result);
  result := TestSMARTSupportCompatibility(
    TSandforceSMARTSupport, result);
  result := TestSMARTSupportCompatibility(
    TOCZSMARTSupport, result);
  result := TestSMARTSupportCompatibility(
    TOCZVectorSMARTSupport, result);
  result := TestSMARTSupportCompatibility(
    TPlextorSMARTSupport, result);
  result := TestSMARTSupportCompatibility(
    TSandiskSMARTSupport, result);
  result := TestSMARTSupportCompatibility(
    TSandiskGBSMARTSupport, result);
  result := TestSMARTSupportCompatibility(
    TKingstonSMARTSupport, result);
  result := TestSMARTSupportCompatibility(
    TToshibaSMARTSupport, result);
  result := TestSMARTSupportCompatibility(
    TCorsairSMARTSupport, result);
end;

function TSMARTSupportFactory.TryFallbackSMARTSupports(
  const LastResult: TSMARTSupport):
  TSMARTSupport;
begin
  result := LastResult;
  result := TestSMARTSupportCompatibility(
    TSSDFallbackSMARTSupport, result);
  result := TestSMARTSupportCompatibility(
    TFallbackSMARTSupport, result);
end;

function TSMARTSupportFactory.TestSMARTSupportCompatibility(
  TSMARTSupportToTry: TMetaSMARTSupport; LastResult: TSMARTSupport):
  TSMARTSupport;
begin
  if LastResult <> nil then
    exit(LastResult);

  result := TSMARTSupportToTry.Create;

  if not result.IsThisStorageMine(FIdentifyDevice, FSMARTList) then
    FreeAndNil(result);
end;

end.
