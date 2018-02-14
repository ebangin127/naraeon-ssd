// Ported CrystalDiskInfo (The MIT License, http://crystalmark.info)
unit SMARTSupport.Fallback.SSD;

interface

uses
  BufferInterpreter, Device.SMART.List, SMARTSupport,
  Support;

type
  TSSDFallbackSMARTSupport = class(TSMARTSupport)
  public
    function IsThisStorageMine(
      const IdentifyDevice: TIdentifyDeviceResult;
      const SMARTList: TSMARTValueList): Boolean; override;
    function GetTypeName: String; override;
    function IsSSD: Boolean; override;
    function IsInsufficientSMART: Boolean; override;
    function GetSMARTInterpreted(
      const SMARTList: TSMARTValueList): TSMARTInterpreted; override;
    function IsWriteValueSupported(const SMARTList: TSMARTValueList): Boolean;
      override;
  end;

implementation

{ TSSDFallbackSMARTSupport }

function TSSDFallbackSMARTSupport.GetTypeName: String;
begin
  result := 'SmartSsd';
end;

function TSSDFallbackSMARTSupport.IsInsufficientSMART: Boolean;
begin
  result := true;
end;

function TSSDFallbackSMARTSupport.IsSSD: Boolean;
begin
  result := true;
end;

function TSSDFallbackSMARTSupport.IsThisStorageMine(
  const IdentifyDevice: TIdentifyDeviceResult;
  const SMARTList: TSMARTValueList): Boolean;
begin
  result := CheckIsSSDInCommonWay(IdentifyDevice);
end;

function TSSDFallbackSMARTSupport.IsWriteValueSupported(
  const SMARTList: TSMARTValueList): Boolean;
begin
  result := false;
end;

function TSSDFallbackSMARTSupport.GetSMARTInterpreted(
  const SMARTList: TSMARTValueList): TSMARTInterpreted;
begin
  FillChar(result, SizeOf(result), 0);
end;

end.

