// Ported CrystalDiskInfo (The MIT License, http://crystalmark.info)
unit SMARTSupport.Fallback;

interface

uses
  BufferInterpreter, Device.SMART.List, SMARTSupport,
  Support;

type
  TFallbackSMARTSupport = class(TSMARTSupport)
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

{ TFallbackSMARTSupport }

function TFallbackSMARTSupport.GetTypeName: String;
begin
  result := 'Smart';
end;

function TFallbackSMARTSupport.IsInsufficientSMART: Boolean;
begin
  result := true;
end;

function TFallbackSMARTSupport.IsSSD: Boolean;
begin
  result := true;
end;

function TFallbackSMARTSupport.IsThisStorageMine(
  const IdentifyDevice: TIdentifyDeviceResult;
  const SMARTList: TSMARTValueList): Boolean;
begin
  result := not CheckIsSSDInCommonWay(IdentifyDevice);
end;

function TFallbackSMARTSupport.IsWriteValueSupported(
  const SMARTList: TSMARTValueList): Boolean;
begin
  result := false;
end;

function TFallbackSMARTSupport.GetSMARTInterpreted(
  const SMARTList: TSMARTValueList): TSMARTInterpreted;
begin
  FillChar(result, SizeOf(result), 0);
end;

end.

