// Ported CrystalDiskInfo (The MIT License, http://crystalmark.info)
unit SMARTSupport.Seagate.NotSSD;

interface

uses
  BufferInterpreter, Device.SMART.List, SMARTSupport, Support;

type
  TSeagateNotSSDSMARTSupport = class(TSMARTSupport)
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

{ TSeagateNotSSDSMARTSupport }

function TSeagateNotSSDSMARTSupport.GetTypeName: String;
begin
  result := 'Smart';
end;

function TSeagateNotSSDSMARTSupport.IsSSD: Boolean;
begin
  result := false;
end;

function TSeagateNotSSDSMARTSupport.IsThisStorageMine(
  const IdentifyDevice: TIdentifyDeviceResult;
  const SMARTList: TSMARTValueList): Boolean;
begin
  result :=
    (FindAtFirst('STT', IdentifyDevice.Model)) and
    (Find('ST', IdentifyDevice.Model)) and
    (not CheckIsSSDInCommonWay(IdentifyDevice));
end;

function TSeagateNotSSDSMARTSupport.IsInsufficientSMART: Boolean;
begin
  result := true;
end;

function TSeagateNotSSDSMARTSupport.IsWriteValueSupported(
  const SMARTList: TSMARTValueList): Boolean;
begin
  result := false;
end;

function TSeagateNotSSDSMARTSupport.GetSMARTInterpreted(
  const SMARTList: TSMARTValueList): TSMARTInterpreted;
begin
  FillChar(result, SizeOf(result), 0);
end;

end.
