// Ported CrystalDiskInfo (The MIT License, http://crystalmark.info)
unit SMARTSupport.JMicron60x;

interface

uses
  BufferInterpreter, Device.SMART.List, SMARTSupport,
  Support;

type
  TJMicron60xSMARTSupport = class(TSMARTSupport)
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
  protected
    function InnerIsErrorAvailable(const SMARTList: TSMARTValueList):
      Boolean; override; 
    function InnerIsCautionAvailable(const SMARTList: TSMARTValueList): 
      Boolean; override; 
    function InnerIsError(const SMARTList: TSMARTValueList): TSMARTErrorResult;
      override; 
    function InnerIsCaution(const SMARTList: TSMARTValueList):
      TSMARTErrorResult; override;
  end;

implementation

{ TJMicron60xSMARTSupport }

function TJMicron60xSMARTSupport.GetTypeName: String;
begin
  result := 'SmartJMicron60x';
end;

function TJMicron60xSMARTSupport.IsInsufficientSMART: Boolean;
begin
  result := true;
end;

function TJMicron60xSMARTSupport.IsSSD: Boolean;
begin
  result := true;
end;

function TJMicron60xSMARTSupport.IsThisStorageMine(
  const IdentifyDevice: TIdentifyDeviceResult;
  const SMARTList: TSMARTValueList): Boolean;
begin
	result :=
    (SMARTList.Count >= 6) and
    (SMARTList[0].Id = $0C) and
    (SMARTList[1].Id = $09) and
    (SMARTList[2].Id = $C2) and
    (SMARTList[3].Id = $E5) and
    (SMARTList[4].Id = $E8) and
    (SMARTList[5].Id = $E9);
end;

function TJMicron60xSMARTSupport.InnerIsErrorAvailable(
  const SMARTList: TSMARTValueList): Boolean; 
begin
  result := true;
end;

function TJMicron60xSMARTSupport.InnerIsCautionAvailable(
  const SMARTList: TSMARTValueList): Boolean; 
begin
  result := true;
end;

function TJMicron60xSMARTSupport.InnerIsError(
  const SMARTList: TSMARTValueList): TSMARTErrorResult;
begin
  result.Override := true;
  result.Status := false;
end;

function TJMicron60xSMARTSupport.InnerIsCaution(
  const SMARTList: TSMARTValueList): TSMARTErrorResult; 
begin
  result.Override := true;
  result.Status := false;
end;

function TJMicron60xSMARTSupport.IsWriteValueSupported(
  const SMARTList: TSMARTValueList): Boolean;
begin
  result := false;
end;

function TJMicron60xSMARTSupport.GetSMARTInterpreted(
  const SMARTList: TSMARTValueList): TSMARTInterpreted;
begin
  FillChar(result, SizeOf(result), 0);
end;

end.

