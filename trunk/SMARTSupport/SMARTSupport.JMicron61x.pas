// Ported CrystalDiskInfo (The MIT License, http://crystalmark.info)
unit SMARTSupport.JMicron61x;

interface

uses
  BufferInterpreter, Device.SMART.List, SMARTSupport, Support;

type
  TJMicron61xSMARTSupport = class(TSMARTSupport)
  private
    const
      EntryID = $AA;
  public
    function IsThisStorageMine(
      const IdentifyDevice: TIdentifyDeviceResult;
      const SMARTList: TSMARTValueList): Boolean; override;
    function GetTypeName: String; override;
    function IsSSD: Boolean; override;
    function IsInsufficientSMART: Boolean; override;
    function GetSMARTInterpreted(
      const SMARTList: TSMARTValueList): TSMARTInterpreted; override;
    function IsWriteValueSupported(
      const SMARTList: TSMARTValueList): Boolean; override;
  protected
    function ErrorCheckedGetLife(const SMARTList: TSMARTValueList): Integer;
      override;
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

{ TJMicron61xSMARTSupport }

function TJMicron61xSMARTSupport.GetTypeName: String;
begin
  result := 'SmartJMicron61x';
end;

function TJMicron61xSMARTSupport.IsInsufficientSMART: Boolean;
begin
  result := false;
end;

function TJMicron61xSMARTSupport.IsSSD: Boolean;
begin
  result := true;
end;

function TJMicron61xSMARTSupport.IsThisStorageMine(
  const IdentifyDevice: TIdentifyDeviceResult;
  const SMARTList: TSMARTValueList): Boolean;
begin
	result :=
    (SMARTList.Count >= 13) and
    (SMARTList[0].Id = $01) and
    (SMARTList[1].Id = $02) and
    (SMARTList[2].Id = $03) and
    (SMARTList[3].Id = $05) and
    (SMARTList[4].Id = $07) and
    (SMARTList[5].Id = $08) and
    (SMARTList[6].Id = $09) and
    (SMARTList[7].Id = $0A) and
    (SMARTList[8].Id = $0C) and
    (SMARTList[9].Id = $A8) and
    (SMARTList[10].Id = $AF) and
    (SMARTList[11].Id = $C0) and
    (SMARTList[12].Id = $C2);
end;

function TJMicron61xSMARTSupport.ErrorCheckedGetLife(
  const SMARTList: TSMARTValueList): Integer;
begin
  result := SMARTList[SMARTList.GetIndexByID($AA)].Current;
end;

function TJMicron61xSMARTSupport.InnerIsErrorAvailable(
  const SMARTList: TSMARTValueList): Boolean; 
begin
  result := true;
end;

function TJMicron61xSMARTSupport.InnerIsCautionAvailable(
  const SMARTList: TSMARTValueList): Boolean;
begin
  result := IsEntryAvailable(EntryID, SMARTList);
end;

function TJMicron61xSMARTSupport.InnerIsError(
  const SMARTList: TSMARTValueList): TSMARTErrorResult;
begin
  result.Override := false;
  result.Status :=
    InnerCommonIsError(EntryID, SMARTList).Status;
end;

function TJMicron61xSMARTSupport.InnerIsCaution(
  const SMARTList: TSMARTValueList): TSMARTErrorResult; 
begin
  result := InnerCommonIsCaution(EntryID, SMARTList, CommonLifeThreshold);
end;

function TJMicron61xSMARTSupport.IsWriteValueSupported(
  const SMARTList: TSMARTValueList): Boolean;
begin
  result := false;
end;

function TJMicron61xSMARTSupport.GetSMARTInterpreted(
  const SMARTList: TSMARTValueList): TSMARTInterpreted;
const
  ReadError = true;
  EraseError = false;

  UsedHourID = $09;
  ThisErrorType = ReadError;
  ErrorID = $01;
  ReplacedSectorsID = $05;
begin
  FillChar(result, SizeOf(result), 0);
  result.UsedHour := SMARTList.ExceptionFreeGetRAWByID(UsedHourID);
  result.ReadEraseError.TrueReadErrorFalseEraseError := ReadError;
  result.ReadEraseError.Value := SMARTList.ExceptionFreeGetRAWByID(ErrorID);
  result.ReplacedSectors :=
    SMARTList.ExceptionFreeGetRAWByID(ReplacedSectorsID);
end;

end.

