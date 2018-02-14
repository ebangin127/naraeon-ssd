// Ported CrystalDiskInfo (The MIT License, http://crystalmark.info)
unit SMARTSupport.OCZ.Vector;

interface

uses
  BufferInterpreter, Device.SMART.List, SMARTSupport, Support;

type
  TOCZVectorSMARTSupport = class(TSMARTSupport)
  private
    function IsPanasonicRPSSB(const Model: String): Boolean;
    function IsOCZVector(const IdentifyDevice: TIdentifyDeviceResult;
      const SMARTList: TSMARTValueList): Boolean;
    function ModelHasOCZString(const Model: String): Boolean;
    function SMARTHasOCZVectorCharacteristics(
      const SMARTList: TSMARTValueList): Boolean;
    function GetTotalWrite(const SMARTList: TSMARTValueList): TTotalWrite;
    const
      EntryID = $E9;
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

{ TOCZVectorSMARTSupport }

function TOCZVectorSMARTSupport.GetTypeName: String;
begin
  result := 'SmartOczVector';
end;

function TOCZVectorSMARTSupport.IsSSD: Boolean;
begin
  result := true;
end;

function TOCZVectorSMARTSupport.IsThisStorageMine(
  const IdentifyDevice: TIdentifyDeviceResult;
  const SMARTList: TSMARTValueList): Boolean;
begin
  result :=
    IsPanasonicRPSSB(IdentifyDevice.Model) or
    IsOCZVector(IdentifyDevice, SMARTList);
end;

function TOCZVectorSMARTSupport.IsPanasonicRPSSB(const Model: String):
  Boolean;
begin
  result := FindAtFirst('PANASONIC RP-SSB', Model);
end;

function TOCZVectorSMARTSupport.IsInsufficientSMART: Boolean;
begin
  result := false;
end;

function TOCZVectorSMARTSupport.IsOCZVector(
  const IdentifyDevice: TIdentifyDeviceResult;
  const SMARTList: TSMARTValueList): Boolean;
begin
  result :=
    ModelHasOCZString(IdentifyDevice.Model) or
    SMARTHasOCZVectorCharacteristics(SMARTList);
end;

function TOCZVectorSMARTSupport.ModelHasOCZString(const Model: String):
  Boolean;
begin
  result := Find('OCZ', Model);
end;

function TOCZVectorSMARTSupport.SMARTHasOCZVectorCharacteristics(
  const SMARTList: TSMARTValueList): Boolean;
begin
	// 2015/11/25
	// PANASONIC RP-SSB240GAK
	// http://crystalmark.info/board/c-board.cgi?cmd=one;no=500;id=#500
	result :=
    (SMARTList.Count >= 9) and
    (SMARTList[0].Id = $05) and
    (SMARTList[1].Id = $09) and
    (SMARTList[2].Id = $0C) and
    (SMARTList[3].Id = $AB) and
    (SMARTList[4].Id = $AE) and
    (SMARTList[5].Id = $C3) and
    (SMARTList[6].Id = $C4) and
    (SMARTList[7].Id = $C5) and
    (SMARTList[8].Id = $C6);
end;

function TOCZVectorSMARTSupport.ErrorCheckedGetLife(
  const SMARTList: TSMARTValueList): Integer;
begin
  result := SMARTList[SMARTList.GetIndexByID($E9)].Current;
end;

function TOCZVectorSMARTSupport.InnerIsErrorAvailable(
  const SMARTList: TSMARTValueList): Boolean; 
begin
  result := true;
end;

function TOCZVectorSMARTSupport.InnerIsCautionAvailable(
  const SMARTList: TSMARTValueList): Boolean;
begin
  result := IsEntryAvailable(EntryID, SMARTList);
end;

function TOCZVectorSMARTSupport.InnerIsError(
  const SMARTList: TSMARTValueList): TSMARTErrorResult;
begin
  result.Override := false;
  result.Status :=
    InnerCommonIsError(EntryID, SMARTList).Status;
end;

function TOCZVectorSMARTSupport.InnerIsCaution(
  const SMARTList: TSMARTValueList): TSMARTErrorResult; 
begin
  result := InnerCommonIsCaution(EntryID, SMARTList, CommonLifeThreshold);
end;

function TOCZVectorSMARTSupport.IsWriteValueSupported(
  const SMARTList: TSMARTValueList): Boolean;
const
  WriteID = $F1;
begin
  try
    SMARTList.GetIndexByID(WriteID);
    result := true;
  except
    result := false;
  end;
end;

function TOCZVectorSMARTSupport.GetSMARTInterpreted(
  const SMARTList: TSMARTValueList): TSMARTInterpreted;
const
  ReadError = true;
  EraseError = false;

  UsedHourID = $09;
  ThisErrorType = ReadError;
  ErrorID = $C5;
  ReplacedSectorsID = $05;
begin
  FillChar(result, SizeOf(result), 0);
  result.UsedHour := SMARTList.ExceptionFreeGetRAWByID(UsedHourID);
  result.ReadEraseError.TrueReadErrorFalseEraseError := ReadError;
  result.ReadEraseError.Value :=
    SMARTList.ExceptionFreeGetRAWByID(ErrorID);
  result.ReplacedSectors :=
    SMARTList.ExceptionFreeGetRAWByID(ReplacedSectorsID);
  result.TotalWrite := GetTotalWrite(SMARTList);
end;

function TOCZVectorSMARTSupport.GetTotalWrite(
  const SMARTList: TSMARTValueList): TTotalWrite;
  function LBAToMB(const SizeInLBA: Int64): UInt64;
  begin
    result := SizeInLBA shr 1;
  end;
  function GBToMB(const SizeInLBA: Int64): UInt64;
  begin
    result := SizeInLBA shl 10;
  end;
const
  HostWrite = true;
  NANDWrite = false;

  WriteID = $F1;
begin
  result.InValue.TrueHostWriteFalseNANDWrite :=
    HostWrite;
  result.InValue.ValueInMiB :=
    GBToMB(SMARTList.ExceptionFreeGetRAWByID(WriteID));
end;

end.

