// Ported CrystalDiskInfo (The MIT License, http://crystalmark.info)
unit SMARTSupport.Intel;

interface

uses
  BufferInterpreter, Device.SMART.List, SMARTSupport, Support;

type
  TIntelSMARTSupport = class(TSMARTSupport)
  private
    function ModelHasIntelString(const Model: String): Boolean;
    function SMARTHasIntelCharacteristics(
      const SMARTList: TSMARTValueList): Boolean;
    function CheckCommonCharacteristics(
      const SMARTList: TSMARTValueList): Boolean;
    function CheckSpecificCharacteristics(
      const SMARTList: TSMARTValueList): Boolean;
    function CheckSpecificCharacteristics1(
      const SMARTList: TSMARTValueList): Boolean;
    function CheckSpecificCharacteristics2(
      const SMARTList: TSMARTValueList): Boolean;
    function CheckSpecificCharacteristics3(
      const SMARTList: TSMARTValueList): Boolean;
    function GetTotalWrite(const SMARTList: TSMARTValueList): TTotalWrite;
    const
      EntryID = $E8;
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

{ TIntelSMARTSupport }

function TIntelSMARTSupport.GetTypeName: String;
begin
  result := 'SmartIntel';
end;

function TIntelSMARTSupport.IsInsufficientSMART: Boolean;
begin
  result := false;
end;

function TIntelSMARTSupport.IsSSD: Boolean;
begin
  result := true;
end;

function TIntelSMARTSupport.IsThisStorageMine(
  const IdentifyDevice: TIdentifyDeviceResult;
  const SMARTList: TSMARTValueList): Boolean;
begin
  result :=
    ModelHasIntelString(IdentifyDevice.Model) or
    SMARTHasIntelCharacteristics(SMARTList);
end;

function TIntelSMARTSupport.ModelHasIntelString(const Model: String): Boolean;
begin
  result := Find('INTEL', Model);
end;

function TIntelSMARTSupport.SMARTHasIntelCharacteristics(
  const SMARTList: TSMARTValueList): Boolean;
begin
  result :=
    CheckCommonCharacteristics(SMARTList) and
    CheckSpecificCharacteristics(SMARTList);
end;

function TIntelSMARTSupport.CheckSpecificCharacteristics(
  const SMARTList: TSMARTValueList): Boolean;
begin
	result :=
    CheckSpecificCharacteristics1(SMARTList) or
    CheckSpecificCharacteristics2(SMARTList) or
    CheckSpecificCharacteristics3(SMARTList);
end;

function TIntelSMARTSupport.CheckCommonCharacteristics(
  const SMARTList: TSMARTValueList): Boolean;
begin
	result :=
    (SMARTList.Count >= 5) and
    (SMARTList[0].Id = $03) and
    (SMARTList[1].Id = $04) and
    (SMARTList[2].Id = $05) and
    (SMARTList[3].Id = $09) and
    (SMARTList[4].Id = $0C);
end;

function TIntelSMARTSupport.CheckSpecificCharacteristics1(
  const SMARTList: TSMARTValueList): Boolean;
begin
	result :=
    (SMARTList.Count >= 8) and
    (SMARTList[5].Id = $C0) and
    (SMARTList[6].Id = $E8) and
    (SMARTList[7].Id = $E9);
end;

function TIntelSMARTSupport.CheckSpecificCharacteristics2(
  const SMARTList: TSMARTValueList): Boolean;
begin
	result :=
    (SMARTList.Count >= 7) and
    (SMARTList[5].Id = $C0) and
    (SMARTList[6].Id = $E1);
end;

function TIntelSMARTSupport.CheckSpecificCharacteristics3(
  const SMARTList: TSMARTValueList): Boolean;
begin
	result :=
    (SMARTList.Count >= 8) and
    (SMARTList[5].Id = $AA) and
    (SMARTList[6].Id = $AB) and
    (SMARTList[7].Id = $AC);
end;

function TIntelSMARTSupport.ErrorCheckedGetLife(
  const SMARTList: TSMARTValueList): Integer;
begin
  result := SMARTList[SMARTList.GetIndexByID($E8)].Current;
end;

function TIntelSMARTSupport.InnerIsErrorAvailable(
  const SMARTList: TSMARTValueList): Boolean; 
begin
  result := true;
end;

function TIntelSMARTSupport.InnerIsCautionAvailable(
  const SMARTList: TSMARTValueList): Boolean;
begin
  result := IsEntryAvailable(EntryID, SMARTList);
end;

function TIntelSMARTSupport.InnerIsError(
  const SMARTList: TSMARTValueList): TSMARTErrorResult;
begin
  result.Override := false;
  result.Status :=
    InnerCommonIsError(EntryID, SMARTList).Status;
end;

function TIntelSMARTSupport.InnerIsCaution(
  const SMARTList: TSMARTValueList): TSMARTErrorResult; 
begin
  result := InnerCommonIsCaution(EntryID, SMARTList, CommonLifeThreshold);
end;

function TIntelSMARTSupport.IsWriteValueSupported(
  const SMARTList: TSMARTValueList): Boolean;
const
  WriteID1 = $E1;
  WriteID2 = $F1;
begin
  try
    SMARTList.GetIndexByID(WriteID1);
    result := true;
  except
    result := false;
  end;
  if not result then
  begin
    try
      SMARTList.GetIndexByID(WriteID2);
      result := true;
    except
      result := false;
    end;
  end;
end;

function TIntelSMARTSupport.GetSMARTInterpreted(
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
  result.TotalWrite := GetTotalWrite(SMARTList);
end;

function TIntelSMARTSupport.GetTotalWrite(
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

  WriteID1 = $E1;
  WriteID2 = $F1;
begin
  result.InValue.TrueHostWriteFalseNANDWrite :=
    HostWrite;
  try
    result.InValue.ValueInMiB :=
      SMARTList.GetRAWByID(WriteID1) * 32;
  except
    try
      result.InValue.ValueInMiB :=
        SMARTList.ExceptionFreeGetRAWByID(WriteID2) * 32;
    except
      result.InValue.ValueInMiB := 0;
    end;
  end;
end;

end.

