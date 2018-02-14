// Ported CrystalDiskInfo (The MIT License, http://crystalmark.info)
unit SMARTSupport.Samsung;

interface

uses
  SysUtils,
  BufferInterpreter, Device.SMART.List, SMARTSupport, Support;

type
  TSamsungSMARTSupport = class(TSMARTSupport)
  private
    function ModelHasSamsungString(const Model: String): Boolean;
    function SMARTHasSamsungCharacteristics(
      const SMARTList: TSMARTValueList): Boolean;
    function CheckSpecificCharacteristics1(
      const SMARTList: TSMARTValueList): Boolean;
    function CheckSpecificCharacteristics2(
      const SMARTList: TSMARTValueList): Boolean;
    function CheckSpecificCharacteristics3(
      const SMARTList: TSMARTValueList): Boolean;
    function CheckSpecificCharacteristics4(
      const SMARTList: TSMARTValueList): Boolean;
    function CheckSpecificCharacteristics5(
      const SMARTList: TSMARTValueList): Boolean;
    function GetTotalWrite(const SMARTList: TSMARTValueList): TTotalWrite;
    const
      EntryID1 = $B3;
      EntryID2 = $B4;
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

{ TSamsungSMARTSupport }

function TSamsungSMARTSupport.GetTypeName: String;
begin
  result := 'SmartSamsung';
end;

function TSamsungSMARTSupport.IsInsufficientSMART: Boolean;
begin
  result := false;
end;

function TSamsungSMARTSupport.IsSSD: Boolean;
begin
  result := true;
end;

function TSamsungSMARTSupport.IsThisStorageMine(
  const IdentifyDevice: TIdentifyDeviceResult;
  const SMARTList: TSMARTValueList): Boolean;
begin
  result :=
    ModelHasSamsungString(IdentifyDevice.Model) or
    SMARTHasSamsungCharacteristics(SMARTList);
end;

function TSamsungSMARTSupport.ModelHasSamsungString(const Model: String):
  Boolean;
var
  ModelInUpperCase: String;
begin
  ModelInUpperCase := UpperCase(Model);
  result := Find('SAMSUNG', ModelInUpperCase) or Find('MZ-', ModelInUpperCase);
end;

function TSamsungSMARTSupport.SMARTHasSamsungCharacteristics(
  const SMARTList: TSMARTValueList): Boolean;
begin
  result :=
    CheckSpecificCharacteristics1(SMARTList) or
    CheckSpecificCharacteristics2(SMARTList) or
    CheckSpecificCharacteristics3(SMARTList) or
    CheckSpecificCharacteristics4(SMARTList) or
    CheckSpecificCharacteristics5(SMARTList);
end;

function TSamsungSMARTSupport.CheckSpecificCharacteristics1(
  const SMARTList: TSMARTValueList): Boolean;
begin
	result :=
    (SMARTList.Count >= 10) and
    (SMARTList[0].Id = $05) and
    (SMARTList[1].Id = $09) and
    (SMARTList[2].Id = $0C) and
    (SMARTList[3].Id = $AA) and
    (SMARTList[4].Id = $AB) and
    (SMARTList[5].Id = $AC) and
    (SMARTList[6].Id = $AD) and
    (SMARTList[7].Id = $AE) and
    (SMARTList[8].Id = $B2) and
    (SMARTList[9].Id = $B4);
end;

function TSamsungSMARTSupport.CheckSpecificCharacteristics2(
  const SMARTList: TSMARTValueList): Boolean;
begin
	result :=
    (SMARTList.Count >= 5) and
    (SMARTList[0].Id = $09) and
    (SMARTList[1].Id = $0C) and
    (SMARTList[2].Id = $B2) and
    (SMARTList[3].Id = $B3) and
    (SMARTList[4].Id = $B4);
end;

function TSamsungSMARTSupport.CheckSpecificCharacteristics3(
  const SMARTList: TSMARTValueList): Boolean;
begin
	result :=
    (SMARTList.Count >= 7) and
    (SMARTList[0].Id = $09) and
    (SMARTList[1].Id = $0C) and
    (SMARTList[2].Id = $B1) and
    (SMARTList[3].Id = $B2) and
    (SMARTList[4].Id = $B3) and
    (SMARTList[5].Id = $B4) and
    (SMARTList[6].Id = $B7);
end;

function TSamsungSMARTSupport.CheckSpecificCharacteristics4(
  const SMARTList: TSMARTValueList): Boolean;
begin
	result :=
    (SMARTList.Count >= 8) and
    (SMARTList[0].Id = $09) and
    (SMARTList[1].Id = $0C) and
    (SMARTList[2].Id = $AF) and
    (SMARTList[3].Id = $B0) and
    (SMARTList[4].Id = $B1) and
    (SMARTList[5].Id = $B2) and
    (SMARTList[6].Id = $B3) and
    (SMARTList[7].Id = $B4);
end;

function TSamsungSMARTSupport.CheckSpecificCharacteristics5(
  const SMARTList: TSMARTValueList): Boolean;
begin
	result :=
    (SMARTList.Count >= 8) and
    (SMARTList[0].Id = $05) and
    (SMARTList[1].Id = $09) and
    (SMARTList[2].Id = $0C) and
    (SMARTList[3].Id = $B1) and
    (SMARTList[4].Id = $B2) and
    (SMARTList[5].Id = $B3) and
    (SMARTList[6].Id = $B5) and
    (SMARTList[7].Id = $B6);
end;

function TSamsungSMARTSupport.ErrorCheckedGetLife(
  const SMARTList: TSMARTValueList): Integer;
begin
  result := SMARTList[SMARTList.GetIndexByID($B4)].Current;
end;

function TSamsungSMARTSupport.InnerIsErrorAvailable(
  const SMARTList: TSMARTValueList): Boolean; 
begin
  result := true;
end;

function TSamsungSMARTSupport.InnerIsCautionAvailable(
  const SMARTList: TSMARTValueList): Boolean;
begin
  result :=
    IsEntryAvailable(EntryID1, SMARTList) or
    IsEntryAvailable(EntryID2, SMARTList);
end;

function TSamsungSMARTSupport.InnerIsError(
  const SMARTList: TSMARTValueList): TSMARTErrorResult;
begin
  result.Override := false;
  result.Status :=
    InnerCommonIsError(EntryID1, SMARTList).Status or
    InnerCommonIsError(EntryID2, SMARTList).Status;
end;

function TSamsungSMARTSupport.InnerIsCaution(
  const SMARTList: TSMARTValueList): TSMARTErrorResult;
var
  Entry1Result, Entry2Result: TSMARTErrorResult;
begin
  Entry1Result :=
    InnerCommonIsCaution(EntryID1, SMARTList, CommonLifeThreshold);
  Entry2Result :=
    InnerCommonIsCaution(EntryID2, SMARTList, CommonLifeThreshold);
  result.Override := Entry1Result.Override;
  result.Status := Entry1Result.Status or Entry2Result.Status;
end;

function TSamsungSMARTSupport.IsWriteValueSupported(
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

function TSamsungSMARTSupport.GetSMARTInterpreted(
  const SMARTList: TSMARTValueList): TSMARTInterpreted;
const
  ReadError = true;
  EraseError = false;

  UsedHourID = $09;
  ThisErrorType = ReadError;
  ErrorID = $BB;
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

function TSamsungSMARTSupport.GetTotalWrite(
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
    LBAToMB(SMARTList.ExceptionFreeGetRAWByID(WriteID));
end;

end.

