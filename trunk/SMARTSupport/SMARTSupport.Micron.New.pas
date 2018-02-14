// Ported CrystalDiskInfo (The MIT License, http://crystalmark.info)
unit SMARTSupport.Micron.New;

interface

uses
  BufferInterpreter, Device.SMART.List, SMARTSupport, Support;

type
  TNewMicronSMARTSupport = class(TSMARTSupport)
  private
    function CheckMU014Characteristics(
      const SMARTList: TSMARTValueList): Boolean;
    function CheckMU02Characteristics(
      const SMARTList: TSMARTValueList): Boolean;
    function GetTotalWrite(const SMARTList: TSMARTValueList): TTotalWrite;
    const
      EntryID = $CA;
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

{ TNewMicronSMARTSupport }

function TNewMicronSMARTSupport.GetTypeName: String;
begin
  result := 'SmartMicronMU02';
end;

function TNewMicronSMARTSupport.IsInsufficientSMART: Boolean;
begin
  result := false;
end;

function TNewMicronSMARTSupport.IsSSD: Boolean;
begin
  result := true;
end;

function TNewMicronSMARTSupport.IsThisStorageMine(
  const IdentifyDevice: TIdentifyDeviceResult;
  const SMARTList: TSMARTValueList): Boolean;
begin
  result :=
    CheckMU02Characteristics(SMARTList) or
    CheckMU014Characteristics(SMARTList);
end;

function TNewMicronSMARTSupport.CheckMU02Characteristics(
  const SMARTList: TSMARTValueList): Boolean;
begin
	// Crucial BX100 MU02 2015/11/26
	result :=
    (SMARTList.Count >= 11) and
    (SMARTList[0].Id = $01) and
    (SMARTList[1].Id = $05) and
    (SMARTList[2].Id = $09) and
    (SMARTList[3].Id = $0C) and
    (SMARTList[4].Id = $A0) and
    (SMARTList[5].Id = $A1) and
    (SMARTList[6].Id = $A3) and
    (SMARTList[7].Id = $A4) and
    (SMARTList[8].Id = $A5) and
    (SMARTList[9].Id = $A6) and
    (SMARTList[10].Id = $A7);
end;

function TNewMicronSMARTSupport.CheckMU014Characteristics(
  const SMARTList: TSMARTValueList): Boolean;
begin
	// Crucial BX200 MU01.4 2015/11/26
	result :=
    (SMARTList.Count >= 11) and
    (SMARTList[0].Id = $01) and
    (SMARTList[1].Id = $05) and
    (SMARTList[2].Id = $09) and
    (SMARTList[3].Id = $0C) and
    (SMARTList[4].Id = $A0) and
    (SMARTList[5].Id = $A1) and
    (SMARTList[6].Id = $A3) and
    (SMARTList[7].Id = $94) and
    (SMARTList[8].Id = $95) and
    (SMARTList[9].Id = $96) and
    (SMARTList[10].Id = $97);
end;

function TNewMicronSMARTSupport.ErrorCheckedGetLife(
  const SMARTList: TSMARTValueList): Integer;
begin
  result := SMARTList[SMARTList.GetIndexByID($A9)].Current;
end;

function TNewMicronSMARTSupport.InnerIsErrorAvailable(
  const SMARTList: TSMARTValueList): Boolean; 
begin
  result := true;
end;

function TNewMicronSMARTSupport.InnerIsCautionAvailable(
  const SMARTList: TSMARTValueList): Boolean;
begin
  result := IsEntryAvailable(EntryID, SMARTList);
end;

function TNewMicronSMARTSupport.InnerIsError(
  const SMARTList: TSMARTValueList): TSMARTErrorResult;
begin
  result.Override := false;
  result.Status :=
    InnerCommonIsError(EntryID, SMARTList).Status;
end;

function TNewMicronSMARTSupport.InnerIsCaution(
  const SMARTList: TSMARTValueList): TSMARTErrorResult; 
begin
  result := InnerCommonIsCaution(EntryID, SMARTList, CommonLifeThreshold);
end;

function TNewMicronSMARTSupport.IsWriteValueSupported(
  const SMARTList: TSMARTValueList): Boolean;
const
  WriteID1 = $F1;
  WriteID2 = $F5;
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

function TNewMicronSMARTSupport.GetSMARTInterpreted(
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

function TNewMicronSMARTSupport.GetTotalWrite(
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

  WriteID1 = $F1;
  WriteID2 = $F5;
begin
  result.InValue.TrueHostWriteFalseNANDWrite :=
    HostWrite;
  try
    result.InValue.ValueInMiB :=
      LBAToMB(SMARTList.GetRAWByID(WriteID1));
  except
    try
      result.InValue.ValueInMiB :=
        LBAToMB(SMARTList.ExceptionFreeGetRAWByID(WriteID2));
    except
      result.InValue.ValueInMiB := 0;
    end;
  end;
end;

end.

