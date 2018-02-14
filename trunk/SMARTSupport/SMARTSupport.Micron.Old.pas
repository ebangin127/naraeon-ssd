// Ported CrystalDiskInfo (The MIT License, http://crystalmark.info)
unit SMARTSupport.Micron.Old;

interface

uses
  SysUtils,
  BufferInterpreter, Device.SMART.List, SMARTSupport, Support;

type
  TOldMicronSMARTSupport = class(TSMARTSupport)
  private
    function ModelHasMicronString(const Model: String): Boolean;
    function SMARTHasMicronCharacteristics(
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

{ TOldMicronSMARTSupport }

function TOldMicronSMARTSupport.GetTypeName: String;
begin
  result := 'SmartMicron';
end;

function TOldMicronSMARTSupport.IsInsufficientSMART: Boolean;
begin
  result := false;
end;

function TOldMicronSMARTSupport.IsSSD: Boolean;
begin
  result := true;
end;

function TOldMicronSMARTSupport.IsThisStorageMine(
  const IdentifyDevice: TIdentifyDeviceResult;
  const SMARTList: TSMARTValueList): Boolean;
begin
  result :=
    ModelHasMicronString(IdentifyDevice.Model) or
    SMARTHasMicronCharacteristics(SMARTList);
end;

function TOldMicronSMARTSupport.ModelHasMicronString(const Model: String):
  Boolean;
var
  ModelInUpperCase: String;
begin
  ModelInUpperCase := UpperCase(Model);
  result :=
    Find('CRUCIAL', ModelInUpperCase) or
    Find('MICRON', ModelInUpperCase) or
    Find('P600', ModelInUpperCase) or
    Find('C600', ModelInUpperCase) or
    Find('M6-', ModelInUpperCase) or
    Find('M600', ModelInUpperCase) or
    Find('P500', ModelInUpperCase) or
    Find('C500', ModelInUpperCase) or
    Find('M5-', ModelInUpperCase) or
    Find('M500', ModelInUpperCase) or
    Find('P400', ModelInUpperCase) or
    Find('C400', ModelInUpperCase) or
    Find('M4-', ModelInUpperCase) or
    Find('M400', ModelInUpperCase) or
    Find('P300', ModelInUpperCase) or
    Find('C300', ModelInUpperCase) or
    Find('C300', ModelInUpperCase) or
    Find('M3-', ModelInUpperCase) or
    Find('M300', ModelInUpperCase);
end;

function TOldMicronSMARTSupport.SMARTHasMicronCharacteristics(
  const SMARTList: TSMARTValueList): Boolean;
begin
	result :=
    (SMARTList.Count >= 11) and
    (SMARTList[0].Id = $01) and
    (SMARTList[1].Id = $05) and
    (SMARTList[2].Id = $09) and
    (SMARTList[3].Id = $0C) and
    (SMARTList[4].Id = $AA) and
    (SMARTList[5].Id = $AB) and
    (SMARTList[6].Id = $AC) and
    (SMARTList[7].Id = $AD) and
    (SMARTList[8].Id = $AE) and
    (SMARTList[9].Id = $B5) and
    (SMARTList[10].Id = $B7);
end;

function TOldMicronSMARTSupport.ErrorCheckedGetLife(
  const SMARTList: TSMARTValueList): Integer;
begin
  try
    result := SMARTList[SMARTList.GetIndexByID($F2)].Current;
  except
    on E: EEntryNotFound do
      result := SMARTList[SMARTList.GetIndexByID($CA)].Current
    else raise;
  end;
end;

function TOldMicronSMARTSupport.InnerIsErrorAvailable(
  const SMARTList: TSMARTValueList): Boolean; 
begin
  result := true;
end;

function TOldMicronSMARTSupport.InnerIsCautionAvailable(
  const SMARTList: TSMARTValueList): Boolean;
begin
  result := IsEntryAvailable(EntryID, SMARTList);
end;

function TOldMicronSMARTSupport.InnerIsError(
  const SMARTList: TSMARTValueList): TSMARTErrorResult;
begin
  result.Override := false;
  result.Status :=
    InnerCommonIsError(EntryID, SMARTList).Status;
end;

function TOldMicronSMARTSupport.InnerIsCaution(
  const SMARTList: TSMARTValueList): TSMARTErrorResult; 
begin
  result := InnerCommonIsCaution(EntryID, SMARTList, CommonLifeThreshold);
end;

function TOldMicronSMARTSupport.IsWriteValueSupported(
  const SMARTList: TSMARTValueList): Boolean;
const
  WriteID1 = $F5;
  WriteID2 = $F6;
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

function TOldMicronSMARTSupport.GetSMARTInterpreted(
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

function TOldMicronSMARTSupport.GetTotalWrite(
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

  WriteID1 = $F5;
  WriteID2 = $F6;
  PageToLBA = 16;
begin
  try
    result.InValue.ValueInMiB :=
      LBAToMB(SMARTList.GetRAWByID(WriteID1) * PageToLBA);
    result.InValue.TrueHostWriteFalseNANDWrite :=
      NANDWrite;
  except
    try
      result.InValue.ValueInMiB :=
        LBAToMB(SMARTList.ExceptionFreeGetRAWByID(WriteID2));
      result.InValue.TrueHostWriteFalseNANDWrite :=
        HostWrite;
    except
      result.InValue.ValueInMiB := 0;
    end;
  end;
end;

end.
