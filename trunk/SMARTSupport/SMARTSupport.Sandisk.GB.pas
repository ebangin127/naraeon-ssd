// Ported CrystalDiskInfo (The MIT License, http://crystalmark.info)
unit SMARTSupport.Sandisk.GB;

interface

uses
  BufferInterpreter, Device.SMART.List, SMARTSupport, Support;

type
  TSandiskGBSMARTSupport = class(TSMARTSupport)
  private
    function ModelHasSandiskString(const Model: String): Boolean;
    function SMARTHasSandiskCharacteristics(
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

{ TSandiskSMARTSupport }

function TSandiskGBSMARTSupport.GetTypeName: String;
begin
  result := 'SmartSanDiskGb';
end;

function TSandiskGBSMARTSupport.IsInsufficientSMART: Boolean;
begin
  result := false;
end;

function TSandiskGBSMARTSupport.IsSSD: Boolean;
begin
  result := true;
end;

function TSandiskGBSMARTSupport.IsThisStorageMine(
  const IdentifyDevice: TIdentifyDeviceResult;
  const SMARTList: TSMARTValueList): Boolean;
begin
  result :=
    ModelHasSandiskString(IdentifyDevice.Model) and
    SMARTHasSandiskCharacteristics(SMARTList);
end;

function TSandiskGBSMARTSupport.ModelHasSandiskString(const Model: String):
  Boolean;
begin
  result := Find('SanDisk', Model);
end;

function TSandiskGBSMARTSupport.SMARTHasSandiskCharacteristics(
  const SMARTList: TSMARTValueList): Boolean;
const
  TotalGBWritten = $E9;
begin
  result := true;
  try
    SMARTList.GetIndexByID(TotalGBWritten);
  except
    on E: EEntryNotFound do
      result := false;
    else raise;
  end;
end;

function TSandiskGBSMARTSupport.ErrorCheckedGetLife(
  const SMARTList: TSMARTValueList): Integer;
begin
  result := SMARTList[SMARTList.GetIndexByID($E8)].Current;
end;

function TSandiskGBSMARTSupport.InnerIsErrorAvailable(
  const SMARTList: TSMARTValueList): Boolean; 
begin
  result := true;
end;

function TSandiskGBSMARTSupport.InnerIsCautionAvailable(
  const SMARTList: TSMARTValueList): Boolean;
begin
  result := IsEntryAvailable(EntryID, SMARTList);
end;

function TSandiskGBSMARTSupport.InnerIsError(
  const SMARTList: TSMARTValueList): TSMARTErrorResult;
begin
  result.Override := false;
  result.Status :=
    InnerCommonIsError(EntryID, SMARTList).Status;
end;

function TSandiskGBSMARTSupport.InnerIsCaution(
  const SMARTList: TSMARTValueList): TSMARTErrorResult; 
begin
  result := InnerCommonIsCaution(EntryID, SMARTList, CommonLifeThreshold);
end;

function TSandiskGBSMARTSupport.IsWriteValueSupported(
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

function TSandiskGBSMARTSupport.GetSMARTInterpreted(
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

function TSandiskGBSMARTSupport.GetTotalWrite(
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

