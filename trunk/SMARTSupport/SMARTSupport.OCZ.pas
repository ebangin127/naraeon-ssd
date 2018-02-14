// Ported CrystalDiskInfo (The MIT License, http://crystalmark.info)
unit SMARTSupport.OCZ;

interface

uses
  BufferInterpreter, Device.SMART.List, SMARTSupport, Support;

type
  TOCZSMARTSupport = class(TSMARTSupport)
  private
    function IsOCZAndNotVector(const IdentifyDevice: TIdentifyDeviceResult;
      const SMARTList: TSMARTValueList): Boolean;
    function IsOCZTrion(const Model: String): Boolean;
    function ModelHasOCZString(const Model: String): Boolean;
    function SMARTHasOCZCharacteristics(
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

{ TOCZSMARTSupport }

function TOCZSMARTSupport.GetTypeName: String;
begin
  result := 'SmartOcz';
end;

function TOCZSMARTSupport.IsSSD: Boolean;
begin
  result := true;
end;

function TOCZSMARTSupport.IsThisStorageMine(
  const IdentifyDevice: TIdentifyDeviceResult;
  const SMARTList: TSMARTValueList): Boolean;
begin
  result :=
    IsOCZTrion(IdentifyDevice.Model) or
    IsOCZAndNotVector(IdentifyDevice, SMARTList);
end;

function TOCZSMARTSupport.IsOCZTrion(const Model: String):
  Boolean;
begin
	// OCZ-TRION100 2015/11/25
  result := Find('OCZ-TRION', Model);
end;

function TOCZSMARTSupport.IsInsufficientSMART: Boolean;
begin
  result := false;
end;

function TOCZSMARTSupport.IsOCZAndNotVector(
  const IdentifyDevice: TIdentifyDeviceResult;
  const SMARTList: TSMARTValueList): Boolean;
begin
  result :=
    ModelHasOCZString(IdentifyDevice.Model) and
    SMARTHasOCZCharacteristics(SMARTList);
end;

function TOCZSMARTSupport.ModelHasOCZString(const Model: String):
  Boolean;
begin
  result := Find('OCZ', Model);
end;

function TOCZSMARTSupport.SMARTHasOCZCharacteristics(
  const SMARTList: TSMARTValueList): Boolean;
begin
	// 2012/3/11
	// OCZ-PETROL - http://crystalmark.info/bbs/c-board.cgi?cmd=one;no=553;id=diskinfo#553
	// OCZ-OCTANE S2 - http://crystalmark.info/bbs/c-board.cgi?cmd=one;no=577;id=diskinfo#577
	// OCZ-VERTEX 4 - http://imageshack.us/a/img269/7506/ocz2.png
	result :=
    (SMARTList.Count >= 8) and
    (SMARTList[0].Id = $01) and
    (SMARTList[1].Id = $03) and
    (SMARTList[2].Id = $04) and
    (SMARTList[3].Id = $05) and
    (SMARTList[4].Id = $09) and
    (SMARTList[5].Id = $0C) and
    (SMARTList[6].Id = $E8) and
    (SMARTList[7].Id = $E9);
end;

function TOCZSMARTSupport.ErrorCheckedGetLife(
  const SMARTList: TSMARTValueList): Integer;
begin
  result := SMARTList[SMARTList.GetIndexByID($E9)].Current;
end;

function TOCZSMARTSupport.InnerIsErrorAvailable(
  const SMARTList: TSMARTValueList): Boolean; 
begin
  result := true;
end;

function TOCZSMARTSupport.InnerIsCautionAvailable(
  const SMARTList: TSMARTValueList): Boolean;
begin
  result := IsEntryAvailable(EntryID, SMARTList);
end;

function TOCZSMARTSupport.InnerIsError(
  const SMARTList: TSMARTValueList): TSMARTErrorResult;
begin
  result.Override := false;
  result.Status :=
    InnerCommonIsError(EntryID, SMARTList).Status;
end;

function TOCZSMARTSupport.InnerIsCaution(
  const SMARTList: TSMARTValueList): TSMARTErrorResult; 
begin
  result := InnerCommonIsCaution(EntryID, SMARTList, CommonLifeThreshold);
end;

function TOCZSMARTSupport.IsWriteValueSupported(
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

function TOCZSMARTSupport.GetSMARTInterpreted(
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
  result.ReadEraseError.Value :=
    SMARTList.ExceptionFreeGetRAWByID(ErrorID);
  result.ReplacedSectors :=
    SMARTList.ExceptionFreeGetRAWByID(ReplacedSectorsID);
  result.TotalWrite := GetTotalWrite(SMARTList);
end;

function TOCZSMARTSupport.GetTotalWrite(
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

