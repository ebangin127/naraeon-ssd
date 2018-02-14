// Ported CrystalDiskInfo (The MIT License, http://crystalmark.info)
unit SMARTSupport.Plextor;

interface

uses
  BufferInterpreter, Device.SMART.List, SMARTSupport, Support;

type
  TPlextorSMARTSupport = class(TSMARTSupport)
  private
    function IsCFDSSD(const Model: String): Boolean;
    function IsPlextorSSD(const IdentifyDevice: TIdentifyDeviceResult;
      const SMARTList: TSMARTValueList): Boolean;
    function ModelHasPlextorString(const Model: String): Boolean;
    function SMARTHasPlextorCharacteristics(
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

{ TPlextorSMARTSupport }

function TPlextorSMARTSupport.GetTypeName: String;
begin
  result := 'SmartPlextor';
end;

function TPlextorSMARTSupport.IsSSD: Boolean;
begin
  result := true;
end;

function TPlextorSMARTSupport.IsThisStorageMine(
  const IdentifyDevice: TIdentifyDeviceResult;
  const SMARTList: TSMARTValueList): Boolean;
begin
  result :=
    IsPlextorSSD(IdentifyDevice, SMARTList) or
    IsCFDSSD(IdentifyDevice.Model);
end;

function TPlextorSMARTSupport.IsCFDSSD(const Model: String):
  Boolean;
begin
  result :=
    FindAtFirst('CSSD-S6T128NM3PQ', Model) or
    FindAtFirst('CSSD-S6T256NM3PQ', Model) or
    FindAtFirst('CSSD-S6T512NM3PQ', Model);
end;

function TPlextorSMARTSupport.IsInsufficientSMART: Boolean;
begin
  result := false;
end;

function TPlextorSMARTSupport.IsPlextorSSD(
  const IdentifyDevice: TIdentifyDeviceResult;
  const SMARTList: TSMARTValueList): Boolean;
begin
  result :=
    ModelHasPlextorString(IdentifyDevice.Model) or
    SMARTHasPlextorCharacteristics(SMARTList);
end;

function TPlextorSMARTSupport.ModelHasPlextorString(const Model: String):
  Boolean;
begin
  result := FindAtFirst('PLEXTOR', Model);
end;

function TPlextorSMARTSupport.SMARTHasPlextorCharacteristics(
  const SMARTList: TSMARTValueList): Boolean;
begin
	// 2012/10/10
	// http://crystalmark.info/bbs/c-board.cgi?cmd=one;no=739;id=diskinfo#739
	// http://crystalmark.info/bbs/c-board.cgi?cmd=one;no=829;id=diskinfo#829
	result :=
    (SMARTList.Count >= 8) and
    (SMARTList[0].Id = $01) and
    (SMARTList[1].Id = $05) and
    (SMARTList[2].Id = $09) and
    (SMARTList[3].Id = $0C) and
    (SMARTList[4].Id = $B1) and
    (SMARTList[5].Id = $B2) and
    (SMARTList[6].Id = $B5) and
    (SMARTList[7].Id = $B6);
end;

function TPlextorSMARTSupport.ErrorCheckedGetLife(
  const SMARTList: TSMARTValueList): Integer;
begin
  result := SMARTList[SMARTList.GetIndexByID($E8)].Current;
end;

function TPlextorSMARTSupport.InnerIsErrorAvailable(
  const SMARTList: TSMARTValueList): Boolean; 
begin
  result := true;
end;

function TPlextorSMARTSupport.InnerIsCautionAvailable(
  const SMARTList: TSMARTValueList): Boolean;
begin
  result := IsEntryAvailable(EntryID, SMARTList);
end;

function TPlextorSMARTSupport.InnerIsError(
  const SMARTList: TSMARTValueList): TSMARTErrorResult;
begin
  result.Override := false;
  result.Status :=
    InnerCommonIsError(EntryID, SMARTList).Status;
end;

function TPlextorSMARTSupport.InnerIsCaution(
  const SMARTList: TSMARTValueList): TSMARTErrorResult; 
begin
  result := InnerCommonIsCaution(EntryID, SMARTList, CommonLifeThreshold);
end;

function TPlextorSMARTSupport.IsWriteValueSupported(
  const SMARTList: TSMARTValueList): Boolean;
const
  WriteID1 = $B1;
  WriteID2 = $E9;
  WriteID3 = $F1;
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
  if not result then
  begin
    try
      SMARTList.GetIndexByID(WriteID3);
      result := true;
    except
      result := false;
    end;
  end;
end;

function TPlextorSMARTSupport.GetSMARTInterpreted(
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

function TPlextorSMARTSupport.GetTotalWrite(
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

  WriteID1 = $B1;
  WriteID2 = $E9;
  WriteID3 = $F1;
begin
  try
    result.InValue.ValueInMiB :=
      SMARTList.GetRAWByID(WriteID1) * 128;
    result.InValue.TrueHostWriteFalseNANDWrite :=
      NANDWrite;
  except
    try
      result.InValue.ValueInMiB :=
        GBToMB(SMARTList.GetRAWByID(WriteID2));
      result.InValue.TrueHostWriteFalseNANDWrite :=
        NANDWrite;
    except
      try
        result.InValue.ValueInMiB :=
          SMARTList.ExceptionFreeGetRAWByID(WriteID3) * 32;
        result.InValue.TrueHostWriteFalseNANDWrite :=
          HostWrite;
      except
        result.InValue.ValueInMiB := 0;
      end;
    end;
  end;
end;

end.
