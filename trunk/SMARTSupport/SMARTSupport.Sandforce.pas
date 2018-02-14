// Ported CrystalDiskInfo (The MIT License, http://crystalmark.info)
unit SMARTSupport.Sandforce;

interface

uses
  BufferInterpreter, Device.SMART.List, SMARTSupport, Support;

type
  TSandforceSMARTSupport = class(TSMARTSupport)
  private
    function ModelHasSandforceString(const Model: String): Boolean;
    function SMARTHasSandforceCharacteristics(
      const SMARTList: TSMARTValueList): Boolean;
    function CheckSpecificCharacteristics1(
      const SMARTList: TSMARTValueList): Boolean;
    function CheckSpecificCharacteristics2(
      const SMARTList: TSMARTValueList): Boolean;
    function CheckToshibaSandforceCharacteristics(
      const SMARTList: TSMARTValueList): Boolean;
    function IsNotSandforceBug(const Entry: TSMARTValueEntry): Boolean;
    function GetTotalWrite(const SMARTList: TSMARTValueList): TTotalWrite;
    const
      EntryID = $E7;
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

{ TSandforceSMARTSupport }

function TSandforceSMARTSupport.GetTypeName: String;
begin
  result := 'SmartSandForce';
end;

function TSandforceSMARTSupport.IsSSD: Boolean;
begin
  result := true;
end;

function TSandforceSMARTSupport.IsThisStorageMine(
  const IdentifyDevice: TIdentifyDeviceResult;
  const SMARTList: TSMARTValueList): Boolean;
begin
  result :=
    ModelHasSandforceString(IdentifyDevice.Model) or
    SMARTHasSandforceCharacteristics(SMARTList);
end;

function TSandforceSMARTSupport.ModelHasSandforceString(const Model: String):
  Boolean;
begin
  result := Find('SandForce', Model);
end;

function TSandforceSMARTSupport.SMARTHasSandforceCharacteristics(
  const SMARTList: TSMARTValueList): Boolean;
begin
  result :=
    CheckSpecificCharacteristics1(SMARTList) or
    CheckSpecificCharacteristics2(SMARTList) or
    CheckToshibaSandforceCharacteristics(SMARTList);
end;

function TSandforceSMARTSupport.CheckSpecificCharacteristics1(
  const SMARTList: TSMARTValueList): Boolean;
begin
	result :=
    (SMARTList.Count >= 7) and
    (SMARTList[0].Id = $01) and
    (SMARTList[1].Id = $05) and
    (SMARTList[2].Id = $09) and
    (SMARTList[3].Id = $0C) and
    (SMARTList[4].Id = $0D) and
    (SMARTList[5].Id = $64) and
    (SMARTList[6].Id = $AA);
end;

function TSandforceSMARTSupport.CheckSpecificCharacteristics2(
  const SMARTList: TSMARTValueList): Boolean;
begin
	result :=
    (SMARTList.Count >= 6) and
    (SMARTList[0].Id = $01) and
    (SMARTList[1].Id = $05) and
    (SMARTList[2].Id = $09) and
    (SMARTList[3].Id = $0C) and
    (SMARTList[4].Id = $AB) and
    (SMARTList[5].Id = $AC);
end;

function TSandforceSMARTSupport.CheckToshibaSandforceCharacteristics(
  const SMARTList: TSMARTValueList): Boolean;
begin
	// TOSHIBA + SandForce
	// http://crystalmark.info/bbs/c-board.cgi?cmd=one;no=1116;id=diskinfo#1116
	// http://crystalmark.info/bbs/c-board.cgi?cmd=one;no=1136;id=diskinfo#1136
	result :=
    (SMARTList.Count >= 16) and
    (SMARTList[0].Id = $01) and
    (SMARTList[1].Id = $02) and
    (SMARTList[2].Id = $03) and
    (SMARTList[3].Id = $05) and
    (SMARTList[4].Id = $07) and
    (SMARTList[5].Id = $08) and
    (SMARTList[6].Id = $09) and
    (SMARTList[7].Id = $0A) and
    (SMARTList[8].Id = $0C) and
    (SMARTList[9].Id = $A7) and
    (SMARTList[10].Id = $A8) and
    (SMARTList[11].Id = $A9) and
    (SMARTList[12].Id = $AA) and
    (SMARTList[13].Id = $AD) and
    (SMARTList[14].Id = $AF) and
    (SMARTList[15].Id = $B1);
end;

function TSandforceSMARTSupport.ErrorCheckedGetLife(
  const SMARTList: TSMARTValueList): Integer;
begin
  result := SMARTList[SMARTList.GetIndexByID($E7)].Current;
end;

function TSandforceSMARTSupport.InnerIsErrorAvailable(
  const SMARTList: TSMARTValueList): Boolean; 
begin
  result := true;
end;

function TSandforceSMARTSupport.InnerIsCautionAvailable(
  const SMARTList: TSMARTValueList): Boolean; 
begin
  result := true;
end;

function TSandforceSMARTSupport.IsInsufficientSMART: Boolean;
begin
  result := false;
end;

function TSandforceSMARTSupport.IsNotSandforceBug(
  const Entry: TSMARTValueEntry): Boolean;
begin
  result := 
    (Entry.ID = 1) and
    (Entry.Current = 0) and
    ((Entry.RAW and $FFFF) = 0);
end;

function TSandforceSMARTSupport.InnerIsError(
  const SMARTList: TSMARTValueList): TSMARTErrorResult;
var
  CurrentEntry: TSMARTValueEntry;
begin
  result.Override := true;
  result.Status := false;
  for CurrentEntry in SMARTList do
    if not IsNotSandforceBug(CurrentEntry) then
      if CurrentEntry.ID <> $C2 then
        if CurrentEntry.Threshold > CurrentEntry.Current then
        begin
          result.Status := true;
          exit;
        end;
  result.Status := result.Status or
    InnerCommonIsError(EntryID, SMARTList).Status;
end;

function TSandforceSMARTSupport.InnerIsCaution(
  const SMARTList: TSMARTValueList): TSMARTErrorResult; 
begin
  result := InnerCommonIsCaution(EntryID, SMARTList, CommonLifeThreshold);
end;

function TSandforceSMARTSupport.IsWriteValueSupported(
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

function TSandforceSMARTSupport.GetSMARTInterpreted(
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
  result.UsedHour :=
    UINT16(SMARTList.ExceptionFreeGetRAWByID(UsedHourID));
  result.ReadEraseError.TrueReadErrorFalseEraseError := ReadError;
  result.ReadEraseError.Value :=
    SMARTList.ExceptionFreeGetRAWByID(ErrorID);
  result.ReplacedSectors :=
    SMARTList.ExceptionFreeGetRAWByID(ReplacedSectorsID);
  result.TotalWrite := GetTotalWrite(SMARTList);
end;

function TSandforceSMARTSupport.GetTotalWrite(
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

