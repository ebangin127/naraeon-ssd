// Ported CrystalDiskInfo (The MIT License, http://crystalmark.info)
unit SMARTSupport.Kingston;

interface

uses
  BufferInterpreter, Device.SMART.List, SMARTSupport, Support;

type
  TKingstonSMARTSupport = class(TSMARTSupport)
  private
    function GetTotalWrite(const SMARTList: TSMARTValueList): TTotalWrite;
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
  end;

implementation

{ TKingstonSMARTSupport }

function TKingstonSMARTSupport.GetTypeName: String;
begin
  result := 'SmartKingston';
end;

function TKingstonSMARTSupport.IsInsufficientSMART: Boolean;
begin
  result := false;
end;

function TKingstonSMARTSupport.IsSSD: Boolean;
begin
  result := true;
end;

function TKingstonSMARTSupport.IsThisStorageMine(
  const IdentifyDevice: TIdentifyDeviceResult;
  const SMARTList: TSMARTValueList): Boolean;
begin
	// 2015/11/29
	result :=
    (SMARTList.Count >= 10) and
    (SMARTList[0].Id = $01) and
    (SMARTList[1].Id = $02) and
    (SMARTList[2].Id = $03) and
    (SMARTList[3].Id = $05) and
    (SMARTList[4].Id = $07) and
    (SMARTList[5].Id = $08) and
    (SMARTList[6].Id = $09) and
    (SMARTList[7].Id = $0A) and
    (SMARTList[8].Id = $0C) and
    (SMARTList[9].Id = $A8);
end;

function TKingstonSMARTSupport.ErrorCheckedGetLife(
  const SMARTList: TSMARTValueList): Integer;
begin
  result := SMARTList[SMARTList.GetIndexByID($E7)].Current;
end;

function TKingstonSMARTSupport.IsWriteValueSupported(
  const SMARTList: TSMARTValueList): Boolean;
const
  WriteID1 = $F1;
  WriteID2 = $E9;
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

function TKingstonSMARTSupport.GetSMARTInterpreted(
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

function TKingstonSMARTSupport.GetTotalWrite(
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
  WriteID2 = $E9;
begin
  try
    result.InValue.ValueInMiB :=
      LBAToMB(SMARTList.GetRAWByID(WriteID1));
    result.InValue.TrueHostWriteFalseNANDWrite :=
      HostWrite;
  except
    try
      result.InValue.ValueInMiB :=
        GBToMB(SMARTList.ExceptionFreeGetRAWByID(WriteID2));
      result.InValue.TrueHostWriteFalseNANDWrite :=
        NANDWrite;
    except
      result.InValue.ValueInMiB := 0;
    end;
  end;
end;

end.

