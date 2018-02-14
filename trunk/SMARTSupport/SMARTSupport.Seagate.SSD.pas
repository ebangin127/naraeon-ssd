// Ported CrystalDiskInfo (The MIT License, http://crystalmark.info)
unit SMARTSupport.Seagate.SSD;

interface

uses
  BufferInterpreter, Device.SMART.List, SMARTSupport, Support;

type
  TSeagateSSDSMARTSupport = class(TSMARTSupport)
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
  end;

implementation

{ TSeagateSSDSMARTSupport }

function TSeagateSSDSMARTSupport.GetTypeName: String;
begin
  result := 'SmartSsd';
end;

function TSeagateSSDSMARTSupport.IsInsufficientSMART: Boolean;
begin
  result := false;
end;

function TSeagateSSDSMARTSupport.IsSSD: Boolean;
begin
  result := true;
end;

function TSeagateSSDSMARTSupport.IsThisStorageMine(
  const IdentifyDevice: TIdentifyDeviceResult;
  const SMARTList: TSMARTValueList): Boolean;
begin
  result :=
    (Pos('STT', IdentifyDevice.Model) = 0) and
    (Pos('ST', IdentifyDevice.Model) > 0) and
    (CheckIsSSDInCommonWay(IdentifyDevice));
end;

function TSeagateSSDSMARTSupport.IsWriteValueSupported(
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

function TSeagateSSDSMARTSupport.GetSMARTInterpreted(
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

function TSeagateSSDSMARTSupport.GetTotalWrite(
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

