// NOT Ported CDI
unit SMARTSupport.NVMe.Intel;

interface

uses
  SysUtils,
  BufferInterpreter, Device.SMART.List, SMARTSupport, Support;

type
  TIntelNVMeSMARTSupport = class(TSMARTSupport)
  private
    function ModelHasIntelString(const Model: String): Boolean;
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

{ TIntelNVMeSMARTSupport }

function TIntelNVMeSMARTSupport.GetTypeName: String;
begin
  result := 'SmartNvme';
end;

function TIntelNVMeSMARTSupport.IsInsufficientSMART: Boolean;
begin
  result := false;
end;

function TIntelNVMeSMARTSupport.IsSSD: Boolean;
begin
  result := true;
end;

function TIntelNVMeSMARTSupport.IsThisStorageMine(
  const IdentifyDevice: TIdentifyDeviceResult;
  const SMARTList: TSMARTValueList): Boolean;
begin
  result :=
    (IdentifyDevice.StorageInterface = TStorageInterface.NVMe) and
    (ModelHasIntelString(IdentifyDevice.Model));
end;

function TIntelNVMeSMARTSupport.ModelHasIntelString(const Model: String):
  Boolean;
var
  ModelInUpperCase: String;
begin
  ModelInUpperCase := UpperCase(Model);
  result := FindAtFirst('INTEL', ModelInUpperCase);
end;

function TIntelNVMeSMARTSupport.ErrorCheckedGetLife(
  const SMARTList: TSMARTValueList): Integer;
begin
  result := SMARTList[SMARTList.GetIndexByID(3)].RAW;
end;

function TIntelNVMeSMARTSupport.IsWriteValueSupported(
  const SMARTList: TSMARTValueList): Boolean;
const
  WriteID = $05;
begin
  try
    SMARTList.GetIndexByID(WriteID);
    result := true;
  except
    result := false;
  end;
end;

function TIntelNVMeSMARTSupport.GetSMARTInterpreted(
  const SMARTList: TSMARTValueList): TSMARTInterpreted;
const
  ReadError = true;
  EraseError = false;

  UsedHourID = $0C;
  ThisErrorType = EraseError;
  ErrorID = $0E;
  ReplacedSectorsID = $00;
  CriticalErrorID = $01;
begin
  FillChar(result, SizeOf(result), 0);
  result.UsedHour := SMARTList.ExceptionFreeGetRAWByID(UsedHourID);
  result.ReadEraseError.TrueReadErrorFalseEraseError := ThisErrorType;
  result.ReadEraseError.Value :=
    SMARTList.ExceptionFreeGetRAWByID(ErrorID);
  result.TotalWrite := GetTotalWrite(SMARTList);
  result.SMARTAlert.CriticalError :=
    SMARTList.GetRAWByID(CriticalErrorID) > 0;
end;

function TIntelNVMeSMARTSupport.GetTotalWrite(
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

  WriteID = $05;
begin
  result.InValue.TrueHostWriteFalseNANDWrite :=
    HostWrite;
  result.InValue.ValueInMiB :=
    LBAToMB(SMARTList.ExceptionFreeGetRAWByID(WriteID));
end;

end.
