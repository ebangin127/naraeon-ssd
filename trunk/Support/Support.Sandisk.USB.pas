unit Support.Sandisk.USB;

interface

uses
  SysUtils, Math,
  Support, Device.SMART.List;

type
  TSandiskUSBNSTSupport = class sealed(TNSTSupport)
  private
    InterpretingSMARTValueList: TSMARTValueList;
    function GetFullSupport: TSupportStatus;
    function GetTotalWrite: TTotalWrite;
    function IsProductOfSandisk: Boolean;
    function IsU100: Boolean;
  public
    function GetSupportStatus: TSupportStatus; override;
    function GetSMARTInterpreted(SMARTValueList: TSMARTValueList):
      TSMARTInterpreted; override;
  end;

implementation

{ TSandiskNSTSupport }

function TSandiskUSBNSTSupport.IsU100: Boolean;
begin
  result := (Pos('SANDISK', Identify.Model) > 0) and
    ((Pos('U100', Identify.Model) > 0) or (Pos('PSSD', Identify.Model) > 0));
end;

function TSandiskUSBNSTSupport.IsProductOfSandisk: Boolean;
begin
  result := IsU100;
end;

function TSandiskUSBNSTSupport.GetFullSupport: TSupportStatus;
begin
  result.Supported := Supported;
  result.FirmwareUpdate := true;
  result.TotalWriteType := TTotalWriteType.WriteSupportedAsValue;
end;

function TSandiskUSBNSTSupport.GetSupportStatus: TSupportStatus;
begin
  result.Supported := NotSupported;
  if IsProductOfSandisk then
    result := GetFullSupport;
end;

function TSandiskUSBNSTSupport.GetTotalWrite: TTotalWrite;
const
  LBAtoMiB: Double = 1/2 * 1/1024;
  IDOfHostWrite = 241;
var
  RAWValue: UInt64;
begin
  result.InValue.TrueHostWriteFalseNANDWrite := true;

  RAWValue :=
    InterpretingSMARTValueList.GetRAWByID(IDOfHostWrite);

  result.InValue.ValueInMiB := Floor(RAWValue * LBAtoMiB);
end;

function TSandiskUSBNSTSupport.GetSMARTInterpreted(
  SMARTValueList: TSMARTValueList): TSMARTInterpreted;
const
  IDOfEraseError = 172;
  IDOfReplacedSector = 5;
  IDofUsedHour = 9;
  ReplacedSectorThreshold = 50;
  EraseErrorThreshold = 10;
begin
  InterpretingSMARTValueList := SMARTValueList;
  result.TotalWrite := GetTotalWrite;

  result.UsedHour :=
    InterpretingSMARTValueList.GetRAWByID(IDOfUsedHour);
  result.ReadEraseError.TrueReadErrorFalseEraseError := false;
  result.ReadEraseError.Value :=
    InterpretingSMARTValueList.GetRAWByID(IDOfEraseError);
  result.SMARTAlert.ReadEraseError :=
    result.ReadEraseError.Value >= EraseErrorThreshold;

  result.ReplacedSectors :=
    InterpretingSMARTValueList.GetRAWByID(IDOfReplacedSector);
  result.SMARTAlert.ReplacedSector :=
    result.ReplacedSectors >= ReplacedSectorThreshold;
end;

end.
