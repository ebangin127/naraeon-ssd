unit Support.NVMe.Intel;

interface

uses
  SysUtils, Math,
  Support, Device.SMART.List;

type
  TIntelNVMeSupport = class sealed(TNSTSupport)
  private
    InterpretingSMARTValueList: TSMARTValueList;
    function GetSemiSupport: TSupportStatus;
    function GetTotalWrite: TTotalWrite;
    function IsProductOfIntel: Boolean;
  public
    function GetSupportStatus: TSupportStatus; override;
    function GetSMARTInterpreted(SMARTValueList: TSMARTValueList):
      TSMARTInterpreted; override;
  end;

implementation

{ TIntelNVMeSupport }

function TIntelNVMeSupport.IsProductOfIntel: Boolean;
begin
  result :=
    (Pos('INTEL SSDPE', UpperCase(Model)) = 1);
end;

function TIntelNVMeSupport.GetSemiSupport: TSupportStatus;
begin
  result.Supported := true;
  result.FirmwareUpdate := false;
  result.TotalWriteType := TTotalWriteType.WriteSupportedAsValue;
end;

function TIntelNVMeSupport.GetSupportStatus: TSupportStatus;
begin
  result.Supported := false;
  if IsProductOfIntel then
    result := GetSemiSupport;
end;

function TIntelNVMeSupport.GetTotalWrite: TTotalWrite;
  function LBAToMB(const SizeInLBA: Int64): UInt64;
  begin
    result := SizeInLBA shr 1;
  end;
const
  IDOfHostWrite = 5;
var
  RAWValue: UInt64;
begin
  result.InValue.TrueHostWriteFalseNANDWrite := true;
  RAWValue :=
    InterpretingSMARTValueList.GetRAWByID(IDOfHostWrite);
  result.InValue.ValueInMiB := LBAToMB(RAWValue);
end;

function TIntelNVMeSupport.GetSMARTInterpreted(
  SMARTValueList: TSMARTValueList): TSMARTInterpreted;
const
  IDOfEraseError = 13;
  IDOfReplacedSector = 6;
  IDOfUsedHour = 11;
  IDOfCriticalError = 1;
  ReplacedSectorThreshold = 50;
  EraseErrorThreshold = 10;
begin
  InterpretingSMARTValueList := SMARTValueList;
  result.TotalWrite := GetTotalWrite;
  result.UsedHour :=
    InterpretingSMARTValueList.GetRAWByID(IDOfUsedHour);
  result.ReadEraseError.TrueReadErrorFalseEraseError := true;
  result.ReadEraseError.Value :=
    InterpretingSMARTValueList.GetRAWByID(IDOfEraseError);
  result.SMARTAlert.ReadEraseError :=
    result.ReadEraseError.Value >= EraseErrorThreshold;
  result.ReplacedSectors := 0;
  result.SMARTAlert.ReplacedSector :=
    result.ReplacedSectors >= ReplacedSectorThreshold;
  result.SMARTAlert.CriticalError :=
    InterpretingSMARTValueList.GetRAWByID(IDOfCriticalError) > 0;
end;

end.
