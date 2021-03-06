unit Support.Liteon;

interface

uses
  SysUtils,
  Support, Device.SMART.List;

type
  TLiteonNSTSupport = class sealed(TNSTSupport)
  private
    InterpretingSMARTValueList: TSMARTValueList;
    function IsProductOfLiteON: Boolean;
    function GetFullSupport: TSupportStatus;
    function GetTotalWrite: TTotalWrite;
    function IsE200: Boolean;
    function IsModelHasLiteonString: Boolean;
    function IsS100: Boolean;
    function IsS200: Boolean;
    function IsS100WithNewUnit: Boolean;
  public
    function GetSupportStatus: TSupportStatus; override;
    function GetSMARTInterpreted(SMARTValueList: TSMARTValueList):
      TSMARTInterpreted; override;
  end;

implementation

{ TLiteonNSTSupport }

function TLiteonNSTSupport.IsModelHasLiteonString: Boolean;
begin
  result := (Pos('LITEONIT', Identify.Model) > 0);
end;

function TLiteonNSTSupport.IsS100: Boolean;
begin
  result := Pos('S100', Identify.Model) > 0;
end;

function TLiteonNSTSupport.IsS200: Boolean;
begin
  result := Pos('M3S', Identify.Model) > 0;
end;

function TLiteonNSTSupport.IsE200: Boolean;
begin
  result := Pos('E200', Identify.Model) > 0;
end;

function TLiteonNSTSupport.IsS100WithNewUnit: Boolean;
begin
  result := IsS100 and (Pos('85', Identify.Firmware) > 0);
end;

function TLiteonNSTSupport.IsProductOfLiteON: Boolean;
begin
  result := IsModelHasLiteonString and (IsS100 or IsS200 or IsE200);
end;

function TLiteonNSTSupport.GetFullSupport: TSupportStatus;
begin
  result.Supported := Supported;
  result.FirmwareUpdate := true;
  result.TotalWriteType := TTotalWriteType.WriteSupportedAsValue;
end;

function TLiteonNSTSupport.GetSupportStatus: TSupportStatus;
begin
  result.Supported := NotSupported;
  if IsProductOfLiteON then
    result := GetFullSupport;
end;

function TLiteonNSTSupport.GetTotalWrite: TTotalWrite;
const
  OldLiteONUnit = 64;
  NewLiteONUnit = 128;
  IDOfLiteONNANDWrite = 177;
var
  RAWValue: UInt64;
begin
  result.InValue.TrueHostWriteFalseNANDWrite := false;

  RAWValue :=
    InterpretingSMARTValueList.GetRAWByID(IDOfLiteONNANDWrite);

  if IsS100WithNewUnit then
    result.InValue.ValueInMiB := RAWValue * NewLiteONUnit
  else
    result.InValue.ValueInMiB := RAWValue * OldLiteONUnit;
end;

function TLiteonNSTSupport.GetSMARTInterpreted(
  SMARTValueList: TSMARTValueList): TSMARTInterpreted;
const
  IDOfEraseError = 182;
  IDOfReplacedSector = 5;
  IDofUsedHour = 9;
  ReplacedSectorThreshold = 25;
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

