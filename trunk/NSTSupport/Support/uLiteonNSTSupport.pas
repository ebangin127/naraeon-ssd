unit uLiteonNSTSupport;

interface

uses
  SysUtils,
  uNSTSupport, uSMARTValueList;

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
  result := (Pos('LITEONIT', Model) > 0);
end;

function TLiteonNSTSupport.IsS100: Boolean;
begin
  result := Pos('S100', Model) > 0;
end;

function TLiteonNSTSupport.IsS200: Boolean;
begin
  result := Pos('S200', Model) > 0;
end;

function TLiteonNSTSupport.IsE200: Boolean;
begin
  result := Pos('E200', Model) > 0;
end;

function TLiteonNSTSupport.IsS100WithNewUnit: Boolean;
begin
  result := IsS100 and (Pos('85', Firmware) > 0);
end;

function TLiteonNSTSupport.IsProductOfLiteON: Boolean;
begin
  result := IsModelHasLiteonString and (IsS100 or IsS200 or IsE200);
end;

function TLiteonNSTSupport.GetFullSupport: TSupportStatus;
begin
  result.Supported := true;
  result.FirmwareUpdate := true;
  result.TotalWriteType := TTotalWriteType.WriteSupportedAsValue;
end;

function TLiteonNSTSupport.GetSupportStatus: TSupportStatus;
begin
  result.Supported := false;
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
  result.TrueHostWriteFalseNANDWrite := false;

  RAWValue :=
    InterpretingSMARTValueList.IndexByID(IDOfLiteONNANDWrite);

  if IsS100WithNewUnit then
    result.ValueInMiB := RAWValue * NewLiteONUnit
  else
    result.ValueInMiB := RAWValue * OldLiteONUnit;
end;

function TLiteonNSTSupport.GetSMARTInterpreted(
  SMARTValueList: TSMARTValueList): TSMARTInterpreted;
const
  IDOfEraseError = 182;
  IDOfReplacedSector = 5;
  IDOfUsedHour = 1;
  ReplacedSectorThreshold = 25;
  EraseErrorThreshold = 10;
begin
  InterpretingSMARTValueList := SMARTValueList;
  result.TotalWrite := GetTotalWrite;

  result.UsedHour := 
    InterpretingSMARTValueList.IndexByID(IDOfUsedHour);
  result.EraseError :=
    InterpretingSMARTValueList.IndexByID(IDOfEraseError);
  result.SMARTAlert.EraseError :=
    result.EraseError >= EraseErrorThreshold;

  result.ReplacedSectors :=
    InterpretingSMARTValueList.IndexByID(IDOfReplacedSector);
  result.SMARTAlert.ReplacedSector :=
    result.ReplacedSectors >= ReplacedSectorThreshold;
end;

end.

