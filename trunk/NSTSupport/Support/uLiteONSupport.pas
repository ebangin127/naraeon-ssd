unit uLiteONSupport;

interface

uses
  SysUtils,
  uNSTSupport, uSMARTValueList;

type
  TLiteONSupport = class abstract(TNSTSupport)
  private
    InterpretingSMARTValueList: TSMARTValueList;
    function IsProductOfLiteON: Boolean;
    function GetFullSupport: TSupportStatus;
    function GetTotalWrite: TTotalWrite;
    function IsE200: Boolean;
    function IsModelHasLiteONString: Boolean;
    function IsS100: Boolean;
    function IsS200: Boolean;
    function IsS100WithNewUnit: Boolean;
  public
    function GetSupportStatus: TSupportStatus; override;
    function GetSMARTInterpreted(SMARTValueList: TSMARTValueList):
      TSMARTInterpreted; override;
  end;

implementation

{ TLiteONSupport }

function TLiteONSupport.IsModelHasLiteONString: Boolean;
begin
  result := (Pos('LITEONIT', Model) > 0);
end;

function TLiteONSupport.IsS100: Boolean;
begin
  result := Pos('S100', Model) > 0;
end;

function TLiteONSupport.IsS200: Boolean;
begin
  result := Pos('S200', Model) > 0;
end;

function TLiteONSupport.IsE200: Boolean;
begin
  result := Pos('E200', Model) > 0;
end;

function TLiteONSupport.IsS100WithNewUnit: Boolean;
begin
  result := IsS100 and (Pos('85', Firmware) > 0);
end;

function TLiteONSupport.IsProductOfLiteON: Boolean;
begin
  result := IsModelHasLiteONString and (IsS100 or IsS200 or IsE200);
end;

function TLiteONSupport.GetFullSupport: TSupportStatus;
begin
  result.Supported := true;
  result.FirmwareUpdate := true;
  result.TotalWriteType := TTotalWriteType.WriteSupportedAsValue;
end;

function TLiteONSupport.GetSupportStatus: TSupportStatus;
begin
  result.Supported := false;
  if IsProductOfLiteON then
    result := GetFullSupport;
end;

function TLiteONSupport.GetTotalWrite: TTotalWrite;
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

function TLiteONSupport.GetSMARTInterpreted(
  SMARTValueList: TSMARTValueList): TSMARTInterpreted;
const
  IDOfEraseError = 182;
  IDOfReplacedSector = 5;
  ReplacedSectorThreshold = 25;
  EraseErrorThreshold = 10;
begin
  InterpretingSMARTValueList := SMARTValueList;
  result.TotalWrite := GetTotalWrite;

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

