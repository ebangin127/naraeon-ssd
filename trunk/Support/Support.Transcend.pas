unit Support.Transcend;

interface

uses
  SysUtils,
  Support, Device.SMART.List;

type
  TTranscendNSTSupport = class sealed(TNSTSupport)
  private
    InterpretingSMARTValueList: TSMARTValueList;
    function GetSemiSupport: TSupportStatus;
    function GetTotalWrite: TTotalWrite;
    function IsProductOfTranscend: Boolean;
    function Is220S: Boolean;
  public
    function GetSupportStatus: TSupportStatus; override;
    function GetSMARTInterpreted(SMARTValueList: TSMARTValueList):
      TSMARTInterpreted; override;
  end;

implementation

{ TSandiskNSTSupport }

function TTranscendNSTSupport.Is220S: Boolean;
begin
  result := (Pos('TS', Model) > 0) and (Pos('GSSD220S', Model) > 0);
end;

function TTranscendNSTSupport.IsProductOfTranscend: Boolean;
begin
  result := Is220S;
end;

function TTranscendNSTSupport.GetSemiSupport: TSupportStatus;
begin
  result.Supported := true;
  result.FirmwareUpdate := false;
  result.TotalWriteType := TTotalWriteType.WriteSupportedAsValue;
end;

function TTranscendNSTSupport.GetSupportStatus: TSupportStatus;
begin
  result.Supported := false;
  if IsProductOfTranscend then
    result := GetSemiSupport;
end;

function TTranscendNSTSupport.GetTotalWrite: TTotalWrite;
const
  TranscendUnit = 32;
  IDOfHostWrite = $F1;
var
  RAWValue: UInt64;
begin
  result.InValue.TrueHostWriteFalseNANDWrite := true;

  RAWValue :=
    InterpretingSMARTValueList.GetRAWByID(IDOfHostWrite);

  result.InValue.ValueInMiB := RAWValue * TranscendUnit;
end;

function TTranscendNSTSupport.GetSMARTInterpreted(
  SMARTValueList: TSMARTValueList): TSMARTInterpreted;
const
  IDOfReadError = 1;
  IDOfReplacedSector = 5;
  ReplacedSectorThreshold = 50;
  EraseErrorThreshold = 10;
begin
  InterpretingSMARTValueList := SMARTValueList;
  result.TotalWrite := GetTotalWrite;

  result.UsedHour := 0;
  result.ReadEraseError.TrueReadErrorFalseEraseError := true;
  result.ReadEraseError.Value :=
    InterpretingSMARTValueList.GetRAWByID(IDOfReadError);
  result.SMARTAlert.ReadEraseError :=
    result.ReadEraseError.Value >= EraseErrorThreshold;

  result.ReplacedSectors :=
    InterpretingSMARTValueList.GetRAWByID(IDOfReplacedSector);
  result.SMARTAlert.ReplacedSector :=
    result.ReplacedSectors >= ReplacedSectorThreshold;
end;

end.
