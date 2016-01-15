unit Support.Sandisk;

interface

uses
  SysUtils,
  Support, Device.SMART.List;

type
  TSandiskNSTSupport = class sealed(TNSTSupport)
  private
    InterpretingSMARTValueList: TSMARTValueList;
    function GetFullSupport: TSupportStatus;

    function GetTotalWrite: TTotalWrite;
    function IsProductOfSandisk: Boolean;
    function IsX110: Boolean;

  public
    function GetSupportStatus: TSupportStatus; override;
    function GetSMARTInterpreted(SMARTValueList: TSMARTValueList):
      TSMARTInterpreted; override;
  end;

implementation

{ TSandiskNSTSupport }

function TSandiskNSTSupport.IsX110: Boolean;
begin
  result := (Pos('SANDISK', Model) > 0) and (Pos('SD6SB1', Model) > 0);
end;

function TSandiskNSTSupport.IsProductOfSandisk: Boolean;
begin
  result := IsX110;
end;

function TSandiskNSTSupport.GetFullSupport: TSupportStatus;
begin
  result.Supported := true;
  result.FirmwareUpdate := true;
  result.TotalWriteType := TTotalWriteType.WriteSupportedAsValue;
end;

function TSandiskNSTSupport.GetSupportStatus: TSupportStatus;
begin
  result.Supported := false;
  if IsProductOfSandisk then
    result := GetFullSupport;
end;

function TSandiskNSTSupport.GetTotalWrite: TTotalWrite;
const
  GiBToMiB = 1024;
  IDOfHostWrite = 241;
var
  RAWValue: UInt64;
begin
  result.InValue.TrueHostWriteFalseNANDWrite := true;

  RAWValue :=
    InterpretingSMARTValueList.GetRAWByID(IDOfHostWrite);

  result.InValue.ValueInMiB := RAWValue * GiBToMiB;
end;

function TSandiskNSTSupport.GetSMARTInterpreted(
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
