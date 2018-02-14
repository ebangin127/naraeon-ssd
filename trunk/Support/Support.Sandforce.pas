unit Support.Sandforce;

interface

uses
  SysUtils, Math,
  Support, Device.SMART.List;

type
  TSandforceNSTSupport = class abstract(TNSTSupport)
  private
    InterpretingSMARTValueList: TSMARTValueList;
    function GetTotalWrite: TTotalWrite;

  protected
    function GetSemiSupport: TSupportStatus;

  public
    function GetSMARTInterpreted(SMARTValueList: TSMARTValueList):
      TSMARTInterpreted; override;
  end;

implementation

{ TSandforceNSTSupport }

function TSandforceNSTSupport.GetSemiSupport: TSupportStatus;
begin
  result.Supported := Supported;
  result.FirmwareUpdate := false;
  result.TotalWriteType := TTotalWriteType.WriteSupportedAsValue;
end;

function TSandforceNSTSupport.GetTotalWrite: TTotalWrite;
const
  GiBtoMiB: Double = 1024;
  IDOfHostWrite = 241;
var
  RAWValue: UInt64;
begin
  result.InValue.TrueHostWriteFalseNANDWrite := true;

  RAWValue :=
    InterpretingSMARTValueList.GetRAWByID(IDOfHostWrite);

  result.InValue.ValueInMiB := Floor(RAWValue * GiBtoMiB);
end;

function TSandforceNSTSupport.GetSMARTInterpreted(
  SMARTValueList: TSMARTValueList): TSMARTInterpreted;
const
  IDOfEraseError = 172;
  IDOfReplacedSector = 5;
  IDOfUsedHour = 9;
  ReplacedSectorThreshold = 50;
  EraseErrorThreshold = 10;
begin
  InterpretingSMARTValueList := SMARTValueList;
  result.TotalWrite := GetTotalWrite;

  //Sandforce uses only 4 bytes for UsedHour RAW
  result.UsedHour :=
    InterpretingSMARTValueList.GetRAWByID(IDOfUsedHour) and $FFFFFFFF;
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
