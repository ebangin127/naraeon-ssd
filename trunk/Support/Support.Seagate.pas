unit Support.Seagate;

interface

uses
  SysUtils,
  Support, Device.SMART.List;

type
  TSeagateNSTSupport = class sealed(TNSTSupport)
  private
    InterpretingSMARTValueList: TSMARTValueList;
    function GetFullSupport: TSupportStatus;
    function GetTotalWrite: TTotalWrite;
    function IsProductOfSeagate: Boolean;
    function IsSeagate600: Boolean;

  public
    function GetSupportStatus: TSupportStatus; override;
    function GetSMARTInterpreted(SMARTValueList: TSMARTValueList):
      TSMARTInterpreted; override;
  end;

implementation

{ TSeagateNSTSupport }

function TSeagateNSTSupport.IsSeagate600: Boolean;
begin
  result := (Pos('ST', Identify.Model) > 0) and (Pos('HM000', Identify.Model) > 0);
end;

function TSeagateNSTSupport.IsProductOfSeagate: Boolean;
begin
  result := IsSeagate600;
end;

function TSeagateNSTSupport.GetFullSupport: TSupportStatus;
begin
  result.Supported := Supported;
  result.FirmwareUpdate := true;
  result.TotalWriteType := TTotalWriteType.WriteSupportedAsValue;
end;

function TSeagateNSTSupport.GetSupportStatus: TSupportStatus;
begin
  result.Supported := NotSupported;
  if IsProductOfSeagate then
    result := GetFullSupport;
end;

function TSeagateNSTSupport.GetTotalWrite: TTotalWrite;
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

function TSeagateNSTSupport.GetSMARTInterpreted(
  SMARTValueList: TSMARTValueList): TSMARTInterpreted;
const
  IDOfEraseError = 182;
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
