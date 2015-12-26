unit uPhisonNSTSupport;

interface

uses
  SysUtils, 
  uNSTSupport, Device.SMART.List;

type
  TPhisonNSTSupport = class sealed(TNSTSupport)
  private
    InterpretingSMARTValueList: TSMARTValueList;
    function GetSemiSupport: TSupportStatus;
    function GetTotalWrite: TTotalWrite;
    function IsHasSATASSDString: Boolean;
    function IsHasSAFM0String: Boolean;
    function IsProductOfPhison: Boolean;

  public
    function GetSupportStatus: TSupportStatus; override;
    function GetSMARTInterpreted(SMARTValueList: TSMARTValueList):
      TSMARTInterpreted; override;
  end;

implementation

{ TPhisonNSTSupport }

function TPhisonNSTSupport.IsHasSATASSDString: Boolean;
begin
  result :=
    Pos('SATA SSD', UpperCase(Model)) = 1;
end;

function TPhisonNSTSupport.IsHasSAFM0String: Boolean;
begin
  result :=
    Pos('SAFM0', UpperCase(Firmware)) = 1;
end;

function TPhisonNSTSupport.IsProductOfPhison: Boolean;
begin
  result :=
    IsHasSATASSDString and IsHasSAFM0String;
end;

function TPhisonNSTSupport.GetSemiSupport: TSupportStatus;
begin
  result.Supported := true;
  result.FirmwareUpdate := false;
  result.TotalWriteType := TTotalWriteType.WriteSupportedAsValue;
end;

function TPhisonNSTSupport.GetSupportStatus: TSupportStatus;
begin
  if IsProductOfPhison then
    result := GetSemiSupport;
end;

function TPhisonNSTSupport.GetTotalWrite: TTotalWrite;
const
  GiBtoMiB = 1024;
  IDOfHostWrite = 241;
var
  RAWValue: UInt64;
begin
  result.InValue.TrueHostWriteFalseNANDWrite := true;

  RAWValue :=
    InterpretingSMARTValueList.GetRAWByID(IDOfHostWrite);

  result.InValue.ValueInMiB := RAWValue * GiBtoMiB;
end;

function TPhisonNSTSupport.GetSMARTInterpreted(
  SMARTValueList: TSMARTValueList): TSMARTInterpreted;
const
  IDOfReadError = 1;   
  IDOfReplacedSector = 170;
  IDofUsedHour = 9;
  ReplacedSectorThreshold = 50;
  ReadErrorThreshold = 10;
  HalfOfSMARTBits = 16;
begin
  InterpretingSMARTValueList := SMARTValueList;
  result.TotalWrite := GetTotalWrite;

  result.UsedHour :=
    InterpretingSMARTValueList.GetRAWByID(IDOfUsedHour);
    
  result.ReadEraseError.TrueReadErrorFalseEraseError := true;
  result.ReadEraseError.Value :=
    InterpretingSMARTValueList.GetRAWByID(IDOfReadError);
  result.SMARTAlert.ReadEraseError :=
    result.ReadEraseError.Value >= ReadErrorThreshold;

  result.ReplacedSectors :=
    InterpretingSMARTValueList.GetRAWByID(
      IDOfReplacedSector shr HalfOfSMARTBits);
  result.SMARTAlert.ReplacedSector :=
    result.ReplacedSectors >= ReplacedSectorThreshold;
end;

end.
