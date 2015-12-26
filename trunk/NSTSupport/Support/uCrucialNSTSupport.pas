unit uCrucialNSTSupport;

interface

uses
  SysUtils, Math,
  uNSTSupport, Device.SMART.List;

type
  TCrucialNSTSupport = class sealed(TNSTSupport)
  private
    InterpretingSMARTValueList: TSMARTValueList;
    function GetFullSupport: TSupportStatus;
    function GetTotalWrite: TTotalWrite;
    function IsM500: Boolean;
    function IsM550: Boolean;
    function IsModelHasCrucialString: Boolean;
    function IsMX100: Boolean;
    function IsMX200: Boolean;
    function IsProductOfCrucial: Boolean;

  public
    function GetSupportStatus: TSupportStatus; override;
    function GetSMARTInterpreted(SMARTValueList: TSMARTValueList):
      TSMARTInterpreted; override;
  end;

implementation

{ TCrucialNSTSupport }

function TCrucialNSTSupport.IsModelHasCrucialString: Boolean;
begin
  result := Pos('CRUCIAL', Model) > 0;
end;

function TCrucialNSTSupport.IsMX100: Boolean;
begin
  result := Pos('MX100', Model) > 0;
end;

function TCrucialNSTSupport.IsMX200: Boolean;
begin
  result := Pos('MX200', Model) > 0;
end;

function TCrucialNSTSupport.IsM550: Boolean;
begin
  result := Pos('M550', Model) > 0;
end;

function TCrucialNSTSupport.IsM500: Boolean;
begin
  result := Pos('M500', Model) > 0;
end;

function TCrucialNSTSupport.IsProductOfCrucial: Boolean;
begin
  result := IsModelHasCrucialString and 
    (IsMX100 or IsMX200 or
     IsM500 or IsM550);
end;

function TCrucialNSTSupport.GetFullSupport: TSupportStatus;
begin
  result.Supported := true;
  result.FirmwareUpdate := true;
  result.TotalWriteType := TTotalWriteType.WriteSupportedAsValue;
end;

function TCrucialNSTSupport.GetSupportStatus: TSupportStatus;
begin
  result.Supported := false;
  if IsProductOfCrucial then
    result := GetFullSupport;
end;

function TCrucialNSTSupport.GetTotalWrite: TTotalWrite;
const
  LBAtoMiB: Double = 1/2 * 1/1024;
  IDOfHostWrite = 246;
var
  RAWValue: UInt64;
begin
  result.InValue.TrueHostWriteFalseNANDWrite := true;

  RAWValue :=
    InterpretingSMARTValueList.GetRAWByID(IDOfHostWrite);

  result.InValue.ValueInMiB := Floor(RAWValue * LBAtoMiB);
end;

function TCrucialNSTSupport.GetSMARTInterpreted(
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
