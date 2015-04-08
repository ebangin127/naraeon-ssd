unit uCrucialNSTSupport;

interface

uses
  SysUtils, Math,
  uNSTSupport, uSMARTValueList;

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
  result := IsModelHasCrucialString and (IsMX100 or IsM550 or IsM500);
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
  LBAtoKiB = 1/2;
  KiBToMiB = 1/1024;
  LBAtoMiB = LBAtoKiB * KiBToMiB;
  IDOfHostWrite = 246;
var
  RAWValue: UInt64;
begin
  result.TrueHostWriteFalseNANDWrite := true;

  RAWValue :=
    InterpretingSMARTValueList.IndexByID(IDOfHostWrite);

  result.ValueInMiB := Floor(RAWValue * LBAtoMiB);
end;

function TCrucialNSTSupport.GetSMARTInterpreted(
  SMARTValueList: TSMARTValueList): TSMARTInterpreted;
const
  IDOfEraseError = 172;
  IDOfReplacedSector = 5;
  IDofUsedHour = 1;
  ReplacedSectorThreshold = 50;
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
