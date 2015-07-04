unit uADATANSTSupport;

interface

uses
  SysUtils, Math,
  uNSTSupport, uSMARTValueList;

type
  TADATANSTSupport = class sealed(TNSTSupport)
  private
    InterpretingSMARTValueList: TSMARTValueList;
    function GetFullSupport: TSupportStatus;
    function GetTotalWrite: TTotalWrite;
    function IsModelHasADATAString: Boolean;
    function IsSP920: Boolean;
    function IsProductOfADATA: Boolean;

  public
    function GetSupportStatus: TSupportStatus; override;
    function GetSMARTInterpreted(SMARTValueList: TSMARTValueList):
      TSMARTInterpreted; override;
  end;

implementation

{ TADATANSTSupport }

function TADATANSTSupport.IsModelHasADATAString: Boolean;
begin
  result := Pos('ADATA ', Model) = 1;
end;

function TADATANSTSupport.IsSP920: Boolean;
begin
  result := Pos('SP920', Model) > 0;
end;

function TADATANSTSupport.IsProductOfADATA: Boolean;
begin
  result := IsModelHasADATAString and IsSP920;
end;

function TADATANSTSupport.GetFullSupport: TSupportStatus;
begin
  result.Supported := true;
  result.FirmwareUpdate := true;
  result.TotalWriteType := TTotalWriteType.WriteSupportedAsValue;
end;

function TADATANSTSupport.GetSupportStatus: TSupportStatus;
begin
  result.Supported := false;
  if IsProductOfADATA then
    result := GetFullSupport;
end;

function TADATANSTSupport.GetTotalWrite: TTotalWrite;
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

function TADATANSTSupport.GetSMARTInterpreted(
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
