unit Support.Samsung;

interface

uses
  SysUtils, Math,
  Support, Device.SMART.List;

type
  TSamsungNSTSupport = class sealed(TNSTSupport)
  private
    InterpretingSMARTValueList: TSMARTValueList;
    function GetSemiSupport: TSupportStatus;
    function GetTotalWrite: TTotalWrite;
    function IsProductOfSamsung: Boolean;
    function IsSamsung470: Boolean;
    function IsSamsungOtherSSD: Boolean;
    function IsSamsungSATA: Boolean;
  public
    function GetSupportStatus: TSupportStatus; override;
    function GetSMARTInterpreted(SMARTValueList: TSMARTValueList):
      TSMARTInterpreted; override;
  end;

implementation

{ TSamsungNSTSupport }

function TSamsungNSTSupport.IsSamsungOtherSSD: Boolean;
begin
  result :=
    Pos('SSD', UpperCase(Model)) > 0;
end;

function TSamsungNSTSupport.IsSamsung470: Boolean;
begin
  result :=
    Pos('470', UpperCase(Model)) > 0;
end;

function TSamsungNSTSupport.IsSamsungSATA: Boolean;
begin
  result :=
    Pos('BX', UpperCase(Firmware)) < 5;
end;

function TSamsungNSTSupport.IsProductOfSamsung: Boolean;
begin
  result :=
    (Pos('SAMSUNG', UpperCase(Model)) > 0) and
    (IsSamsungOtherSSD or IsSamsung470) and
    IsSamsungSATA;
end;

function TSamsungNSTSupport.GetSemiSupport: TSupportStatus;
begin
  result.Supported := true;
  result.FirmwareUpdate := false;
  result.TotalWriteType := TTotalWriteType.WriteSupportedAsValue;
end;

function TSamsungNSTSupport.GetSupportStatus: TSupportStatus;
begin
  result.Supported := false;
  if IsProductOfSamsung then
    result := GetSemiSupport;
end;

function TSamsungNSTSupport.GetTotalWrite: TTotalWrite;
const
  LBAtoMiB: Double = 1/2 * 1/1024;
  IDOfHostWrite = 241;
var
  RAWValue: UInt64;
begin
  result.InValue.TrueHostWriteFalseNANDWrite := true;

  RAWValue :=
    InterpretingSMARTValueList.GetRAWByID(IDOfHostWrite);

  result.InValue.ValueInMiB := Floor(RAWValue * LBAtoMiB);
end;

function TSamsungNSTSupport.GetSMARTInterpreted(
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
