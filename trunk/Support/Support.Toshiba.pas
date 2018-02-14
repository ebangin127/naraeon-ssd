unit Support.Toshiba;

interface

uses
  SysUtils,
  Support, Device.SMART.List;

type
  TToshibaNSTSupport = class sealed(TNSTSupport)
  private
    InterpretingSMARTValueList: TSMARTValueList;
    function GetEraseError: TReadEraseError;
    function GetFullSupport: TSupportStatus;
    function IsModelHasToshibaString: Boolean;
    function IsProductOfToshiba: Boolean;
    function IsTHNSNF: Boolean;
    function IsTHNSNH: Boolean;
    function IsTHNSNJ: Boolean;
  public
    function GetSupportStatus: TSupportStatus; override;
    function GetSMARTInterpreted(SMARTValueList: TSMARTValueList):
      TSMARTInterpreted; override;
  end;

implementation

{ TToshibaNSTSupport }

function TToshibaNSTSupport.IsModelHasToshibaString: Boolean;
begin
  result := Pos('TOSHIBA', Identify.Model) > 0;
end;

function TToshibaNSTSupport.IsTHNSNF: Boolean;
begin
  result := Pos('THNSNF', Identify.Model) > 0;
end;

function TToshibaNSTSupport.IsTHNSNH: Boolean;
begin
  result := Pos('THNSNH', Identify.Model) > 0;
end;

function TToshibaNSTSupport.IsTHNSNJ: Boolean;
begin
  result := Pos('THNSNJ', Identify.Model) > 0;
end;

function TToshibaNSTSupport.IsProductOfToshiba: Boolean;
begin
  result := IsModelHasToshibaString and
    (IsTHNSNF or IsTHNSNH or IsTHNSNJ);
end;

function TToshibaNSTSupport.GetFullSupport: TSupportStatus;
begin
  result.Supported := Supported;
  result.FirmwareUpdate := true;
  result.TotalWriteType := TTotalWriteType.WriteNotSupported;
end;

function TToshibaNSTSupport.GetSupportStatus: TSupportStatus;
begin
  result.Supported := NotSupported;
  if IsProductOfToshiba then
    result := GetFullSupport;
end;

function TToshibaNSTSupport.GetEraseError: TReadEraseError;
const
  IDOfEraseErrorElse = 1;
begin
  result.TrueReadErrorFalseEraseError := true;
  result.Value :=
    InterpretingSMARTValueList.GetRAWByID(IDOfEraseErrorElse);
end;

function TToshibaNSTSupport.GetSMARTInterpreted(
  SMARTValueList: TSMARTValueList): TSMARTInterpreted;
const
  IDOfReplacedSector = 5;
  IDofUsedHour = 9;
  ReplacedSectorThreshold = 50;
  EraseErrorThreshold = 10;
begin
  InterpretingSMARTValueList := SMARTValueList;
  result.TotalWrite.InValue.ValueInMiB := 0;
  result.UsedHour := 
    InterpretingSMARTValueList.GetRAWByID(IDOfUsedHour);
  result.ReadEraseError := GetEraseError;
  result.SMARTAlert.ReadEraseError :=
    result.ReadEraseError.Value >= EraseErrorThreshold;
  result.ReplacedSectors :=
    InterpretingSMARTValueList.GetRAWByID(IDOfReplacedSector);
  result.SMARTAlert.ReplacedSector :=
    result.ReplacedSectors >= ReplacedSectorThreshold;
end;

end.
