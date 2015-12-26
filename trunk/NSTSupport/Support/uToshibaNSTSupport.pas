unit uToshibaNSTSupport;

interface

uses
  SysUtils,
  uNSTSupport, Device.SMART.List;

type
  TToshibaNSTSupport = class sealed(TNSTSupport)
  private
    InterpretingSMARTValueList: TSMARTValueList;
    function GetEraseError: TReadEraseError;
    function GetFullSupport: TSupportStatus;
    function GetTotalWrite: TTotalWrite;
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
  result := Pos('TOSHIBA', Model) > 0;
end;

function TToshibaNSTSupport.IsTHNSNF: Boolean;
begin
  result := Pos('THNSNF', Model) > 0;
end;

function TToshibaNSTSupport.IsTHNSNH: Boolean;
begin
  result := Pos('THNSNH', Model) > 0;
end;

function TToshibaNSTSupport.IsTHNSNJ: Boolean;
begin
  result := Pos('THNSNJ', Model) > 0;
end;

function TToshibaNSTSupport.IsProductOfToshiba: Boolean;
begin
  result := IsModelHasToshibaString and
    (IsTHNSNF or IsTHNSNH or IsTHNSNJ);
end;

function TToshibaNSTSupport.GetFullSupport: TSupportStatus;
begin
  result.Supported := true;
  result.FirmwareUpdate := true;
  
  result.TotalWriteType := TTotalWriteType.WriteNotSupported;
end;

function TToshibaNSTSupport.GetSupportStatus: TSupportStatus;
begin
  result.Supported := false;
  if IsProductOfToshiba then
    result := GetFullSupport;
end;

function TToshibaNSTSupport.GetTotalWrite: TTotalWrite;
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

function TToshibaNSTSupport.GetEraseError: TReadEraseError;
const
  IDOfEraseErrorTHNSNS = 172;
  IDOfEraseErrorElse = 182;
begin
  result.TrueReadErrorFalseEraseError := false;
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
  result.TotalWrite := GetTotalWrite;

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
