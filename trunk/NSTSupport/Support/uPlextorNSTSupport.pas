unit uPlextorNSTSupport;

interface

uses
  SysUtils,
  uNSTSupport, uSMARTValueList;

type
  TPlextorNSTSupport = class sealed(TNSTSupport)
  private
    InterpretingSMARTValueList: TSMARTValueList;
    function IsNinja: Boolean;
    function IsM3Series: Boolean;
    function IsM5Series: Boolean;
    function IsProductOfPlextor: Boolean;
    function GetFullSupport: TSupportStatus;
    function GetTotalWrite: TTotalWrite;
    function IsM3SeriesWithOldUnit: Boolean;
    function IsM3PWithOldUnit: Boolean;
    function IsM3WithOldUnit: Boolean;
    function IsM3AndNotM3P: Boolean;
    function IsM3128WithOldUnit: Boolean;
    function IsM3256WithOldUnit: Boolean;
    function IsM3512WithOldUnit: Boolean;
    function IsM364WithOldUnit: Boolean;

  public
    function GetSupportStatus: TSupportStatus; override;
    function GetSMARTInterpreted(SMARTValueList: TSMARTValueList):
      TSMARTInterpreted; override;
  end;

implementation

{ TPlextorNSTSupport }

function TPlextorNSTSupport.IsM5Series: Boolean;
begin
  result := (Pos('PLEXTOR', Model) > 0) and (Pos('M5', Model) > 0);
end;

function TPlextorNSTSupport.IsM3Series: Boolean;
begin
  result := (Pos('PLEXTOR', Model) > 0) and (Pos('M3', Model) > 0);
end;

function TPlextorNSTSupport.IsM3AndNotM3P: Boolean;
begin
  result := (Copy(Model, Length(Model) - 2, 2) = 'M3');
end;

function TPlextorNSTSupport.IsM364WithOldUnit: Boolean;
const
  OldUnit = 1.06;
begin
  result :=
    (Pos('64', Model) > 0) and (StrToFloat(Firmware) < OldUnit);
end;
function TPlextorNSTSupport.IsM3128WithOldUnit: Boolean;
const
  OldUnit = 1.07;
begin
  result :=
    (Pos('128', Model) > 0) and (StrToFloat(Firmware) < OldUnit);
end;
function TPlextorNSTSupport.IsM3256WithOldUnit: Boolean;
const
  OldUnit = 1.07;
begin
  result :=
    (Pos('256', Model) > 0) and (StrToFloat(Firmware) < OldUnit);
end;

function TPlextorNSTSupport.IsM3512WithOldUnit: Boolean;
const
  OldUnit = 1.06;
begin
  result :=
    (Pos('512', Model) > 0) and (StrToFloat(Firmware) < OldUnit);
end;

function TPlextorNSTSupport.IsM3WithOldUnit: Boolean;
begin
  result :=
    IsM3AndNotM3P and
    (IsM364WithOldUnit  or IsM3128WithOldUnit or
     IsM3256WithOldUnit or IsM3512WithOldUnit or
     IsM3512WithOldUnit);
end;

function TPlextorNSTSupport.IsM3PWithOldUnit: Boolean;
const
  OldUnit = 1.06;
begin
  result :=
    (Pos('M3P', Model) > 0) and (StrToFloat(Firmware) < OldUnit);
end;

function TPlextorNSTSupport.IsM3SeriesWithOldUnit: Boolean;
begin
  result := IsM3Series and
    (IsM3PWithOldUnit or IsM3WithOldUnit);
end;

function TPlextorNSTSupport.IsNinja: Boolean;
begin
  result := Pos('NINJA', Model) > 0;
end;

function TPlextorNSTSupport.IsProductOfPlextor: Boolean;
begin
  result := IsNinja or IsM3Series or IsM5Series;
end;

function TPlextorNSTSupport.GetFullSupport: TSupportStatus;
begin
  result.Supported := true;
  result.FirmwareUpdate := true;
  result.TotalWriteType := TTotalWriteType.WriteSupportedAsValue;
end;

function TPlextorNSTSupport.GetSupportStatus: TSupportStatus;
begin
  result.Supported := false;
  if IsProductOfPlextor then
    result := GetFullSupport;
end;

function TPlextorNSTSupport.GetTotalWrite: TTotalWrite;
const
  OldPlextorUnit = 64;
  NewPlextorUnit = 128;
  IDOfPlextorNANDWrite = 177;
var
  RAWValue: UInt64;
begin
  result.InValue.TrueHostWriteFalseNANDWrite := false;

  RAWValue :=
    InterpretingSMARTValueList.GetRAWByID(IDOfPlextorNANDWrite);

  if IsM3SeriesWithOldUnit then
    result.InValue.ValueInMiB := RAWValue * OldPlextorUnit
  else
    result.InValue.ValueInMiB := RAWValue * NewPlextorUnit;
end;

function TPlextorNSTSupport.GetSMARTInterpreted(
  SMARTValueList: TSMARTValueList): TSMARTInterpreted;
const
  IDOfEraseError = 182;
  IDOfReplacedSector = 5;
  IDofUsedHour = 9;
  ReplacedSectorThreshold = 25;
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
