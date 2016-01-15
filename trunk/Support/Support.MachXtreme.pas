unit Support.MachXtreme;

interface

uses
  SysUtils, Math,
  Support, Device.SMART.List;

type
  TMachXtremeNSTSupport = class sealed(TNSTSupport)
  private
    InterpretingSMARTValueList: TSMARTValueList;
    function GetSemiSupport: TSupportStatus;
    function GetJetUltraTotalWrite: TTotalWrite;
    function IsHasMachXtremeString: Boolean;
    function IsJetUltra: Boolean;
    function IsMyles: Boolean;
    function IsProductOfMachXtreme: Boolean;
    function GetSemiSupportWithoutWriteSupport: TSupportStatus;

  public
    function GetSupportStatus: TSupportStatus; override;
    function GetSMARTInterpreted(SMARTValueList: TSMARTValueList):
      TSMARTInterpreted; override;
  end;

implementation

{ TMachXtremeNSTSupport }

function TMachXtremeNSTSupport.IsJetUltra: Boolean;
begin
  result :=
    Pos('JT', UpperCase(Model)) > 0;
end;

function TMachXtremeNSTSupport.IsMyles: Boolean;
begin
  result :=
    Pos('MMY', UpperCase(Model)) > 0;
end;

function TMachXtremeNSTSupport.IsHasMachXtremeString: Boolean;
begin
  result :=
    Pos('MXSSD', UpperCase(Model)) > 0;
end;

function TMachXtremeNSTSupport.IsProductOfMachXtreme: Boolean;
begin
  result :=
    IsHasMachXtremeString and
    (IsMyles or IsJetUltra);
end;

function TMachXtremeNSTSupport.GetSemiSupportWithoutWriteSupport:
  TSupportStatus;
begin
  result.Supported := true;
  result.FirmwareUpdate := false;
  result.TotalWriteType := TTotalWriteType.WriteNotSupported;
end;

function TMachXtremeNSTSupport.GetSemiSupport: TSupportStatus;
begin
  result.Supported := true;
  result.FirmwareUpdate := false;
  result.TotalWriteType := TTotalWriteType.WriteSupportedAsValue;
end;

function TMachXtremeNSTSupport.GetSupportStatus: TSupportStatus;
begin
  result.Supported := false;
  if IsProductOfMachXtreme then
    if IsMyles then
      result := GetSemiSupportWithoutWriteSupport
    else
      result := GetSemiSupport;
end;

function TMachXtremeNSTSupport.GetJetUltraTotalWrite: TTotalWrite;
const
  JetUltraUnitToMiB: Double = 32;
  IDOfHostWrite = 241;
var
  RAWValue: UInt64;
begin
  result.InValue.TrueHostWriteFalseNANDWrite := true;

  RAWValue :=
    InterpretingSMARTValueList.GetRAWByID(IDOfHostWrite);

  result.InValue.ValueInMiB := Floor(RAWValue * JetUltraUnitToMiB);
end;

function TMachXtremeNSTSupport.GetSMARTInterpreted(
  SMARTValueList: TSMARTValueList): TSMARTInterpreted;
const
  IDOfEraseError = 182;   
  IDOfMylesReadError = 1;
  IDOfReplacedSector = 5;
  IDofUsedHour = 9;
  ReplacedSectorThreshold = 50;
  EraseErrorThreshold = 10;
begin
  InterpretingSMARTValueList := SMARTValueList;
  if IsJetUltra then
    result.TotalWrite := GetJetUltraTotalWrite;

  result.UsedHour :=
    InterpretingSMARTValueList.GetRAWByID(IDOfUsedHour);
  result.ReadEraseError.TrueReadErrorFalseEraseError := IsMyles;
  
  if IsMyles then
    result.ReadEraseError.Value :=
      InterpretingSMARTValueList.GetRAWByID(IDOfMylesReadError)
  else
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
