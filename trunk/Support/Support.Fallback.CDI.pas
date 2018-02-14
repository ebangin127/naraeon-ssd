unit Support.Fallback.CDI;

interface

uses
  SMARTSupport, SMARTSupport.Factory,
  SysUtils,
  Support, Device.SMART.List;

type
  TFallbackCDISupport = class sealed(TNSTSupport)
  private
    FSMARTSupport: TSMARTSupport;
    FIsWriteValueSupported: Boolean;
    function GetCDISupport: TSupportStatus;
    function IsSupportedByCDI: Boolean;
    function IsValidSSD: Boolean;
  public
    function GetSupportStatus: TSupportStatus; override;
    function GetSMARTInterpreted(SMARTValueList: TSMARTValueList):
      TSMARTInterpreted; override;
    destructor Destroy; override;
  end;

implementation

{ TFallbackCDISupport }

function TFallbackCDISupport.IsSupportedByCDI: Boolean;
var
  SMARTSupportFactory: TSMARTSupportFactory;
begin
  result := FSMARTSupport <> nil;
  if result then
  begin
    exit;
  end;

  try
    try
      SMARTSupportFactory := TSMARTSupportFactory.Create;
      FSMARTSupport := SMARTSupportFactory.GetSuitableSMARTSupport(
        Identify, SMART);
      result := FSMARTSupport <> nil;
    except
      result := false;
    end;
  finally
    if SMARTSupportFactory <> nil then
      FreeAndNil(SMARTSupportFactory);
    if FSMARTSupport <> nil then
      FIsWriteValueSupported := FSMARTSupport.IsWriteValueSupported(SMART);
  end;
end;

function TFallbackCDISupport.IsValidSSD: Boolean;
begin
  result := Identify.IsDataSetManagementSupported;
end;

destructor TFallbackCDISupport.Destroy;
begin
  if FSMARTSupport <> nil then
    FreeAndNil(FSMARTSupport);
  inherited;
end;

function TFallbackCDISupport.GetCDISupport: TSupportStatus;
begin
  if (FSMARTSupport.IsInsufficientSMART) or (not FIsWriteValueSupported) then
  begin
    result.Supported := CDIInsufficient;
    result.TotalWriteType := TTotalWriteType.WriteNotSupported;
  end
  else
  begin
    result.Supported := CDISupported;
    result.TotalWriteType := TTotalWriteType.WriteSupportedAsValue;
  end;
  result.FirmwareUpdate := false;
end;

function TFallbackCDISupport.GetSupportStatus: TSupportStatus;
begin
  result.Supported := NotSupported;
  if IsSupportedByCDI and IsValidSSD then
    result := GetCDISupport;
end;

function TFallbackCDISupport.GetSMARTInterpreted(
  SMARTValueList: TSMARTValueList): TSMARTInterpreted;
const
  ReplacedSectorThreshold = 50;
  EraseErrorThreshold = 10;
begin
  try
    result := FSMARTSupport.GetSMARTInterpreted(SMARTValueList);
  except
    on E: EEntryNotFound do
    begin
      FillChar(result, SizeOf(result), 0);
    end;
  end;
  result.SMARTAlert.ReplacedSector :=
    result.ReplacedSectors >= ReplacedSectorThreshold;
  result.SMARTAlert.ReadEraseError :=
    result.ReadEraseError.Value >= EraseErrorThreshold;
  if FSMARTSupport <> nil then
    result.SMARTAlert.CriticalError :=
      FSMARTSupport.GetDriveStatus(SMARTValueList) = BAD;
end;

end.
