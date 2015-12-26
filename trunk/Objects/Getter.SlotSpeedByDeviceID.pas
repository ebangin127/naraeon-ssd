unit Getter.SlotSpeedByDeviceID;

interface

uses
  SysUtils, Windows,
  OS.SetupAPI;

type
  TOSSlotSpeed = record
    SpecVersion: Byte;
    LinkWidth: Byte;
  end;
  TOSSlotMaxCurrSpeed = record
    Maximum: TOSSlotSpeed;
    Current: TOSSlotSpeed;
  end;
  TIDtoSlotSpeedGetter = class
  private
    FClassDeviceInformations: THDEVINFO;
    DeviceInfoData: TSP_DEVINFO_DATA;
    PDeviceInfoData: PSP_DevInfo_Data;
    ResultCache: TOSSlotMaxCurrSpeed;
    FDeviceID: String;
    function DeviceIDFound: Boolean;
    procedure SetResultCache;
    function GetCurrentSpeed: TOSSlotSpeed;
    function GetMaximumSpeed: TOSSlotSpeed;
  public
    constructor Create;
    function GetSlotSpeed(const DeviceID: String): TOSSlotMaxCurrSpeed;
  end;

implementation

{ TIDtoSlotSpeedGetter }

constructor TIDtoSlotSpeedGetter.Create;
const
  SCSIAdapterGUIDInString = '{4d36e97b-e325-11ce-bfc1-08002be10318}';
var
  SCSIAdaptorGUID: TGUID;
begin
  SCSIAdaptorGUID := StringToGUID(SCSIAdapterGUIDInString);
  FClassDeviceInformations := SetupDiGetClassDevsW(@SCSIAdaptorGUID, nil, 0,
    DIGCF_PRESENT);
  PDeviceInfoData := @DeviceInfoData;
end;

function TIDtoSlotSpeedGetter.GetSlotSpeed(const DeviceID: String):
  TOSSlotMaxCurrSpeed;
var
  CurrentDevice: Cardinal;
  LastResult: LongBool;
begin
  CurrentDevice := 0;
  FDeviceID := DeviceID;
  FillChar(ResultCache, SizeOf(ResultCache), #0);
  repeat
    DeviceInfoData.cbSize := sizeof(TSP_DEVINFO_DATA);
    LastResult := SetupDiEnumDeviceInfo(FClassDeviceInformations, CurrentDevice,
      @DeviceInfoData);
    if LastResult then
      if DeviceIDFound then
        break;
    Inc(CurrentDevice);
  until not LastResult;
  result := ResultCache;
end;

function TIDtoSlotSpeedGetter.DeviceIDFound: Boolean;
var
  DeviceIDBuffer: Array[0..MAX_DEVICE_ID_LEN] of WCHAR;
  LastResult: Cardinal;
begin
  LastResult := CM_Get_Device_IDW(DeviceInfoData.DevInst, @DeviceIDBuffer,
    MAX_PATH, 0);
  result := (LastResult = ERROR_SUCCESS) and
    (WideCharToString(DeviceIDBuffer) = FDeviceID);
  if result then
    SetResultCache;
end;

procedure TIDtoSlotSpeedGetter.SetResultCache;
begin
  ResultCache.Maximum := GetMaximumSpeed;
  ResultCache.Current := GetCurrentSpeed;
end;

function TIDtoSlotSpeedGetter.GetMaximumSpeed: TOSSlotSpeed;
var
  ResultBuffer: array[0..1023] of char;
  RequiredSize: DWORD;
  PropertyType: DEVPROPTYPE;
begin
  if SetupDiGetDeviceProperty(FClassDeviceInformations, @DeviceInfoData,
    @DEVPKEY_PciDevice_MaxLinkSpeed, PropertyType, @ResultBuffer,
    sizeof(ResultBuffer), @RequiredSize, 0) then
      result.SpecVersion := Ord(ResultBuffer[0]);
  if SetupDiGetDeviceProperty(FClassDeviceInformations, @DeviceInfoData,
    @DEVPKEY_PciDevice_MaxLinkWidth, PropertyType, @ResultBuffer,
    sizeof(ResultBuffer), @RequiredSize, 0) then
      result.LinkWidth := Ord(ResultBuffer[0]);
end;

function TIDtoSlotSpeedGetter.GetCurrentSpeed: TOSSlotSpeed;
var
  ResultBuffer: array[0..1023] of char;
  RequiredSize: DWORD;
  PropertyType: DEVPROPTYPE;
begin
  if SetupDiGetDeviceProperty(FClassDeviceInformations, @DeviceInfoData,
    @DEVPKEY_PciDevice_CurrentLinkSpeed, PropertyType, @ResultBuffer,
    sizeof(ResultBuffer), @RequiredSize, 0) then
      result.SpecVersion := Ord(ResultBuffer[0]);
  if SetupDiGetDeviceProperty(FClassDeviceInformations, @DeviceInfoData,
    @DEVPKEY_PciDevice_CurrentLinkWidth, PropertyType, @ResultBuffer,
    sizeof(ResultBuffer), @RequiredSize, 0) then
      result.LinkWidth := Ord(ResultBuffer[0]);
end;

end.
