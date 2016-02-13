unit OS.SetupAPI;

interface

uses
  SysUtils, Windows,
  Getter.OS.Version, OS.Version.Helper;

const
  MAX_PATH = 260;
  CR_SUCCESS = $00000000;

type
  THDEVINFO = Pointer;
  PSP_DevInfo_Data = ^TSP_DevInfo_Data;
  SP_DEVINFO_DATA = packed record
    cbSize: DWORD;
    ClassGuid: TGUID;
    DevInst: NativeInt;
    Reserved: LongWord;
  end;
  TSP_DevInfo_Data = SP_DEVINFO_DATA;
  PDEVPROPKEY = ^TDEVPROPKEY;
  DEVPROPKEY = packed record
    fmtid: TGUID;
    pid: Pointer;
  end;
  TDEVPROPKEY = DEVPROPKEY;
  DEVPROPTYPE = Pointer;
  PCWSTR = PWCHAR;
  TDEVINST = DWORD;
  TSetupDiGetDevicePropertyW = function (DeviceInfoSet: THDEVINFO;
    DeviceInfoData: PSP_DEVINFO_DATA; const PropertyKey: PDEVPROPKEY;
    var PropertyType:DEVPROPTYPE; PropertyBuffer:PBYTE;PropertyBufferSize:DWORD;
    RequiredSize:PDWORD; Flags:DWORD): BOOL; stdcall;
  TSetupDiGetClassDevsW = function (const ClassGuid: PGUID; Enumerator: PCWSTR;
    hwndParent: HWND; Flags: DWORD): THDEVINFO; stdcall;
  TSetupDiEnumDeviceInfo = function (DeviceInfoSet: THDEVINFO;
    MemberIndex: DWORD; DeviceInfoData: PSP_DEVINFO_DATA): BOOL; stdcall;
  TCM_Get_Device_IDW = function (DeviceInstanceHandle: TDEVINst; Buffer: PCWSTR;
    Bufferlen : ULONG; ulFlags:ULONG): DWORD; stdcall;
  TSetupDiGetDeviceRegistryPropertyW = function (DeviceInfoSet: THDEVINFO;
    const DeviceInfoData: SP_DevInfo_Data; Property_: DWORD;
    var PropertyRegDataType: DWORD; PropertyBuffer: PBYTE;
    PropertyBufferSize: DWORD; var RequiredSize: DWORD): BOOL; stdcall;
  EBelowVistaException = class(ENotImplemented);
  TSetupAPI = class
  private
    DLLHandle: THandle;
    type
      TSetupAPIFunctions = record
        SetupDiGetDevicePropertyW: TSetupDiGetDevicePropertyW;
        SetupDiGetClassDevsW: TSetupDiGetClassDevsW;
        SetupDiEnumDeviceInfo: TSetupDiEnumDeviceInfo;
        CM_Get_Device_IDW: TCM_Get_Device_IDW;
        SetupDiGetDeviceRegistryPropertyW: TSetupDiGetDeviceRegistryPropertyW;
      end;
    var
      SetupAPIFunctions: TSetupAPIFunctions;
    constructor CreateSingletonInstance;
  public
    function GetSetupAPIFunctions: TSetupAPIFunctions;
    class function Create: TSetupAPI;
    destructor Destroy; override;
  end;

const
  DIGCF_PRESENT = $00000002;
  DIGCF_ALLCLASSES = $00000004;
  DIGCF_PROFILE = $00000008;
  DIGCF_DEVICEINTERFACE = $00000010;
  INVALID_HANDLE_VALUE = DWORD($FFFFFFFF);
  MAX_DEVICE_ID_LEN = 200;
  SPDRP_DEVICEDESC = ($00000000);
  DEVPKEY_Device_BusReportedDeviceDesc: TDEVPROPKEY =
    (fmtid: '{540b947e-8b40-45bc-a8a2-6a0b894cbda2}'; pid: pointer(4));
  DEVPKEY_PciDevice_CurrentLinkSpeed: TDEVPROPKEY =
    (fmtid: '{3ab22e31-8264-4b4e-9af5-a8d2d8e33e62}'; pid: pointer(9));
  DEVPKEY_PciDevice_CurrentLinkWidth: TDEVPROPKEY =
    (fmtid: '{3ab22e31-8264-4b4e-9af5-a8d2d8e33e62}'; pid: pointer(10));
  DEVPKEY_PciDevice_MaxLinkSpeed: TDEVPROPKEY =
    (fmtid: '{3ab22e31-8264-4b4e-9af5-a8d2d8e33e62}'; pid: pointer(11));
  DEVPKEY_PciDevice_MaxLinkWidth: TDEVPROPKEY =
    (fmtid: '{3ab22e31-8264-4b4e-9af5-a8d2d8e33e62}'; pid: pointer(12));

var
  SetupAPI: TSetupAPI;

implementation


{ TSetupAPI }

class function TSetupAPI.Create: TSetupAPI;
begin
  if SetupAPI = nil then
    SetupAPI := TSetupAPI.CreateSingletonInstance;
  result := SetupAPI;
end;

constructor TSetupAPI.CreateSingletonInstance;
begin
  DLLHandle := LoadLibrary(PChar('Setupapi.dll'));
  SetupAPIFunctions.SetupDiGetDevicePropertyW :=
    GetProcAddress(
      DLLHandle, 'SetupDiGetDevicePropertyW');
  SetupAPIFunctions.SetupDiGetClassDevsW :=
    GetProcAddress(
      DLLHandle, 'SetupDiGetClassDevsW');
  SetupAPIFunctions.SetupDiEnumDeviceInfo :=
    GetProcAddress(
      DLLHandle, 'SetupDiEnumDeviceInfo');
  SetupAPIFunctions.CM_Get_Device_IDW :=
    GetProcAddress(
      DLLHandle, 'CM_Get_Device_IDW');
  SetupAPIFunctions.SetupDiGetDeviceRegistryPropertyW :=
    GetProcAddress(
      DLLHandle, 'SetupDiGetDeviceRegistryPropertyW');
end;

destructor TSetupAPI.Destroy;
begin
  FreeLibrary(DLLHandle);
  inherited;
end;

function TSetupAPI.GetSetupAPIFunctions: TSetupAPIFunctions;
begin
  if IsBelowVista(VersionHelper.Version) then
    raise EBelowVistaException.Create(
      'SetupAPI Error: OS Version is Below Vista');
  result := SetupAPIFunctions;
end;

initialization
  SetupAPI := TSetupAPI.Create;
finalization
  SetupAPI.Free;
end.
