unit OS.SetupAPI;

interface

uses
  Windows;

const
  MAX_PATH = 260;
  CR_SUCCESS = $00000000;

type
  THDEVINFO = Pointer;
  PSP_DevInfo_Data = ^TSP_DevInfo_Data;
  SP_DEVINFO_DATA = packed record
    cbSize: DWORD;
    ClassGuid: TGUID;
    DevInst: DWORD;
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

const
  DIGCF_PRESENT = $00000002;
  DIGCF_ALLCLASSES = $00000004;
  DIGCF_PROFILE = $00000008;
  DIGCF_DEVICEINTERFACE = $00000010;
  INVALID_HANDLE_VALUE = DWORD($FFFFFFFF);
  MAX_DEVICE_ID_LEN = 200;
  SPDRP_DEVICEDESC = ($00000000);
  DEVPKEY_Device_BusReportedDeviceDesc: TDEVPROPKEY = (fmtid: '{540b947e-8b40-45bc-a8a2-6a0b894cbda2}'; pid: pointer(4));
  DEVPKEY_PciDevice_CurrentLinkSpeed: TDEVPROPKEY = (fmtid: '{3ab22e31-8264-4b4e-9af5-a8d2d8e33e62}'; pid: pointer(9));
  DEVPKEY_PciDevice_CurrentLinkWidth: TDEVPROPKEY = (fmtid: '{3ab22e31-8264-4b4e-9af5-a8d2d8e33e62}'; pid: pointer(10));
  DEVPKEY_PciDevice_MaxLinkSpeed: TDEVPROPKEY = (fmtid: '{3ab22e31-8264-4b4e-9af5-a8d2d8e33e62}'; pid: pointer(11));
  DEVPKEY_PciDevice_MaxLinkWidth: TDEVPROPKEY = (fmtid: '{3ab22e31-8264-4b4e-9af5-a8d2d8e33e62}'; pid: pointer(12));

function SetupDiGetDeviceProperty(DeviceInfoSet: THDEVINFO; DeviceInfoData: PSP_DEVINFO_DATA; const PropertyKey: PDEVPROPKEY; var PropertyType:DEVPROPTYPE; PropertyBuffer:PBYTE;PropertyBufferSize:DWORD; RequiredSize:PDWORD; Flags:DWORD): BOOL; stdcall; external 'Setupapi.DLL' name 'SetupDiGetDevicePropertyW';
function SetupDiGetClassDevsW(const ClassGuid: PGUID; Enumerator: PCWSTR; hwndParent: HWND; Flags: DWORD): THDEVINFO; stdcall; external 'Setupapi.DLL' name 'SetupDiGetClassDevsW';
function SetupDiEnumDeviceInfo(DeviceInfoSet: THDEVINFO; MemberIndex: DWORD; DeviceInfoData: PSP_DEVINFO_DATA): BOOL; stdcall; external 'Setupapi.DLL' name 'SetupDiEnumDeviceInfo';
function CM_Get_Device_IDW(DeviceInstanceHandle: TDEVINst; Buffer:PCWSTR; Bufferlen : ULONG; ulFlags:ULONG): DWORD; stdcall; external 'Setupapi.DLL' name 'CM_Get_Device_IDW';
function SetupDiGetDeviceRegistryPropertyW(DeviceInfoSet: THDEVINFO; const DeviceInfoData: SP_DevInfo_Data; Property_: DWORD; var PropertyRegDataType: DWORD; PropertyBuffer: PBYTE; PropertyBufferSize: DWORD; var RequiredSize: DWORD): BOOL; stdcall; external 'Setupapi.DLL' name 'SetupDiGetDeviceRegistryPropertyW';

implementation

end.
