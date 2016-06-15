unit Getter.SlotSpeed;

interface

uses
  Windows, ActiveX, ComObj, Variants, SysUtils, Dialogs,
  OSFile.ForInternal, Getter.SlotSpeedByDeviceID, Device.SlotSpeed, WMI;

type
  TSlotSpeedGetter = class sealed(TOSFileForInternal)
  public
    function GetSlotSpeed: TSlotMaxCurrSpeed;
  private
    function GetStorageDeviceID(const WMIObject: OleVariant): String;
    function TryToGetSlotDataWidth: TSlotMaxCurrSpeed;
    function GetControllerDeviceID(const WMIObject: OleVariant;
      const StorageDeviceID: String): String;
    function GetOSSlotInformationByDeviceID(
      const ControllerDeviceID: String): TOSSlotMaxCurrSpeed;
    function InterpretOSSlotSpeed(
      const OSSlotSpeed: TOSSlotMaxCurrSpeed): TSlotMaxCurrSpeed;
    procedure FreeWMIConnectionIfNeeded(
      const ThisThreadWMIConnection: TWMIConnection);
    function GetWMIConnection: TWMIConnection;
  end;

implementation

{ TDeviceDriverGetter }

function TSlotSpeedGetter.GetSlotSpeed: TSlotMaxCurrSpeed;
begin
  try
    result := TryToGetSlotDataWidth;
  except
    FillChar(result, SizeOf(result), #0);
  end;
end;

function TSlotSpeedGetter.TryToGetSlotDataWidth: TSlotMaxCurrSpeed;
var
  WMIConnection: TWMIConnection;
  WMIObject: OleVariant;
  StorageDeviceID: String;
  ControllerDeviceID: String;
  OSSlotSpeed: TOSSlotMaxCurrSpeed;
begin
  WMIConnection := GetWMIConnection;
  try
    WMIObject := WMIConnection.GetWMIConnection;
    StorageDeviceID := GetStorageDeviceID(WMIObject);
    ControllerDeviceID := GetControllerDeviceID(WMIObject, StorageDeviceID);
    OSSlotSpeed := GetOSSlotInformationByDeviceID(ControllerDeviceID);
    result := InterpretOSSlotSpeed(OSSlotSpeed);
  finally
    FreeWMIConnectionIfNeeded(WMIConnection);
  end;
end;

function TSlotSpeedGetter.GetWMIConnection: TWMIConnection;
begin
  if WMIConnection.IsMyConnection then
    exit(WMIConnection)
  else
    exit(TWMIConnection.Create);
end;

procedure TSlotSpeedGetter.FreeWMIConnectionIfNeeded(
  const ThisThreadWMIConnection: TWMIConnection);
begin
  if not WMIConnection.IsMyConnection then
    ThisThreadWMIConnection.Free;
end;

function TSlotSpeedGetter.GetOSSlotInformationByDeviceID(
  const ControllerDeviceID: String): TOSSlotMaxCurrSpeed;
var
  IDtoSlotInformationGetter: TIDtoSlotSpeedGetter;
begin
  IDtoSlotInformationGetter := TIDtoSlotSpeedGetter.Create;
  result := IDtoSlotInformationGetter.GetSlotSpeed(ControllerDeviceID);
  FreeAndNil(IDtoSlotInformationGetter);
end;

function TSlotSpeedGetter.InterpretOSSlotSpeed(
  const OSSlotSpeed: TOSSlotMaxCurrSpeed): TSlotMaxCurrSpeed;
begin
  result.Maximum.SpecVersion := TPCIeSpecification(
    OSSlotSpeed.Maximum.SpecVersion);
  result.Maximum.LinkWidth := TPCIeDataWidth(OSSlotSpeed.Maximum.LinkWidth);
  result.Current.SpecVersion := TPCIeSpecification(
    OSSlotSpeed.Current.SpecVersion);
  result.Current.LinkWidth := TPCIeDataWidth(OSSlotSpeed.Current.LinkWidth);
end;

function TSlotSpeedGetter.GetStorageDeviceID(const WMIObject: OleVariant):
  String;
const
  PreQuery = 'ASSOCIATORS OF {Win32_DiskDrive.DeviceID=''';
  PostQuery = '''} WHERE ResultClass=Win32_PnPEntity';
  OneDeviceInformationNeeded = 1;
var
  ResultAsOleVariant: OleVariant;
  EnumVariant: IEnumVARIANT;
  CurrentDevice: OleVariant;
  DeviceReturned: Cardinal;
begin
  ResultAsOleVariant :=
    WMIObject.ExecQuery(PreQuery + GetPathOfFileAccessing + PostQuery);
  EnumVariant :=
    IUnknown(ResultAsOleVariant._NewEnum) as IEnumVARIANT;
  EnumVariant.Next(OneDeviceInformationNeeded, CurrentDevice,
    DeviceReturned);
  result := CurrentDevice.DeviceID;
end;

function TSlotSpeedGetter.GetControllerDeviceID(const WMIObject: OleVariant;
  const StorageDeviceID: String): String;
const
  PreQuery = 'ASSOCIATORS OF {Win32_PnPEntity.DeviceID=''';
  PostQuery = '''} WHERE AssocClass=Win32_SCSIControllerDevice';
  OneDeviceInformationNeeded = 1;
var
  ResultAsOleVariant: OleVariant;
  EnumVariant: IEnumVARIANT;
  CurrentDevice: OleVariant;
  DeviceReturned: Cardinal;
begin
  ResultAsOleVariant :=
    WMIObject.ExecQuery(PreQuery + StorageDeviceID + PostQuery);
  EnumVariant :=
    IUnknown(ResultAsOleVariant._NewEnum) as IEnumVARIANT;
  EnumVariant.Next(OneDeviceInformationNeeded, CurrentDevice,
    DeviceReturned);
  result := CurrentDevice.DeviceID;
end;

end.
