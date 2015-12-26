unit Getter.SlotSpeed;

interface

uses
  Windows, ActiveX, ComObj, Variants, SysUtils, Dialogs,
  OSFile, Getter.SlotSpeedByDeviceID, Device.SlotSpeed;

type
  TSlotSpeedGetter = class sealed(TOSFile)
  public
    function GetSlotSpeed: TSlotMaxCurrSpeed;
  private
    function ConnectToWMIObjectByMoniker: IDispatch;
    function GetStorageDeviceID(WMIObject: IDispatch): String;
    function GetDefaultMonikerFromObjectPath(ObjectPath: String;
      BindableContext: IBindCtx): IMoniker;
    function GetLocalhostWMIRepositoryURI: String;
    function GetMonikerBindableContext: IBindCtx;
    function GetReferredObjectByMoniker(DefaultMoniker: IMoniker;
      BindableContext: IBindCtx): IDispatch;
    function TryToGetSlotDataWidth: TSlotMaxCurrSpeed;
    function GetControllerDeviceID(WMIObject: IDispatch;
      StorageDeviceID: String): String;
    function GetOSSlotInformationByDeviceID(
      const ControllerDeviceID: String): TOSSlotMaxCurrSpeed;
    function InterpretOSSlotSpeed(
      const OSSlotSpeed: TOSSlotMaxCurrSpeed): TSlotMaxCurrSpeed;
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
  WMIObject: IDispatch;
  StorageDeviceID: String;
  ControllerDeviceID: String;
  OSSlotSpeed: TOSSlotMaxCurrSpeed;
begin
  WMIObject := ConnectToWMIObjectByMoniker;
  StorageDeviceID := GetStorageDeviceID(WMIObject);
  ControllerDeviceID := GetControllerDeviceID(WMIObject, StorageDeviceID);
  OSSlotSpeed := GetOSSlotInformationByDeviceID(ControllerDeviceID);
  result := InterpretOSSlotSpeed(OSSlotSpeed);
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

function TSlotSpeedGetter.ConnectToWMIObjectByMoniker: IDispatch;
var
  ContextToBindMoniker: IBindCtx;
  DefaultMoniker: IMoniker;
begin
  ContextToBindMoniker := GetMonikerBindableContext;
  DefaultMoniker :=
    GetDefaultMonikerFromObjectPath(
      GetLocalhostWMIRepositoryURI,
      ContextToBindMoniker);
  result :=
    GetReferredObjectByMoniker(
      DefaultMoniker,
      ContextToBindMoniker);
end;

function TSlotSpeedGetter.GetMonikerBindableContext: IBindCtx;
const
  ReservedAndMustBeZero = 0;
begin
  OleCheck(CreateBindCtx(ReservedAndMustBeZero, result));
end;

function TSlotSpeedGetter.GetDefaultMonikerFromObjectPath
  (ObjectPath: String; BindableContext: IBindCtx): IMoniker;
var
  LengthOfURISuccessfullyParsed: Integer;
begin
  OleCheck(
    MkParseDisplayName(BindableContext,
      PWideChar(ObjectPath),
      LengthOfURISuccessfullyParsed,
      result));
end;

function TSlotSpeedGetter.GetLocalhostWMIRepositoryURI: String;
const
  WMIService = 'winmgmts:\\';
  Localhost = 'localhost\';
  WMIRepositoryPrefix = 'root\cimv2';
begin
  result := WMIService + Localhost + WMIRepositoryPrefix;
end;

function TSlotSpeedGetter.GetReferredObjectByMoniker
  (DefaultMoniker: IMoniker; BindableContext: IBindCtx): IDispatch;
begin
  OleCheck(
    DefaultMoniker.BindToObject(BindableContext, nil, IUnknown, result));
end;

function TSlotSpeedGetter.GetStorageDeviceID(WMIObject: IDispatch):
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
    OleVariant(WMIObject).ExecQuery(PreQuery + GetPathOfFileAccessing +
      PostQuery);
  EnumVariant :=
    IUnknown(ResultAsOleVariant._NewEnum) as IEnumVARIANT;
  EnumVariant.Next(OneDeviceInformationNeeded, CurrentDevice,
    DeviceReturned);
  result := CurrentDevice.DeviceID;
end;

function TSlotSpeedGetter.GetControllerDeviceID(WMIObject: IDispatch;
  StorageDeviceID: String): String;
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
    OleVariant(WMIObject).ExecQuery(PreQuery + StorageDeviceID +
      PostQuery);
  EnumVariant :=
    IUnknown(ResultAsOleVariant._NewEnum) as IEnumVARIANT;
  EnumVariant.Next(OneDeviceInformationNeeded, CurrentDevice,
    DeviceReturned);
  result := CurrentDevice.DeviceID;
end;

end.
