unit Getter.DeviceDriver;

interface

uses
  Windows, ActiveX, ComObj, Variants, SysUtils, Dialogs,
  OSFile, Getter.PhysicalDriveList, Device.PhysicalDrive,
  Device.PhysicalDrive.List, CommandSet.Factory;

type
  TDeviceDriver = record
    Name: String;
    Provider: String;
    Date: String;
    InfName: String;
    Version: String;
  end;

  TDeviceDriverGetter = class sealed(TOSFile)
  public
    function GetDeviceDriver: TDeviceDriver;
  private
    DeviceDriver: TDeviceDriver;
    function ConnectToWMIObjectByMoniker: IDispatch;
    function GetStorageDeviceID(WMIObject: IDispatch): String;
    function GetDefaultMonikerFromObjectPath(ObjectPath: String;
      BindableContext: IBindCtx): IMoniker;
    function GetLocalhostWMIRepositoryURI: String;
    function GetMonikerBindableContext: IBindCtx;
    function GetReferredObjectByMoniker(DefaultMoniker: IMoniker;
      BindableContext: IBindCtx): IDispatch;
    procedure TryToGetDeviceDriver;
    function GetSCSIControllerDeviceID(WMIObject: IDispatch;
      StorageDeviceID: String): String;
    function GetDeviceDriverInformation(WMIObject: IDispatch;
      ControllerDeviceID: String): TDeviceDriver;
    function GetIDEControllerDeviceID(WMIObject: IDispatch;
      StorageDeviceID: String): String;
  end;

implementation

{ TDeviceDriverGetter }

function TDeviceDriverGetter.GetDeviceDriver: TDeviceDriver;
begin
  FillChar(DeviceDriver, SizeOf(DeviceDriver), 0);
  try
    TryToGetDeviceDriver;
  except
    FillChar(DeviceDriver, SizeOf(DeviceDriver), 0);
  end;
  result := DeviceDriver;
end;

procedure TDeviceDriverGetter.TryToGetDeviceDriver;
var
  WMIObject: IDispatch;
  StorageDeviceID: String;
  ControllerDeviceID: String;
begin
  WMIObject := ConnectToWMIObjectByMoniker;
  StorageDeviceID := GetStorageDeviceID(WMIObject);
  try
    ControllerDeviceID := GetSCSIControllerDeviceID(WMIObject,
      StorageDeviceID);
  except
    ControllerDeviceID := GetIDEControllerDeviceID(WMIObject,
      StorageDeviceID);
  end;
  DeviceDriver := GetDeviceDriverInformation(WMIObject,
    ControllerDeviceID);
end;

function TDeviceDriverGetter.ConnectToWMIObjectByMoniker: IDispatch;
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

function TDeviceDriverGetter.GetMonikerBindableContext: IBindCtx;
const
  ReservedAndMustBeZero = 0;
begin
  OleCheck(CreateBindCtx(ReservedAndMustBeZero, result));
end;

function TDeviceDriverGetter.GetDefaultMonikerFromObjectPath
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

function TDeviceDriverGetter.GetLocalhostWMIRepositoryURI: String;
const
  WMIService = 'winmgmts:\\';
  Localhost = 'localhost\';
  WMIRepositoryPrefix = 'root\cimv2';
begin
  result := WMIService + Localhost + WMIRepositoryPrefix;
end;

function TDeviceDriverGetter.GetReferredObjectByMoniker
  (DefaultMoniker: IMoniker; BindableContext: IBindCtx): IDispatch;
begin
  OleCheck(
    DefaultMoniker.BindToObject(BindableContext, nil, IUnknown, result));
end;

function TDeviceDriverGetter.GetStorageDeviceID(WMIObject: IDispatch):
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

function TDeviceDriverGetter.GetSCSIControllerDeviceID(WMIObject: IDispatch;
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

function TDeviceDriverGetter.GetIDEControllerDeviceID(WMIObject: IDispatch;
  StorageDeviceID: String): String;
const
  PreQuery = 'ASSOCIATORS OF {Win32_PnPEntity.DeviceID=''';
  PostQuery = '''} WHERE AssocClass=Win32_IDEControllerDevice';
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

function TDeviceDriverGetter.GetDeviceDriverInformation(WMIObject: IDispatch;
  ControllerDeviceID: String): TDeviceDriver;
const
  PreQuery = 'SELECT * FROM Win32_PnPSignedDriver WHERE DeviceID=''';
  PostQuery = '''';
  OneDeviceInformationNeeded = 1;
var
  ResultAsOleVariant: OleVariant;
  EnumVariant: IEnumVARIANT;
  CurrentDevice: OleVariant;
  DeviceReturned: Cardinal;
  Query: String;
begin
  ControllerDeviceID := StringReplace(ControllerDeviceID, '\', '\\',
    [rfReplaceAll]);
  Query := PreQuery + ControllerDeviceID + PostQuery;
  ResultAsOleVariant := OleVariant(WMIObject).ExecQuery(Query);
  EnumVariant :=
    IUnknown(ResultAsOleVariant._NewEnum) as IEnumVARIANT;
  EnumVariant.Next(OneDeviceInformationNeeded, CurrentDevice,
    DeviceReturned);
  result.Name := CurrentDevice.DeviceName;
  result.Provider := CurrentDevice.DriverProviderName;
  result.Date := Copy(CurrentDevice.DriverDate, 1, 8);
  result.InfName := CurrentDevice.InfName;
  result.Version := CurrentDevice.DriverVersion;
end;

end.
