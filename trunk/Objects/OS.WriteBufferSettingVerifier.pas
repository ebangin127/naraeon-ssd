unit OS.WriteBufferSettingVerifier;

interface

uses
  Classes, SysUtils,
  Support, Support.Sandforce, Support.Factory,
  Registry.Helper.Internal, Registry.Helper;

type
  TWriteBufferSettingVerifier = class
  {$IfNDef UNITTEST}
  private
  {$EndIf}
    function SplitIntoModelAndFirmware(
      ModelAndFirmware: String; out Model, Firmware: String): Boolean;
  private
    ModelList: TStringList;
    DeviceList: TStringList;
    function CheckAndCorrectByInterface(const InterfaceName: String):
      TStringList;
    function CheckAndCorrectByDevice(ModelPathInRegistry: TRegistryPath;
      const CurrentDevice: String): String;
    function CheckAndCorrectByModel(InterfacePathInRegistry: TRegistryPath;
      const ModelAndFirmware: String): TStringList;
    function CheckDevice(DevicePathInRegistry: TRegistryPath): Boolean;
    function CheckSupportStatus(const ModelAndFirmware: String): Boolean;
    function CorrectDevice(DevicePathInRegistry: TRegistryPath): Boolean;
    function UnderbarToSpace(const ModelAndFirmware: String): String;
    function GetDeviceFriendlyName(DevicePathInRegistry: TRegistryPath): String;
  public
    function CheckAndCorrect: TStringList;
  end;

implementation

{ TWriteBufferSettingVerifier }

function TWriteBufferSettingVerifier.UnderbarToSpace(
  const ModelAndFirmware: String): String;
var
  CurrentPoint: Integer;
begin
  result := ModelAndFirmware;
  for CurrentPoint := 1 to Length(result) do
    if result[CurrentPoint] = '_' then
      result[CurrentPoint] := ' ';
end;

function TWriteBufferSettingVerifier.SplitIntoModelAndFirmware(
  ModelAndFirmware: String; out Model, Firmware: String): Boolean;
const
  IdeDiskString = 'Disk';
  FirmwareLength = 8;
var
  ModelLength: Integer;
begin
  if Copy(ModelAndFirmware, 1, Length(IdeDiskString)) <> IdeDiskString then
    exit(false);

  ModelAndFirmware := UnderbarToSpace(ModelAndFirmware);
  ModelLength := Length(ModelAndFirmware) -
    (Length(IdeDiskString) + FirmwareLength);
  Model := Trim(Copy(ModelAndFirmware, Length(IdeDiskString) + 1, ModelLength));
  Firmware := Trim(Copy(ModelAndFirmware,
    ModelLength + Length(IdeDiskString) + 1, FirmwareLength));

  result := true;
end;

function TWriteBufferSettingVerifier.CheckSupportStatus(
  const ModelAndFirmware: String): Boolean;
var
  SupportFactory: TNSTSupportFactory;
  NSTSupport: TNSTSupport;
  Model, Firmware: String;
begin
  if not SplitIntoModelAndFirmware(ModelAndFirmware, Model, Firmware) then
    exit(false);

  SupportFactory := TNSTSupportFactory.Create;
  try
    NSTSupport := SupportFactory.GetSuitableNSTSupport(Model, Firmware);
    result :=
      (NSTSupport <> nil) and
      (NSTSupport.GetSupportStatus.Supported) and
      (not (NSTSupport is TSandforceNSTSupport));
    FreeAndNil(NSTSupport);
  finally
    FreeAndNil(SupportFactory);
  end;
end;

function TWriteBufferSettingVerifier.CheckDevice(
  DevicePathInRegistry: TRegistryPath): Boolean;
const
  WriteBufferIsDisabled = 0;
begin
  try
    result := NSTRegistry.GetRegInt(DevicePathInRegistry) =
      WriteBufferIsDisabled;
  except
    result := false;
  end;
end;

function TWriteBufferSettingVerifier.CorrectDevice(
  DevicePathInRegistry: TRegistryPath): Boolean;
const
  WriteBufferIsEnabled = 1;
begin
  result := NSTRegistry.SetRegInt(DevicePathInRegistry,
    WriteBufferIsEnabled);
end;

function TWriteBufferSettingVerifier.GetDeviceFriendlyName(
  DevicePathInRegistry: TRegistryPath): String;
begin
  DevicePathInRegistry.ValueName := 'FriendlyName';
  result := NSTRegistry.GetRegStr(DevicePathInRegistry);
end;

function TWriteBufferSettingVerifier.CheckAndCorrectByDevice(
  ModelPathInRegistry: TRegistryPath; const CurrentDevice: String): String;
var
  IsDeviceNeedCorrection: Boolean;
  DevicePath: String;
begin
  result := '';
  DevicePath := ModelPathInRegistry.PathUnderHKEY
    + '\' + CurrentDevice;
  ModelPathInRegistry.PathUnderHKEY :=
    DevicePath + '\Device Parameters\Disk';
  ModelPathInRegistry.ValueName := 'UserWriteCacheSetting';
  IsDeviceNeedCorrection := CheckDevice(ModelPathInRegistry);
  if IsDeviceNeedCorrection then
  begin
    CorrectDevice(ModelPathInRegistry);
    ModelPathInRegistry.PathUnderHKEY := DevicePath;
    result := GetDeviceFriendlyName(ModelPathInRegistry);
  end;
end;

function TWriteBufferSettingVerifier.CheckAndCorrectByModel(
  InterfacePathInRegistry: TRegistryPath;
  const ModelAndFirmware: String): TStringList;
var
  CurrentDevice: String;
  CurrentDeviceResult: String;
begin
  result := TStringList.Create;
  if not CheckSupportStatus(ModelAndFirmware) then
    exit;
  InterfacePathInRegistry.PathUnderHKEY := InterfacePathInRegistry.PathUnderHKEY
    + '\' + ModelAndFirmware;
  NSTRegistry.GetKeyList(InterfacePathInRegistry, DeviceList);
  for CurrentDevice in DeviceList do
  begin
    CurrentDeviceResult :=
      CheckAndCorrectByDevice(InterfacePathInRegistry, CurrentDevice);
    if CurrentDeviceResult <> '' then
      result.Add(CurrentDeviceResult);
  end;
end;

function TWriteBufferSettingVerifier.CheckAndCorrectByInterface(
  const InterfaceName: String): TStringList;
const
  DeviceBasePath: TRegistryPath =
    (Root: LocalMachine;
     PathUnderHKEY: 'SYSTEM\CurrentControlSet\Enum\';
     ValueName: '');
var
  BasePathWithInterfaceName: TRegistryPath;
  ModelAndFirmware: String;
  ResultInModel: TStringList;
begin
  result := TStringList.Create;
  BasePathWithInterfaceName := DeviceBasePath;
  BasePathWithInterfaceName.PathUnderHKEY :=
    BasePathWithInterfaceName.PathUnderHKEY + InterfaceName;
  NSTRegistry.GetKeyList(BasePathWithInterfaceName, ModelList);
  for ModelAndFirmware in ModelList do
  begin
    ResultInModel :=
      CheckAndCorrectByModel(
        BasePathWithInterfaceName, ModelAndFirmware);
    if ResultInModel.Count > 0 then
      result.AddStrings(ResultInModel);
    FreeAndNil(ResultInModel);
  end;
end;

function TWriteBufferSettingVerifier.CheckAndCorrect: TStringList;
const
  InterfaceSet: Array[0..1] of String = ('IDE', 'SCSI');
var
  CurrentInterfaceName: String;
  ResultInSpecificInterface: TStringList;
begin
  result := TStringList.Create;
  ModelList := TStringList.Create;
  DeviceList := TStringList.Create;
  for CurrentInterfaceName in InterfaceSet do
  begin
    ResultInSpecificInterface :=
      CheckAndCorrectByInterface(CurrentInterfaceName);
    result.AddStrings(ResultInSpecificInterface);
    FreeAndNil(ResultInSpecificInterface);
  end;
  FreeAndNil(ModelList);
  FreeAndNil(DeviceList);
end;

end.

