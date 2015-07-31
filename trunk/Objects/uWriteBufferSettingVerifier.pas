unit uWriteBufferSettingVerifier;

interface

uses
  Classes, SysUtils,
  uPartitionTrimmer;

type
  TWriteBufferSettingVerifier = class
  private
    function CheckAndCorrectByInterface(InterfaceName: String): TStringList;
    ModelList, DeviceList: TStringList;
  public
    function CheckAndCorrect: TStringList;
  end;

implementation

{ TWriteBufferSettingVerifier }

function TWriteBufferSettingVerifier.SplitIntoModelAndFirmware(
  const ModelAndFirmware: String; out Model, Firmware: String): Boolean;
var
  Model, Firmware: String;
begin
  Assert(false, 'Split not implemented');
end;

function TWriteBufferSettingVerifier.CheckSupportStatus(
  ModelAndFirmware: String): Boolean;
var
  PartitionTrimmer: TPartitionTrimmer;
  Model, Firmware: String;
begin
  PartitionTrimmer := TPartitionTrimmer.Create;
  SplitIntoModelAndFirmware(ModelAndFirmware, Model, Firmware);
  PartitionTrimmer.SetModelAndFirmware(Model, Firmware);
  result :=
    (PartitionTrimmer.GetSupportStatus.Supported) and
    (not PartitionTrimmer.IsSandforce);
  FreeAndNil(PartitionTrimmer);
end;

function TWriteBufferSettingVerifier.CheckDevice(
  DevicePathInRegistry: TRegistryPath): Boolean;
const
  WriteBufferIsDisabled = 0;
begin
  result := TStaticRegistry.GetRegInt(DevicePathInRegistry) =
    WriteBufferIsDisabled;
end;

function TWriteBufferSettingVerifier.CorrectDevice(
  DevicePathInRegistry: TRegistryPath): Boolean;
const
  WriteBufferIsEnabled = 1;
begin
  TStaticRegistry.SetRegInt(DevicePathInRegistry,
    WriteBufferIsEnabled);
end;

function TWriteBufferSettingVerifier.CheckAndCorrectByDevice(
  ModelPathInRegistry: TRegistryPath; CurrentDevice: String): Boolean;
var
  CurrentDevice: String;
begin
  ModelPathInRegistry.PathUnderHKEY := InterfacePathInRegistry.PathUnderHKEY
    + '\' + CurrentDevice;
  ModelPathInRegistry.PathUnderHKEY :=
    ModelPathInRegistry.PathUnderHKEY + '\Device Parameters\Disk';
  ModelPathInRegistry.ValueName := 'UserWriteCacheSetting';
  result := CheckDevice(ModelPathInRegistry);
  if result then
    CorrectDevice(ModelPathInRegistry);
end;

function TWriteBufferSettingVerifier.CheckAndCorrectByModel(
  InterfacePathInRegistry: TRegistryPath;
  ModelAndFirmware: String): TStringList;
var
  CurrentDevice: String;
  ResultInDevice: TStringList;
begin
  if not CheckSupportStatus(ModelAndFirmware) then
    exit;
    
  InterfacePathInRegistry.PathUnderHKEY := InterfacePathInRegistry.PathUnderHKEY
    + '\' + ModelAndFirmware;
  TStaticRegistry.GetKeyList(InterfacePathInRegistry, DeviceList);
  for CurrentDevice in DeviceList do
  begin
    if CheckAndCorrectByDevice(InterfacePathInRegistry, CurrentDevice) then
      result := GetDeviceFriendlyName(InterfacePathInRegistry, CurrentDevice);
  end;
end;

function TWriteBufferSettingVerifier.CheckAndCorrectByInterface(
  InterfaceName: String): TStringList;
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
  TStaticRegistry.GetKeyList(BasePathWithInterfaceName, ModelList);
  
  for ModelAndFirmware in ModelList do
  begin
    ResultInModel :=
      CheckAndCorrectByModel(
        BasePathWithInterfaceName, ModelAndFirmware);
    if ResultInModel <> '' then
      result.AddStrings(ResultInModel);
    FreeAndNil(ResultInModel);
  end;
end;

function TWriteBufferSettingVerifier.CheckAndCorrect: TStringList;
const
  InterfaceArray = ['IDE', 'SCSI'];
var
  CurrentInterfaceName: String;
  ResultInSpecificInterface: TStringList;
begin
  result := TStringList.Create;
  
  ModelList := TStringList.Create;
  DeviceList := TStringList.Create;
  ValueList := TStringList.Create;
  
  for CurrentInterfaceName in InterfaceArray do
  begin
    ResultInSpecificInterface :=
      CheckAndCorrectByInterface(CurrentInterfaceName);
    result.AddStrings(ResultInSpecificInterface);
    FreeAndNil(ResultInSpecificInterface);
  end;
  
  FreeAndNil(ModelList);
  FreeAndNil(DeviceList);
  FreeAndNil(ValueList);
end;

end.