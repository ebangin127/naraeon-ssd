unit uWriteBufferSettingVerifier;

interface

uses
  Classes, SysUtils,
  uNSTSupport, uSandforceNSTSupport, uNSTSupportFactory;

type
  TWriteBufferSettingVerifier = class
  {$IfNDef UNITTEST}
  private
  {$EndIf}
    function SplitIntoModelAndFirmware(
      ModelAndFirmware: String; out Model, Firmware: String): Boolean;
  private
    function CheckAndCorrectByInterface(InterfaceName: String): TStringList;
    ModelList, DeviceList: TStringList;
  public
    function CheckAndCorrect: TStringList;
  end;

implementation

{ TWriteBufferSettingVerifier }

function TWriteBufferSettingVerifier.UnderbarToSpace(
  ModelAndFirmware: String): String;
var
  CurrentPoint: Integer;
begin
  result := ModelAndFirmware;
  for CurrentPoint := 0 to Length(result) - 1 do
    if result[CurrentPoint] = '_' then
      result[CurrentPoint] := ' ';
end;

function TWriteBufferSettingVerifier.SplitIntoModelAndFirmware(
  ModelAndFirmware: String; out Model, Firmware: String): Boolean;
const
  IdeDiskString = 'Disk';
  FirmwareLength = 8;
var
  Model, Firmware: String;
  ModelLength: Integer;
begin
  if Copy(ModelAndFirmware, 1, Length(IdeDiskString)) <> IdeDiskString then
    exit;
  
  ModelAndFirmware := UnderbarToSpace(ModelAndFirmware);
  ModelLength := Length(ModelAndFirmware) -
    (Length(DiskString) + FirmwareLength);
  Model := Copy(ModelAndFirmware, Length(DiskString) + 1, ModelLength);
  Firmware := Copy(ModelAndFirmware, 
    Length(ModelLength) + Length(DiskString) + 1,
    FirmwareLength);
end;

function TWriteBufferSettingVerifier.CheckSupportStatus(
  ModelAndFirmware: String): Boolean;
var
  NSTSupport: TNSTSupport;
  Model, Firmware: String;
begin
  NSTSupport := TNSTSupport.Create;
  SplitIntoModelAndFirmware(ModelAndFirmware, Model, Firmware);
  NSTSupport.SetModelAndFirmware(Model, Firmware);
  result :=
    (NSTSupport.GetSupportStatus.Supported) and
    (not (NSTSupport is TSandforceNSTSupport));
  FreeAndNil(NSTSupport);
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