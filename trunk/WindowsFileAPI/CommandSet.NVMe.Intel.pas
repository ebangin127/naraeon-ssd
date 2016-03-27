unit CommandSet.NVMe.Intel;

interface

uses
  Windows, SysUtils,
  CommandSet, BufferInterpreter, Device.SMART.List,
  CommandSet.NVMe, CommandSet.NVMe.Intel.PortPart,
  Getter.SCSIAddress, Device.SlotSpeed, OS.SetupAPI;

type
  TIntelNVMeCommandSet = class sealed(TNVMeCommandSet)
  private
    SCSIPath: String;
    function GetPortIdentifyDevice: TIdentifyDeviceResult;
    procedure SetSCSIPathIfNeeded;
  public
    function IdentifyDevice: TIdentifyDeviceResult; override;
    function SMARTReadData: TSMARTValueList; override;
    function RAWIdentifyDevice: String; override;
    function RAWSMARTReadData: String; override;
  end;

implementation

{ TIntelNVMeCommandSet }

const
  SCSIPrefix = '\\.\SCSI';
  SCSIPostfix = ':';

function TIntelNVMeCommandSet.IdentifyDevice: TIdentifyDeviceResult;
var
  ReadCapacityResult: TIdentifyDeviceResult;
  SlotSpeed: TSlotMaxCurrSpeed;
begin
  result := GetPortIdentifyDevice;
  if result.Model = '' then
    exit;
  SetBufferAndReadCapacity;
  ReadCapacityResult := InterpretReadCapacityBuffer;
  result.UserSizeInKB := ReadCapacityResult.UserSizeInKB;
  result.LBASize := ReadCapacityResult.LBASize;
  result.IsDataSetManagementSupported := IsDataSetManagementSupported;
  try
    SlotSpeed := GetSlotSpeed;
    result.SlotSpeed := SlotSpeed.Current;
  except
    on EBelowVistaException do
      FillChar(result.SlotSpeed, SizeOf(result.SlotSpeed), #0);
    else raise;
  end;
end;

function TIntelNVMeCommandSet.RAWIdentifyDevice: String;
var
  PortCommandSet: TIntelNVMePortCommandSet;
begin
  SetSCSIPathIfNeeded;
  PortCommandSet := TIntelNVMePortCommandSet.Create(SCSIPath);
  try
    result := PortCommandSet.RAWIdentifyDevice;
  finally
    FreeAndNil(PortCommandSet);
  end;
end;

function TIntelNVMeCommandSet.RAWSMARTReadData: String;
var
  PortCommandSet: TIntelNVMePortCommandSet;
begin
  SetSCSIPathIfNeeded;
  PortCommandSet := TIntelNVMePortCommandSet.Create(SCSIPath);
  try
    result := PortCommandSet.RAWSMARTReadData;
  finally
    FreeAndNil(PortCommandSet);
  end;
end;

function TIntelNVMeCommandSet.GetPortIdentifyDevice: TIdentifyDeviceResult;
var
  PortCommandSet: TIntelNVMePortCommandSet;
begin
  result.Model := '';
  SetSCSIPathIfNeeded;
  PortCommandSet := TIntelNVMePortCommandSet.Create(SCSIPath);
  try
    result := PortCommandSet.IdentifyDevice;
  finally
    FreeAndNil(PortCommandSet);
  end;
end;

procedure TIntelNVMeCommandSet.SetSCSIPathIfNeeded;
var
  SCSIAddressGetter: TSCSIAddressGetter;
begin
  if SCSIPath <> '' then
    exit;
  SCSIAddressGetter := TSCSIAddressGetter.Create(GetPathOfFileAccessing);
  try
    SCSIPath :=
      SCSIPrefix + IntToStr(SCSIAddressGetter.GetSCSIAddress.PortNumber) +
      SCSIPostFix;
  finally
    FreeAndNil(SCSIAddressGetter);
  end;
end;

function TIntelNVMeCommandSet.SMARTReadData: TSMARTValueList;
var
  PortCommandSet: TIntelNVMePortCommandSet;
begin
  SetSCSIPathIfNeeded;
  PortCommandSet := TIntelNVMePortCommandSet.Create(SCSIPath);
  try
    result := PortCommandSet.SMARTReadData;
  finally
    FreeAndNil(PortCommandSet);
  end;
end;

end.
