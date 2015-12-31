unit CommandSet.NVMe.Intel;

interface

uses
  Windows, SysUtils,
  OSFile.IoControl, CommandSet, BufferInterpreter, Device.SMART.List,
  CommandSet.NVMe, CommandSet.NVMe.Intel.PortPart,
  Getter.SCSIAddress;

type
  TIntelNVMeCommandSet = class sealed(TNVMeCommandSet)
  private
    function GetPortIdentifyDevice: TIdentifyDeviceResult;
    function SendPortIdentifyDevice(
      const SCSIAddressGetter: TSCSIAddressGetter): TIdentifyDeviceResult;
  public
    function IdentifyDevice: TIdentifyDeviceResult; override;
    function SMARTReadData: TSMARTValueList; override;
  end;

implementation

{ TIntelNVMeCommandSet }

const
  SCSIPrefix = '\\.\SCSI';
  SCSIPostfix = ':';

function TIntelNVMeCommandSet.IdentifyDevice: TIdentifyDeviceResult;
var
  ReadCapacityResult: TIdentifyDeviceResult;
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
    result.SlotSpeed := GetSlotSpeed.Current;
  except
    FillChar(result.SlotSpeed, SizeOf(result.SlotSpeed), #0);
  end;
end;

function TIntelNVMeCommandSet.GetPortIdentifyDevice: TIdentifyDeviceResult;
var
  SCSIAddressGetter: TSCSIAddressGetter;
begin
  SCSIAddressGetter := TSCSIAddressGetter.Create(
    GetPathOfFileAccessing);
  try
    result := SendPortIdentifyDevice(SCSIAddressGetter);
  finally
    FreeAndNil(SCSIAddressGetter);
  end;
end;

function TIntelNVMeCommandSet.SMARTReadData: TSMARTValueList;
var
  PortCommandSet: TIntelNVMePortCommandSet;
  SCSIAddressGetter: TSCSIAddressGetter;
begin
  SCSIAddressGetter := TSCSIAddressGetter.Create(
    GetPathOfFileAccessing);
  PortCommandSet := TIntelNVMePortCommandSet.Create(
    SCSIPrefix + IntToStr(SCSIAddressGetter.GetSCSIAddress.PortNumber) +
    SCSIPostFix);
  result := PortCommandSet.SMARTReadData;
  FreeAndNil(PortCommandSet);
  FreeAndNil(SCSIAddressGetter);
end;

function TIntelNVMeCommandSet.SendPortIdentifyDevice(
  const SCSIAddressGetter: TSCSIAddressGetter): TIdentifyDeviceResult;
var
  PortCommandSet: TIntelNVMePortCommandSet;
begin
  result.Model := '';
  PortCommandSet := TIntelNVMePortCommandSet.Create(
    SCSIPrefix + IntToStr(SCSIAddressGetter.GetSCSIAddress.PortNumber) +
    SCSIPostFix);
  try
    result := PortCommandSet.IdentifyDevice;
  finally
    FreeAndNil(PortCommandSet);
  end;
end;

end.
