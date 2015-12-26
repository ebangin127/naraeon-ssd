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
  PortCommandSet: TIntelNVMePortCommandSet;
  SCSIAddressGetter: TSCSIAddressGetter;
begin
  SCSIAddressGetter := TSCSIAddressGetter.Create(
    GetPathOfFileAccessing);
  PortCommandSet := TIntelNVMePortCommandSet.Create(
    SCSIPrefix + IntToStr(SCSIAddressGetter.GetSCSIAddress.PortNumber) +
    SCSIPostFix);
  try
    result := PortCommandSet.IdentifyDevice;
  finally
    FreeAndNil(PortCommandSet);
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

end.
