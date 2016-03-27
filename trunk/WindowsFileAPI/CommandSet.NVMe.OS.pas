unit CommandSet.NVMe.OS;

interface

uses
  Windows, SysUtils,
  OSFile.IoControl, CommandSet.NVMe, BufferInterpreter, Device.SMART.List,
  BufferInterpreter.NVMe, OS.SetupAPI;

type
  TOSNVMeCommandSet = class sealed(TNVMeCommandSet)
  public
    function IdentifyDevice: TIdentifyDeviceResult; override;
    function SMARTReadData: TSMARTValueList; override;
    function RAWIdentifyDevice: String; override;
    function RAWSMARTReadData: String; override;
  private
    type
      TStoragePropertyId = (
        StorageDeviceProperty = 0,
        StorageAdapterProperty,
        StorageDeviceIdProperty,
        StorageDeviceUniqueIdProperty,
        StorageDeviceWriteCacheProperty,
        StorageMiniportProperty,
        StorageAccessAlignmentProperty,
        StorageDeviceSeekPenaltyProperty,
        StorageDeviceTrimProperty,
        StorageDeviceWriteAggregationProperty,
        StorageDeviceDeviceTelemetryProperty,
        StorageDeviceLBProvisioningProperty,
        StorageDevicePowerProperty,
        StorageDeviceCopyOffloadProperty,
        StorageDeviceResiliencyProperty,
        StorageDeviceMediumProductType,
        StorageDeviceRpmbProperty,
        StorageDeviceIoCapabilityProperty = 48,
        StorageAdapterProtocolSpecificProperty,
        StorageDeviceProtocolSpecificProperty,
        StorageAdapterTemperatureProperty,
        StorageDeviceTemperatureProperty,
        StorageAdapterPhysicalTopologyProperty,
        StorageDevicePhysicalTopologyProperty,
        StorageDeviceAttributesProperty);
      TStorageQueryType = (
        PropertyStandardQuery = 0,
        PropertyExistsQuery,
        PropertyMaskQuery,
        PropertyQueryMaxDefined);
      TStoragePropertyQuery = record
        PropertyId: DWORD;
        QueryType: DWORD;
      end;
      TStroageProtocolType = (
        ProtocolTypeUnknown = $00,
        ProtocolTypeScsi,
        ProtocolTypeAta,
        ProtocolTypeNvme,
        ProtocolTypeSd,
        ProtocolTypeProprietary = $7E,
        ProtocolTypeMaxReserved = $7F);
      TStorageProtocolSpecificData = record
        ProtocolType: TStroageProtocolType;
        DataType: DWORD;
        ProtocolDataRequestValue: DWORD;
        ProtocolDataRequestSubValue: DWORD;
        ProtocolDataOffset: DWORD;
        ProtocolDataLength: DWORD;
        FixedProtocolReturnData: DWORD;
        Reserved: Array[0..2] of DWORD;
      end;
      TStorageProtocolNVMeDataType = (
        NVMeDataTypeUnknown = 0,
        NVMeDataTypeIdentify,
        NVMeDataTypeLogPage,
        NVMeDataTypeFeature);
      TStorageQueryWithBuffer = record
        Query: TStoragePropertyQuery;
        ProtocolSpecific: TStorageProtocolSpecificData;
        Buffer: TLargeBuffer;
      end;
  private
    IoInnerBuffer: TStorageQueryWithBuffer;
    function GetCommonBuffer: TStorageQueryWithBuffer;
    function InterpretIdentifyDeviceBuffer: TIdentifyDeviceResult;
    procedure SetBufferAndIdentifyDevice;
    function InterpretSMARTBuffer: TSMARTValueList;
    procedure SetBufferAndSMART;
    procedure SetInnerBufferToSMARTCommand;
    procedure SetInnerBufferToIdentifyDeviceCommand;
  end;

implementation

{ TOSNVMeCommandSet }

function TOSNVMeCommandSet.GetCommonBuffer: TStorageQueryWithBuffer;
begin
  FillChar(result, SizeOf(result), #0);
	result.ProtocolSpecific.ProtocolType := ProtocolTypeNvme;
	result.ProtocolSpecific.DataType := DWORD(NVMeDataTypeIdentify);
	result.ProtocolSpecific.ProtocolDataOffset := sizeof(
    TStorageProtocolSpecificData);
	result.ProtocolSpecific.ProtocolDataLength := sizeof(TLargeBuffer);
	result.Query.PropertyId := DWORD(StorageAdapterProtocolSpecificProperty);
	result.Query.QueryType := DWORD(PropertyStandardQuery);
end;

procedure TOSNVMeCommandSet.SetBufferAndIdentifyDevice;
begin
  SetInnerBufferToIdentifyDeviceCommand;
  IoControl(TIoControlCode.StorageQueryProperty,
    BuildOSBufferBy<TStorageQueryWithBuffer, TStorageQueryWithBuffer>(
      IoInnerBuffer, IoInnerBuffer));
end;

procedure TOSNVMeCommandSet.SetInnerBufferToIdentifyDeviceCommand;
begin
  IoInnerBuffer := GetCommonBuffer;
  IoInnerBuffer.ProtocolSpecific.DataType := DWORD(NVMeDataTypeIdentify);
end;

function TOSNVMeCommandSet.InterpretIdentifyDeviceBuffer:
  TIdentifyDeviceResult;
var
  SCSIBufferInterpreter: TNVMeBufferInterpreter;
begin
  SCSIBufferInterpreter := TNVMeBufferInterpreter.Create;
  result :=
    SCSIBufferInterpreter.LargeBufferToIdentifyDeviceResult(
      IoInnerBuffer.Buffer);
  FreeAndNil(SCSIBufferInterpreter);
end;

function TOSNVMeCommandSet.InterpretSMARTBuffer: TSMARTValueList;
var
  SCSIBufferInterpreter: TNVMeBufferInterpreter;
begin
  SCSIBufferInterpreter := TNVMeBufferInterpreter.Create;
  result :=
    SCSIBufferInterpreter.LargeBufferToSMARTValueList(IoInnerBuffer.Buffer);
  FreeAndNil(SCSIBufferInterpreter);
end;

function TOSNVMeCommandSet.RAWIdentifyDevice: String;
begin
  SetBufferAndIdentifyDevice;
  result :=
    IdentifyDevicePrefix +
    TBufferInterpreter.BufferToString(IoInnerBuffer.Buffer) + ';';
end;

function TOSNVMeCommandSet.RAWSMARTReadData: String;
begin
  SetBufferAndSMART;
  result :=
    SMARTPrefix +
    TBufferInterpreter.BufferToString(IoInnerBuffer.Buffer) + ';';
end;

function TOSNVMeCommandSet.IdentifyDevice: TIdentifyDeviceResult;
var
  ReadCapacityResult: TIdentifyDeviceResult;
begin
  SetBufferAndIdentifyDevice;
  result := InterpretIdentifyDeviceBuffer;
  if result.Model = '' then
    exit;
  result.StorageInterface := TStorageInterface.NVMe;
  SetBufferAndReadCapacity;
  ReadCapacityResult := InterpretReadCapacityBuffer;
  result.UserSizeInKB := ReadCapacityResult.UserSizeInKB;
  result.LBASize := ReadCapacityResult.LBASize;
  result.IsDataSetManagementSupported := IsDataSetManagementSupported;
  try
    result.SlotSpeed := GetSlotSpeed.Current;
  except
    on EBelowVistaException do
      FillChar(result.SlotSpeed, SizeOf(result.SlotSpeed), #0);
    else raise;
  end;
end;

function TOSNVMeCommandSet.SMARTReadData: TSMARTValueList;
begin
  SetBufferAndSMART;
  result := InterpretSMARTBuffer;
end;

procedure TOSNVMeCommandSet.SetBufferAndSMART;
begin
  SetInnerBufferToSMARTCommand;
  IoControl(TIoControlCode.StorageQueryProperty,
    BuildOSBufferBy<TStorageQueryWithBuffer, TStorageQueryWithBuffer>(
      IoInnerBuffer, IoInnerBuffer));
end;

procedure TOSNVMeCommandSet.SetInnerBufferToSMARTCommand;
const
  SMARTHealthInformation = 2;
  GlobalLogPage = $FFFFFFFF;
begin
  IoInnerBuffer := GetCommonBuffer;
  IoInnerBuffer.ProtocolSpecific.DataType := DWORD(NVMeDataTypeLogPage);
  IoInnerBuffer.ProtocolSpecific.ProtocolDataRequestValue :=
    SMARTHealthInformation;
  IoInnerBuffer.ProtocolSpecific.ProtocolDataRequestSubValue :=
    GlobalLogPage;
end;

end.
