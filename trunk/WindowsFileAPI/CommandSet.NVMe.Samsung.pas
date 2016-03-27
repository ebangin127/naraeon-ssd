unit CommandSet.NVMe.Samsung;

interface

uses
  Windows, SysUtils,
  OSFile.IoControl, CommandSet.NVMe, BufferInterpreter, Device.SMART.List,
  BufferInterpreter.NVMe, OS.SetupAPI;

type
  TSamsungNVMeCommandSet = class sealed(TNVMeCommandSet)
  public
    function IdentifyDevice: TIdentifyDeviceResult; override;
    function SMARTReadData: TSMARTValueList; override;
    function RAWIdentifyDevice: String; override;
    function RAWSMARTReadData: String; override;
  private
    type
      SCSI_COMMAND_DESCRIPTOR_BLOCK = record
        SCSICommand: UCHAR;
        MiscellaneousCDBInformation: UCHAR;
        LogicalBlockAddress: Array[0..7] of UCHAR;
        TransferParameterListAllocationLength: Array[0..3] of UCHAR;
        MiscellaneousCDBInformation2: UCHAR;
        Control: UCHAR;
      end;
      SCSI_PASS_THROUGH = record
        Length: USHORT;
        ScsiStatus: UCHAR;
        PathId: UCHAR;
        TargetId: UCHAR;
        Lun: UCHAR;
        CdbLength: UCHAR;
        SenseInfoLength: UCHAR;
        DataIn: UCHAR;
        DataTransferLength: ULONG;
        TimeOutValue: ULONG;
        DataBufferOffset: ULONG_PTR;
        SenseInfoOffset: ULONG;
        CommandDescriptorBlock: SCSI_COMMAND_DESCRIPTOR_BLOCK;
      end;
      SCSI_24B_SENSE_BUFFER = record
        ResponseCodeAndValidBit: UCHAR;
        Obsolete: UCHAR;
        SenseKeyILIEOMFilemark: UCHAR;
        Information: Array[0..3] of UCHAR;
        AdditionalSenseLengthMinusSeven: UCHAR;
        CommandSpecificInformation: Array[0..3] of UCHAR;
        AdditionalSenseCode: UCHAR;
        AdditionalSenseCodeQualifier: UCHAR;
        FieldReplaceableUnitCode: UCHAR;
        SenseKeySpecific: Array[0..2] of UCHAR;
        AdditionalSenseBytes: Array[0..5] of UCHAR;
      end;
      SCSI_WITH_BUFFER = record
        Parameter: SCSI_PASS_THROUGH;
        SenseBuffer: SCSI_24B_SENSE_BUFFER;
        Buffer: TSmallBuffer;
      end;
    const
      SCSI_IOCTL_DATA_OUT = 0;
      SCSI_IOCTL_DATA_IN = 1;
      SCSI_IOCTL_DATA_UNSPECIFIED = 2;
  private
    IoInnerBuffer: SCSI_WITH_BUFFER;
    function GetCommonBuffer: SCSI_WITH_BUFFER;
    function GetCommonCommandDescriptorBlock: SCSI_COMMAND_DESCRIPTOR_BLOCK;
    procedure SetInnerBufferAsFlagsAndCdb(Flags: ULONG;
      CommandDescriptorBlock: SCSI_COMMAND_DESCRIPTOR_BLOCK);
    function InterpretIdentifyDeviceBuffer: TIdentifyDeviceResult;
    procedure SetBufferAndIdentifyDevice;
    function InterpretSMARTBuffer: TSMARTValueList;
    procedure SetBufferAndSMART;
    procedure SetInnerBufferToReceiveSMARTCommand;
    procedure SetInnerBufferToSendSMARTCommand;
    procedure SetBufferAndSendSMARTCommand;
    procedure SetBufferAndReceiveSMARTCommand;
    procedure SetBufferForSMARTCommand;
    procedure SetBufferAndSendIdentifyDeviceCommand;
    procedure SetInnerBufferToSendIdentifyDeviceCommand;
    procedure SetBufferForIdentifyDeviceCommand;
    procedure SetBufferAndReceiveIdentifyDeviceCommand;
    procedure SetInnerBufferToReceiveIdentifyDeviceCommand;
  end;

implementation

{ TSamsungNVMeCommandSet }

function TSamsungNVMeCommandSet.GetCommonBuffer: SCSI_WITH_BUFFER;
const
  SATTargetId = 1;
begin
  FillChar(result, SizeOf(result), #0);
	result.Parameter.Length :=
    SizeOf(result.Parameter);
  result.Parameter.TargetId := SATTargetId;
  result.Parameter.CdbLength := SizeOf(result.Parameter.CommandDescriptorBlock);
	result.Parameter.SenseInfoLength :=
    SizeOf(result.SenseBuffer);
	result.Parameter.DataTransferLength :=
    SizeOf(result.Buffer);
	result.Parameter.TimeOutValue := 2;
	result.Parameter.DataBufferOffset :=
    PAnsiChar(@result.Buffer) - PAnsiChar(@result);
	result.Parameter.SenseInfoOffset :=
    PAnsiChar(@result.SenseBuffer) - PAnsiChar(@result);
end;

function TSamsungNVMeCommandSet.GetCommonCommandDescriptorBlock:
  SCSI_COMMAND_DESCRIPTOR_BLOCK;
begin
  FillChar(result, SizeOf(result), #0);
end;

procedure TSamsungNVMeCommandSet.SetInnerBufferAsFlagsAndCdb
  (Flags: ULONG; CommandDescriptorBlock: SCSI_COMMAND_DESCRIPTOR_BLOCK);
begin
  IoInnerBuffer := GetCommonBuffer;
  IoInnerBuffer.Parameter.DataIn := Flags;
  IoInnerBuffer.Parameter.CommandDescriptorBlock := CommandDescriptorBlock;
end;

procedure TSamsungNVMeCommandSet.SetBufferAndIdentifyDevice;
begin
  SetBufferAndSendIdentifyDeviceCommand;
  SetBufferAndReceiveIdentifyDeviceCommand;
end;

procedure TSamsungNVMeCommandSet.SetBufferAndSendIdentifyDeviceCommand;
begin
  SetInnerBufferToSendIdentifyDeviceCommand;
  IoControl(TIoControlCode.SCSIPassThrough,
    BuildOSBufferBy<SCSI_WITH_BUFFER, SCSI_WITH_BUFFER>(IoInnerBuffer,
      IoInnerBuffer));
end;

procedure TSamsungNVMeCommandSet.SetInnerBufferToSendIdentifyDeviceCommand;
const
  SecurityOutCommand = $B5;
  SamsungProtocol = $FE;
  Identify = 5;
  TransferLength = $40;
var
  CommandDescriptorBlock: SCSI_COMMAND_DESCRIPTOR_BLOCK;
begin
  CommandDescriptorBlock := GetCommonCommandDescriptorBlock;
  CommandDescriptorBlock.SCSICommand := SecurityOutCommand;
  CommandDescriptorBlock.MiscellaneousCDBInformation := SamsungProtocol;
  CommandDescriptorBlock.LogicalBlockAddress[1] := Identify;
  CommandDescriptorBlock.LogicalBlockAddress[7] := TransferLength;
  SetInnerBufferAsFlagsAndCdb(SCSI_IOCTL_DATA_OUT, CommandDescriptorBlock);
  SetBufferForIdentifyDeviceCommand;
end;

procedure TSamsungNVMeCommandSet.SetBufferForIdentifyDeviceCommand;
const
  ReturnToHost = 1;
begin
  IOInnerBuffer.Buffer[0] := ReturnToHost;
end;

procedure TSamsungNVMeCommandSet.SetBufferAndReceiveIdentifyDeviceCommand;
begin
  SetInnerBufferToReceiveIdentifyDeviceCommand;
  IoControl(TIoControlCode.SCSIPassThrough,
    BuildOSBufferBy<SCSI_WITH_BUFFER, SCSI_WITH_BUFFER>(IoInnerBuffer,
      IoInnerBuffer));
end;

procedure TSamsungNVMeCommandSet.SetInnerBufferToReceiveIdentifyDeviceCommand;
const
  SecurityInCommand = $A2;
  SamsungProtocol = $FE;
  Identify = 5;
  TransferLength = 1;
var
  CommandDescriptorBlock: SCSI_COMMAND_DESCRIPTOR_BLOCK;
begin
  CommandDescriptorBlock := GetCommonCommandDescriptorBlock;
  CommandDescriptorBlock.SCSICommand := SecurityInCommand;
  CommandDescriptorBlock.MiscellaneousCDBInformation := SamsungProtocol;
  CommandDescriptorBlock.LogicalBlockAddress[1] := Identify;
  CommandDescriptorBlock.LogicalBlockAddress[6] := TransferLength;
  SetInnerBufferAsFlagsAndCdb(SCSI_IOCTL_DATA_IN, CommandDescriptorBlock);
end;

function TSamsungNVMeCommandSet.InterpretIdentifyDeviceBuffer:
  TIdentifyDeviceResult;
var
  SCSIBufferInterpreter: TNVMeBufferInterpreter;
begin
  SCSIBufferInterpreter := TNVMeBufferInterpreter.Create;
  result :=
    SCSIBufferInterpreter.BufferToIdentifyDeviceResult(IoInnerBuffer.Buffer);
  FreeAndNil(SCSIBufferInterpreter);
end;

function TSamsungNVMeCommandSet.InterpretSMARTBuffer: TSMARTValueList;
var
  SCSIBufferInterpreter: TNVMeBufferInterpreter;
begin
  SCSIBufferInterpreter := TNVMeBufferInterpreter.Create;
  result :=
    SCSIBufferInterpreter.BufferToSMARTValueList(IoInnerBuffer.Buffer);
  FreeAndNil(SCSIBufferInterpreter);
end;

function TSamsungNVMeCommandSet.RAWIdentifyDevice: String;
begin
  SetBufferAndIdentifyDevice;
  result :=
    IdentifyDevicePrefix +
    TBufferInterpreter.BufferToString(IoInnerBuffer.Buffer) + ';';
end;

function TSamsungNVMeCommandSet.RAWSMARTReadData: String;
begin
  SetBufferAndSMART;
  result :=
    SMARTPrefix +
    TBufferInterpreter.BufferToString(IoInnerBuffer.Buffer) + ';';
end;

function TSamsungNVMeCommandSet.IdentifyDevice: TIdentifyDeviceResult;
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

function TSamsungNVMeCommandSet.SMARTReadData: TSMARTValueList;
begin
  SetBufferAndSMART;
  result := InterpretSMARTBuffer;
end;

procedure TSamsungNVMeCommandSet.SetBufferAndSMART;
begin
  SetBufferAndSendSMARTCommand;
  SetBufferAndReceiveSMARTCommand;
end;

procedure TSamsungNVMeCommandSet.SetBufferAndSendSMARTCommand;
begin
  SetInnerBufferToSendSMARTCommand;
  IoControl(TIoControlCode.SCSIPassThrough,
    BuildOSBufferBy<SCSI_WITH_BUFFER, SCSI_WITH_BUFFER>(IoInnerBuffer,
      IoInnerBuffer));
end;

procedure TSamsungNVMeCommandSet.SetInnerBufferToSendSMARTCommand;
const
  SecurityOutCommand = $B5;
  SamsungProtocol = $FE;
  GetLogPage = 6;
  TransferLength = $40;
var
  CommandDescriptorBlock: SCSI_COMMAND_DESCRIPTOR_BLOCK;
begin
  CommandDescriptorBlock := GetCommonCommandDescriptorBlock;
  CommandDescriptorBlock.SCSICommand := SecurityOutCommand;
  CommandDescriptorBlock.MiscellaneousCDBInformation := SamsungProtocol;
  CommandDescriptorBlock.LogicalBlockAddress[1] := GetLogPage;
  CommandDescriptorBlock.LogicalBlockAddress[7] := TransferLength;
  SetInnerBufferAsFlagsAndCdb(SCSI_IOCTL_DATA_OUT, CommandDescriptorBlock);
  SetBufferForSMARTCommand;
end;

procedure TSamsungNVMeCommandSet.SetBufferForSMARTCommand;
const
  SMARTHealthInformation = 2;
  GlobalLogPage = $FF;
begin
  IOInnerBuffer.Buffer[0] := SMARTHealthInformation;
  IOInnerBuffer.Buffer[4] := GlobalLogPage;
  IOInnerBuffer.Buffer[5] := GlobalLogPage;
  IOInnerBuffer.Buffer[6] := GlobalLogPage;
  IOInnerBuffer.Buffer[7] := GlobalLogPage;
end;

procedure TSamsungNVMeCommandSet.SetBufferAndReceiveSMARTCommand;
begin
  SetInnerBufferToReceiveSMARTCommand;
  IoControl(TIoControlCode.SCSIPassThrough,
    BuildOSBufferBy<SCSI_WITH_BUFFER, SCSI_WITH_BUFFER>(IoInnerBuffer,
      IoInnerBuffer));
end;

procedure TSamsungNVMeCommandSet.SetInnerBufferToReceiveSMARTCommand;
const
  SecurityInCommand = $A2;
  SamsungProtocol = $FE;
  AllPages = $08;
  GetLogPage = 6;
  TransferLength = 1;
var
  CommandDescriptorBlock: SCSI_COMMAND_DESCRIPTOR_BLOCK;
begin
  CommandDescriptorBlock := GetCommonCommandDescriptorBlock;
  CommandDescriptorBlock.SCSICommand := SecurityInCommand;
  CommandDescriptorBlock.MiscellaneousCDBInformation := SamsungProtocol;
  CommandDescriptorBlock.LogicalBlockAddress[1] := GetLogPage;
  CommandDescriptorBlock.LogicalBlockAddress[6] := TransferLength;
  SetInnerBufferAsFlagsAndCdb(SCSI_IOCTL_DATA_IN, CommandDescriptorBlock);
end;

end.
