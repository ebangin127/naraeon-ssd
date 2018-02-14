unit CommandSet.NVMe.WithoutDriver;

interface

uses
  Windows, SysUtils,
  OSFile.IoControl, CommandSet.NVMe, BufferInterpreter, Device.SMART.List,
  BufferInterpreter.SCSI, OS.SetupAPI;

type
  TNVMeWithoutDriverCommandSet = class sealed(TNVMeCommandSet)
  public
    function IdentifyDevice: TIdentifyDeviceResult; override;
    function SMARTReadData: TSMARTValueList; override;
    function RAWIdentifyDevice: String; override;
    function RAWSMARTReadData: String; override;
    function IsExternal: Boolean; override;
    procedure Flush; override;
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
    procedure IfRealSCSIDeleteModel(var Model: String);
    procedure SetInnerBufferAsFlagsAndCdb(Flags: ULONG;
      CommandDescriptorBlock: SCSI_COMMAND_DESCRIPTOR_BLOCK);
    function InterpretIdentifyDeviceBuffer: TIdentifyDeviceResult;
    procedure SetBufferAndIdentifyDevice;
    procedure SetInnerBufferToSendIdentifyDeviceCommand;
  end;

implementation

{ TNVMeWithoutDriverCommandSet }

function TNVMeWithoutDriverCommandSet.GetCommonBuffer: SCSI_WITH_BUFFER;
const
  SATTargetId = 1;
begin
  FillChar(result, SizeOf(result), #0);
	result.Parameter.Length :=
    SizeOf(result.Parameter);
  result.Parameter.TargetId := SATTargetId;
  result.Parameter.CdbLength := 8;
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

function TNVMeWithoutDriverCommandSet.GetCommonCommandDescriptorBlock:
  SCSI_COMMAND_DESCRIPTOR_BLOCK;
begin
  FillChar(result, SizeOf(result), #0);
end;

procedure TNVMeWithoutDriverCommandSet.SetInnerBufferAsFlagsAndCdb
  (Flags: ULONG; CommandDescriptorBlock: SCSI_COMMAND_DESCRIPTOR_BLOCK);
begin
  IoInnerBuffer := GetCommonBuffer;
  IoInnerBuffer.Parameter.DataIn := Flags;
  IoInnerBuffer.Parameter.CommandDescriptorBlock := CommandDescriptorBlock;
end;

procedure TNVMeWithoutDriverCommandSet.SetBufferAndIdentifyDevice;
begin
  SetInnerBufferToSendIdentifyDeviceCommand;
  IoControl(TIoControlCode.SCSIPassThrough,
    BuildOSBufferBy<SCSI_WITH_BUFFER, SCSI_WITH_BUFFER>(IoInnerBuffer,
      IoInnerBuffer));
end;

procedure TNVMeWithoutDriverCommandSet.
  SetInnerBufferToSendIdentifyDeviceCommand;
const
  InquiryCommand = $12;
var
  CommandDescriptorBlock: SCSI_COMMAND_DESCRIPTOR_BLOCK;
begin
  CommandDescriptorBlock := GetCommonCommandDescriptorBlock;
  CommandDescriptorBlock.SCSICommand := InquiryCommand;
  CommandDescriptorBlock.LogicalBlockAddress[1] :=
    SizeOf(IoInnerBuffer.Buffer) shr 8;
  CommandDescriptorBlock.LogicalBlockAddress[2] :=
    SizeOf(IoInnerBuffer.Buffer) and $FF;
  SetInnerBufferAsFlagsAndCdb(SCSI_IOCTL_DATA_IN, CommandDescriptorBlock);
end;

procedure TNVMeWithoutDriverCommandSet.IfRealSCSIDeleteModel(
  var Model: String);
begin
  if Copy(Model, 1, 4) <> 'NVMe' then
    Model := '';
end;

function TNVMeWithoutDriverCommandSet.IdentifyDevice: TIdentifyDeviceResult;
var
  ReadCapacityResult: TIdentifyDeviceResult;
begin
  SetBufferAndIdentifyDevice;
  result := InterpretIdentifyDeviceBuffer;
  IfRealSCSIDeleteModel(result.Model);
  result.StorageInterface := TStorageInterface.NVMe;
  SetBufferAndReadCapacity;
  ReadCapacityResult := InterpretReadCapacityBuffer;
  result.UserSizeInKB := ReadCapacityResult.UserSizeInKB;
  result.LBASize := ReadCapacityResult.LBASize;
  result.IsDataSetManagementSupported := false;
end;

function TNVMeWithoutDriverCommandSet.InterpretIdentifyDeviceBuffer:
  TIdentifyDeviceResult;
var
  SCSIBufferInterpreter: TSCSIBufferInterpreter;
begin
  SCSIBufferInterpreter := TSCSIBufferInterpreter.Create;
  result :=
    SCSIBufferInterpreter.BufferToIdentifyDeviceResult(IoInnerBuffer.Buffer);
  FreeAndNil(SCSIBufferInterpreter);
end;

function TNVMeWithoutDriverCommandSet.IsExternal: Boolean;
begin
  result := false;
end;

function TNVMeWithoutDriverCommandSet.RAWIdentifyDevice: String;
begin
  raise ENotSupportedException.Create('Please install the driver');
end;

function TNVMeWithoutDriverCommandSet.RAWSMARTReadData: String;
begin
  raise ENotSupportedException.Create('Please install the driver');
end;

function TNVMeWithoutDriverCommandSet.SMARTReadData: TSMARTValueList;
begin
  raise ENotSupportedException.Create('Please install the driver');
end;

procedure TNVMeWithoutDriverCommandSet.Flush;
begin
  raise ENotSupportedException.Create('Please install the driver');
end;


end.
