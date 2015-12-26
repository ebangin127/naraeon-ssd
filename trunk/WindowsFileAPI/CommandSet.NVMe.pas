unit CommandSet.NVMe;

interface

uses
  Windows, SysUtils,
  OSFile.IoControl, CommandSet, BufferInterpreter, BufferInterpreter.NVMe,
  Device.SMART.List, Device.SlotSpeed, Getter.SlotSpeed;

type
  TNVMeCommandSet = class abstract(TCommandSet)
  public
    function DataSetManagement(StartLBA, LBACount: Int64): Cardinal;
      override;
    function IsDataSetManagementSupported: Boolean; override;
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
  protected
    procedure SetBufferAndReadCapacity;
    function InterpretReadCapacityBuffer: TIdentifyDeviceResult;
    function GetSlotSpeed: TSlotMaxCurrSpeed;
  private
    IoInnerBuffer: SCSI_WITH_BUFFER;
    function GetCommonBuffer: SCSI_WITH_BUFFER;
    function GetCommonCommandDescriptorBlock: SCSI_COMMAND_DESCRIPTOR_BLOCK;
    procedure SetInnerBufferAsFlagsAndCdb(Flags: ULONG;
      CommandDescriptorBlock: SCSI_COMMAND_DESCRIPTOR_BLOCK);
    procedure SetInnerBufferToReadCapacity;
    procedure SetUnmapHeader;
    procedure SetUnmapLBA(StartLBA: Int64);
    procedure SetUnmapLBACount(LBACount: Int64);
    procedure SetInnerBufferToUnmap(const StartLBA, LBACount: Int64);
  end;

implementation

{ TNVMeCommandSet }

function TNVMeCommandSet.GetCommonBuffer: SCSI_WITH_BUFFER;
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

function TNVMeCommandSet.GetSlotSpeed: TSlotMaxCurrSpeed;
{$IFNDEF SERVICE}
var
  SlotSpeedGetter: TSlotSpeedGetter;
{$ENDIF}
begin
  {$IFNDEF SERVICE}
  SlotSpeedGetter := TSlotSpeedGetter.Create(GetPathOfFileAccessing);
  result := SlotSpeedGetter.GetSlotSpeed;
  FreeAndNil(SlotSpeedGetter);
  {$ENDIF}
end;

function TNVMeCommandSet.GetCommonCommandDescriptorBlock:
  SCSI_COMMAND_DESCRIPTOR_BLOCK;
begin
  FillChar(result, SizeOf(result), #0);
end;

procedure TNVMeCommandSet.SetInnerBufferAsFlagsAndCdb
  (Flags: ULONG; CommandDescriptorBlock: SCSI_COMMAND_DESCRIPTOR_BLOCK);
begin
  IoInnerBuffer := GetCommonBuffer;
  IoInnerBuffer.Parameter.DataIn := Flags;
  IoInnerBuffer.Parameter.CommandDescriptorBlock := CommandDescriptorBlock;
end;

procedure TNVMeCommandSet.SetUnmapHeader;
const
  HeaderSize = 6;
  ContentSize = 16;
begin
  IoInnerBuffer.Buffer[1] := HeaderSize + ContentSize;
  IoInnerBuffer.Buffer[3] := ContentSize;
end;

procedure TNVMeCommandSet.SetBufferAndReadCapacity;
begin
  SetInnerBufferToReadCapacity;
  IoControl(TIoControlCode.SCSIPassThrough,
    BuildOSBufferBy<SCSI_WITH_BUFFER, SCSI_WITH_BUFFER>(IoInnerBuffer,
      IoInnerBuffer));
end;

procedure TNVMeCommandSet.SetInnerBufferToReadCapacity;
const
  ReadCapacityCommand = $9E;
  AllocationLowBit = 3;
var
  CommandDescriptorBlock: SCSI_COMMAND_DESCRIPTOR_BLOCK;
begin
  CommandDescriptorBlock := GetCommonCommandDescriptorBlock;
  CommandDescriptorBlock.SCSICommand := ReadCapacityCommand;
  CommandDescriptorBlock.MiscellaneousCDBInformation := $10;
  CommandDescriptorBlock.TransferParameterListAllocationLength[
    AllocationLowBit - 1] := (512 shr 8) and $FF;
  CommandDescriptorBlock.TransferParameterListAllocationLength[
    AllocationLowBit] := 512 and $FF;
  SetInnerBufferAsFlagsAndCdb(SCSI_IOCTL_DATA_IN, CommandDescriptorBlock);
end;

function TNVMeCommandSet.InterpretReadCapacityBuffer:
  TIdentifyDeviceResult;
var
  SCSIBufferInterpreter: TNVMeBufferInterpreter;
begin
  SCSIBufferInterpreter := TNVMeBufferInterpreter.Create;
  result :=
    SCSIBufferInterpreter.BufferToCapacityAndLBA(IoInnerBuffer.Buffer);
  FreeAndNil(SCSIBufferInterpreter);
end;

function TNVMeCommandSet.IsDataSetManagementSupported: Boolean;
begin
  exit(true);
end;

procedure TNVMeCommandSet.SetUnmapLBA(StartLBA: Int64);
const
  StartLBAHi = 10;
begin
  IoInnerBuffer.Buffer[StartLBAHi + 5] := StartLBA and 255;
  StartLBA := StartLBA shr 8;
  IoInnerBuffer.Buffer[StartLBAHi + 4] := StartLBA and 255;
  StartLBA := StartLBA shr 8;
  IoInnerBuffer.Buffer[StartLBAHi + 3] := StartLBA and 255;
  StartLBA := StartLBA shr 8;
  IoInnerBuffer.Buffer[StartLBAHi + 2] := StartLBA and 255;
  StartLBA := StartLBA shr 8;
  IoInnerBuffer.Buffer[StartLBAHi + 1] := StartLBA and 255;
  StartLBA := StartLBA shr 8;
  IoInnerBuffer.Buffer[StartLBAHi] := StartLBA;
end;

procedure TNVMeCommandSet.SetUnmapLBACount(LBACount: Int64);
const
  LBACountHi = 16;
begin
  IoInnerBuffer.Buffer[LBACountHi + 3] := LBACount and 255;
  LBACount := LBACount shr 8;
  IoInnerBuffer.Buffer[LBACountHi + 2] := LBACount and 255;
  LBACount := LBACount shr 8;
  IoInnerBuffer.Buffer[LBACountHi + 1] := LBACount and 255;
  LBACount := LBACount shr 8;
  IoInnerBuffer.Buffer[LBACountHi] := LBACount shr 8;
end;

procedure TNVMeCommandSet.SetInnerBufferToUnmap(const StartLBA, LBACount:
  Int64);
var
  CommandDescriptorBlock: SCSI_COMMAND_DESCRIPTOR_BLOCK;
const
  UnmapCommand = $42;
begin
  CommandDescriptorBlock := GetCommonCommandDescriptorBlock;
  CommandDescriptorBlock.SCSICommand := UnmapCommand;
  CommandDescriptorBlock.LogicalBlockAddress[6] := 24;
  SetInnerBufferAsFlagsAndCdb(SCSI_IOCTL_DATA_OUT, CommandDescriptorBlock);
  SetUnmapHeader;
  SetUnmapLBA(StartLBA);
  SetUnmapLBACount(LBACount);
end;

function TNVMeCommandSet.DataSetManagement(StartLBA, LBACount: Int64):
  Cardinal;
begin
  SetInnerBufferToUnmap(StartLBA, LBACount);
  result := ExceptionFreeIoControl(TIoControlCode.SCSIPassThrough,
    BuildOSBufferBy<SCSI_WITH_BUFFER, SCSI_WITH_BUFFER>(IoInnerBuffer,
      IoInnerBuffer));
end;

end.
