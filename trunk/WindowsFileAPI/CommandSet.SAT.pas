unit CommandSet.SAT;

interface

uses
  Windows, SysUtils,
  OSFile.IoControl, CommandSet, BufferInterpreter, Device.SMART.List,
  BufferInterpreter.ATA;

type
  TSATCommandSet = class sealed(TCommandSet)
  public
    function IdentifyDevice: TIdentifyDeviceResult; override;
    function SMARTReadData: TSMARTValueList; override;
    function DataSetManagement(StartLBA, LBACount: Int64): Cardinal; override;
    function IsDataSetManagementSupported: Boolean; override;
    function IsExternal: Boolean; override;
  private
    type
      SCSI_COMMAND_DESCRIPTOR_BLOCK = record
        SCSICommand: UCHAR;
        MultiplecountProtocolExtend: UCHAR;
        OfflineCkcondDirectionByteblockLength: UCHAR;
        Features: UCHAR;
        SectorCount: UCHAR;
        LBALo: UCHAR;
        LBAMid: UCHAR;
        LBAHi: UCHAR;
        DeviceHead: UCHAR;
        ATACommand: UCHAR;
        Reserved: UCHAR;
        Control: UCHAR;
        ReservedToFill16B: Array[0..3] of Byte;
      end;
      SCSI_COMMAND_DESCRIPTOR_BLOCK_16 = record
        SCSICommand: UCHAR;
        MultiplecountProtocolExtend: UCHAR;
        OfflineCkcondDirectionByteblockLength: UCHAR;
        FeaturesHi: UCHAR;
        FeaturesLow: UCHAR;
        SectorCountHi: UCHAR;
        SectorCountLo: UCHAR;
        LBALoHi: UCHAR;
        LBALoLo: UCHAR;
        LBAMidHi: UCHAR;
        LBAMidLo: UCHAR;
        LBAHiHi: UCHAR;
        LBAHiLo: UCHAR;
        DeviceHead: UCHAR;
        ATACommand: UCHAR;
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
    IoOSBuffer: TIoControlIOBuffer;

    function GetCommonBuffer: SCSI_WITH_BUFFER;
    function GetCommonCommandDescriptorBlock: SCSI_COMMAND_DESCRIPTOR_BLOCK;
    procedure SetOSBufferByInnerBuffer;
    procedure SetInnerBufferAsFlagsAndCdb(Flags: ULONG;
      CommandDescriptorBlock: SCSI_COMMAND_DESCRIPTOR_BLOCK);
    procedure SetInnerBufferToSMARTReadData;
    procedure SetInnerBufferToIdentifyDevice;
    function InterpretIdentifyDeviceBuffer: TIdentifyDeviceResult;
    procedure SetBufferAndIdentifyDevice;
    function InterpretSMARTReadDataBuffer: TSMARTValueList;
    procedure SetBufferAndSMARTReadData;
    function InterpretSMARTThresholdBuffer(
      const OriginalResult: TSMARTValueList): TSMARTValueList;
    procedure SetBufferAndSMARTReadThreshold;
    procedure SetInnerBufferToSMARTReadThreshold;
    procedure SetDataSetManagementBuffer(StartLBA, LBACount: Int64);
    procedure SetInnerBufferToDataSetManagement(StartLBA, LBACount: Int64);
    procedure SetLBACountToDataSetManagementBuffer(LBACount: Int64);
    procedure SetStartLBAToDataSetManagementBuffer(StartLBA: Int64);
    function GetCommonCommandDescriptorBlock16:
      SCSI_COMMAND_DESCRIPTOR_BLOCK_16;
    procedure SetInnerBufferAsFlagsAndCdb16(Flags: ULONG;
      CommandDescriptorBlock: SCSI_COMMAND_DESCRIPTOR_BLOCK_16);
  end;

implementation

{ TSATCommandSet }

function TSATCommandSet.GetCommonBuffer: SCSI_WITH_BUFFER;
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

function TSATCommandSet.GetCommonCommandDescriptorBlock:
  SCSI_COMMAND_DESCRIPTOR_BLOCK;
const
  ATAPassThrough12Command = $A1;

  SAT_PROTOCOL_DATA_IN = 1 shl 3;
  SAT_PROTOCOL_DATA_OUT = 1 shl 4;
  SAT_PROTOCOL_USE_DMA = 1 shl 5;

  SAT_BYTEBLOCK_BYTE = 0;
  SAT_BYTEBLOCK_BLOCK = 1 shl 2;

  SAT_LENGTH_NO_DATA = 0;
  SAT_LENGTH_AT_FEATURES = 1;
  SAT_LENGTH_AT_SECTORCOUNT = 2;
  SAT_LENGTH_AT_TPSIU = 3;

  SAT_DIR_CLIENT_TO_DEVICE = 0;
  SAT_DIR_DEVICE_TO_CLIENT = 1 shl 3;
begin
  FillChar(result, SizeOf(result), #0);
  result.SCSICommand := ATAPassThrough12Command;
  result.MultiplecountProtocolExtend := SAT_PROTOCOL_DATA_IN;
  result.OfflineCkcondDirectionByteblockLength :=
    SAT_LENGTH_AT_SECTORCOUNT or SAT_BYTEBLOCK_BLOCK or
    SAT_DIR_DEVICE_TO_CLIENT;
end;

function TSATCommandSet.GetCommonCommandDescriptorBlock16:
  SCSI_COMMAND_DESCRIPTOR_BLOCK_16;
const
  ATAPassThrough16Command = $85;

  SCSI_IOCTL_DATA_48BIT = 1;
  SAT_PROTOCOL_DATA_IN = 4 shl 1;
  SAT_PROTOCOL_DATA_OUT = 5 shl 1;
  SAT_PROTOCOL_USE_DMA = 6 shl 1;

  SAT_BYTEBLOCK_BYTE = 0;
  SAT_BYTEBLOCK_BLOCK = 1 shl 2;

  SAT_LENGTH_NO_DATA = 0;
  SAT_LENGTH_AT_FEATURES = 1;
  SAT_LENGTH_AT_SECTORCOUNT = 2;
  SAT_LENGTH_AT_TPSIU = 3;

  SAT_DIR_CLIENT_TO_DEVICE = 0;
  SAT_DIR_DEVICE_TO_CLIENT = 1 shl 3;
begin
  FillChar(result, SizeOf(result), #0);
  result.SCSICommand := ATAPassThrough16Command;
  result.MultiplecountProtocolExtend :=
    SCSI_IOCTL_DATA_48BIT or
    SAT_PROTOCOL_USE_DMA;
  result.OfflineCkcondDirectionByteblockLength :=
    SAT_LENGTH_AT_SECTORCOUNT or SAT_BYTEBLOCK_BLOCK or
    SAT_DIR_CLIENT_TO_DEVICE;
end;

procedure TSATCommandSet.SetInnerBufferAsFlagsAndCdb
  (Flags: ULONG; CommandDescriptorBlock: SCSI_COMMAND_DESCRIPTOR_BLOCK);
begin
  CommandDescriptorBlock.SectorCount := 1;
  IoInnerBuffer := GetCommonBuffer;
	IoInnerBuffer.Parameter.DataIn := Flags;
  IoInnerBuffer.Parameter.CommandDescriptorBlock := CommandDescriptorBlock;
end;

procedure TSATCommandSet.SetInnerBufferAsFlagsAndCdb16
  (Flags: ULONG; CommandDescriptorBlock: SCSI_COMMAND_DESCRIPTOR_BLOCK_16);
begin
  Assert(SizeOf(CommandDescriptorBlock) =
    SizeOf(SCSI_COMMAND_DESCRIPTOR_BLOCK));
  IoInnerBuffer := GetCommonBuffer;
	IoInnerBuffer.Parameter.DataIn := Flags;
  Move(CommandDescriptorBlock,
    IoInnerBuffer.Parameter.CommandDescriptorBlock,
    SizeOf(SCSI_COMMAND_DESCRIPTOR_BLOCK));
end;

procedure TSATCommandSet.SetInnerBufferToIdentifyDevice;
const
  IdentifyDeviceCommand = $EC;
var
  CommandDescriptorBlock: SCSI_COMMAND_DESCRIPTOR_BLOCK;
begin
  CommandDescriptorBlock := GetCommonCommandDescriptorBlock;
  CommandDescriptorBlock.ATACommand := IdentifyDeviceCommand;
  CommandDescriptorBlock.SectorCount := 1;
  SetInnerBufferAsFlagsAndCdb(SCSI_IOCTL_DATA_IN, CommandDescriptorBlock);
end;

procedure TSATCommandSet.SetOSBufferByInnerBuffer;
begin
  IoOSBuffer.InputBuffer.Size := SizeOf(IoInnerBuffer);
  IoOSBuffer.InputBuffer.Buffer := @IOInnerBuffer;

  IoOSBuffer.OutputBuffer.Size := SizeOf(IoInnerBuffer);
  IoOSBuffer.OutputBuffer.Buffer := @IOInnerBuffer;
end;

procedure TSATCommandSet.SetBufferAndIdentifyDevice;
begin
  SetInnerBufferToIdentifyDevice;
  SetOSBufferByInnerBuffer;
  IoControl(TIoControlCode.SCSIPassThrough, IoOSBuffer);
end;

function TSATCommandSet.InterpretIdentifyDeviceBuffer:
  TIdentifyDeviceResult;
var
  ATABufferInterpreter: TATABufferInterpreter;
begin
  ATABufferInterpreter := TATABufferInterpreter.Create;
  result :=
    ATABufferInterpreter.BufferToIdentifyDeviceResult(IoInnerBuffer.Buffer);
  FreeAndNil(ATABufferInterpreter);
end;

function TSATCommandSet.IdentifyDevice: TIdentifyDeviceResult;
begin
  SetBufferAndIdentifyDevice;
  result := InterpretIdentifyDeviceBuffer;
  result.StorageInterface := TStorageInterface.SAT;
  result.IsDataSetManagementSupported := IsDataSetManagementSupported;
end;

procedure TSATCommandSet.SetInnerBufferToSMARTReadData;
const
  SMARTFeatures = $D0;
  SMARTCycleLo = $4F;
  SMARTCycleHi = $C2;
  SMARTReadDataCommand = $B0;
var
  IoTaskFile: SCSI_COMMAND_DESCRIPTOR_BLOCK;
begin
  IoTaskFile := GetCommonCommandDescriptorBlock;
  IoTaskFile.Features := SMARTFeatures;
  IoTaskFile.LBAMid := SMARTCycleLo;
  IoTaskFile.LBAHi := SMARTCycleHi;
  IoTaskFile.ATACommand := SMARTReadDataCommand;
  SetInnerBufferAsFlagsAndCdb(SCSI_IOCTL_DATA_IN, IoTaskFile);
end;

procedure TSATCommandSet.SetBufferAndSMARTReadData;
begin
  SetInnerBufferToSMARTReadData;
  SetOSBufferByInnerBuffer;
  IoControl(TIoControlCode.SCSIPassThrough, IoOSBuffer);
end;

function TSATCommandSet.InterpretSMARTReadDataBuffer:
  TSMARTValueList;
var
  ATABufferInterpreter: TATABufferInterpreter;
begin
  ATABufferInterpreter := TATABufferInterpreter.Create;
  result := ATABufferInterpreter.BufferToSMARTValueList(IoInnerBuffer.Buffer);
  FreeAndNil(ATABufferInterpreter);
end;

procedure TSATCommandSet.SetInnerBufferToSMARTReadThreshold;
const
  SMARTFeatures = $D1;
  SMARTCycleLo = $4F;
  SMARTCycleHi = $C2;
  SMARTReadDataCommand = $B0;
var
  IoTaskFile: SCSI_COMMAND_DESCRIPTOR_BLOCK;
begin
  IoTaskFile := GetCommonCommandDescriptorBlock;
  IoTaskFile.Features := SMARTFeatures;
  IoTaskFile.LBAMid := SMARTCycleLo;
  IoTaskFile.LBAHi := SMARTCycleHi;
  IoTaskFile.ATACommand := SMARTReadDataCommand;
  SetInnerBufferAsFlagsAndCdb(SCSI_IOCTL_DATA_IN, IoTaskFile);
end;

procedure TSATCommandSet.SetBufferAndSMARTReadThreshold;
begin
  SetInnerBufferToSMARTReadThreshold;
  SetOSBufferByInnerBuffer;
  IoControl(TIoControlCode.SCSIPassThrough, IoOSBuffer);
end;

function TSATCommandSet.InterpretSMARTThresholdBuffer(
  const OriginalResult: TSMARTValueList): TSMARTValueList;
var
  ATABufferInterpreter: TATABufferInterpreter;
  ThresholdList: TSMARTValueList;
  SmallBuffer: TSmallBuffer;
begin
  result := OriginalResult;
  ATABufferInterpreter := TATABufferInterpreter.Create;
  Move(IoInnerBuffer.Buffer, SmallBuffer, SizeOf(SmallBuffer));
  ThresholdList := ATABufferInterpreter.BufferToSMARTThresholdValueList(
    SmallBuffer);
  try
    OriginalResult.MergeThreshold(ThresholdList);
  finally
    FreeAndNil(ThresholdList);
  end;
  FreeAndNil(ATABufferInterpreter);
end;

function TSATCommandSet.SMARTReadData: TSMARTValueList;
begin
  SetBufferAndSMARTReadData;
  result := InterpretSMARTReadDataBuffer;
  SetBufferAndSMARTReadThreshold;
  result := InterpretSMARTThresholdBuffer(result);
end;

function TSATCommandSet.IsDataSetManagementSupported: Boolean;
begin
  exit(true);
end;

function TSATCommandSet.IsExternal: Boolean;
begin
  result := true;
end;

procedure TSATCommandSet.SetStartLBAToDataSetManagementBuffer(StartLBA: Int64);
const
  StartLBALo = 0;
begin
  IoInnerBuffer.Buffer[StartLBALo] := StartLBA and 255;
  StartLBA := StartLBA shr 8;
  IoInnerBuffer.Buffer[StartLBALo + 1] := StartLBA and 255;
  StartLBA := StartLBA shr 8;
  IoInnerBuffer.Buffer[StartLBALo + 2] := StartLBA and 255;
  StartLBA := StartLBA shr 8;
  IoInnerBuffer.Buffer[StartLBALo + 3] := StartLBA and 255;
  StartLBA := StartLBA shr 8;
  IoInnerBuffer.Buffer[StartLBALo + 4] := StartLBA and 255;
  StartLBA := StartLBA shr 8;
  IoInnerBuffer.Buffer[StartLBALo + 5] := StartLBA;
end;

procedure TSATCommandSet.SetLBACountToDataSetManagementBuffer(LBACount: Int64);
const
  LBACountHi = 7;
  LBACountLo = 6;
begin
  IoInnerBuffer.Buffer[LBACountLo] := LBACount and 255;
  IoInnerBuffer.Buffer[LBACountHi] := LBACount shr 8;
end;

procedure TSATCommandSet.SetDataSetManagementBuffer(
  StartLBA, LBACount: Int64);
begin
  SetStartLBAToDataSetManagementBuffer(StartLBA);
  SetLBACountToDataSetManagementBuffer(LBACount);
end;

procedure TSATCommandSet.SetInnerBufferToDataSetManagement
  (StartLBA, LBACount: Int64);
const
  DataSetManagementFeatures = $1;
  DataSetManagementSectorCount = $1;
  DataSetManagementCommand = $6;
var
  CommandDescriptorBlock: SCSI_COMMAND_DESCRIPTOR_BLOCK_16;
begin
  CommandDescriptorBlock := GetCommonCommandDescriptorBlock16;
  CommandDescriptorBlock.FeaturesLow := DataSetManagementFeatures;
  CommandDescriptorBlock.SectorCountLo := DataSetManagementSectorCount;
  CommandDescriptorBlock.ATACommand := DataSetManagementCommand;
  SetInnerBufferAsFlagsAndCdb16(SCSI_IOCTL_DATA_OUT, CommandDescriptorBlock);
  SetDataSetManagementBuffer(StartLBA, LBACount);
end;

function TSATCommandSet.DataSetManagement(StartLBA, LBACount: Int64):
  Cardinal;
begin
  SetInnerBufferToDataSetManagement(StartLBA, LBACount);
  result := ExceptionFreeIoControl(TIoControlCode.SCSIPassThrough,
    BuildOSBufferBy<SCSI_WITH_BUFFER, SCSI_WITH_BUFFER>(IoInnerBuffer,
      IoInnerBuffer));
end;

end.
