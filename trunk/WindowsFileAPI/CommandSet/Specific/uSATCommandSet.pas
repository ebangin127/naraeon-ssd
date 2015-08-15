unit uSATCommandSet;

interface

uses
  Windows, SysUtils,
  uIoControlFile, uCommandSet, uBufferInterpreter, uSMARTValueList,
  uATABufferInterpreter;

type
  TSATCommandSet = class sealed(TCommandSet)
  public
    function IdentifyDevice: TIdentifyDeviceResult; override;
    function SMARTReadData: TSMARTValueList; override;
    function DataSetManagement(StartLBA, LBACount: Int64): Cardinal; override;

    function IsDataSetManagementSupported: Boolean; override;

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
        Buffer: T512Buffer;
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

procedure TSATCommandSet.SetInnerBufferAsFlagsAndCdb
  (Flags: ULONG; CommandDescriptorBlock: SCSI_COMMAND_DESCRIPTOR_BLOCK);
begin
  CommandDescriptorBlock.SectorCount := 1;
  IoInnerBuffer := GetCommonBuffer;
	IoInnerBuffer.Parameter.DataIn := Flags;
  IoInnerBuffer.Parameter.CommandDescriptorBlock := CommandDescriptorBlock;
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

function TSATCommandSet.SMARTReadData: TSMARTValueList;
begin
  SetBufferAndSMARTReadData;
  result := InterpretSMARTReadDataBuffer;
end;

function TSATCommandSet.IsDataSetManagementSupported: Boolean;
begin
  exit(false);
end;

function TSATCommandSet.DataSetManagement(StartLBA, LBACount: Int64): Cardinal;
begin
  raise ENotSupportedException.Create
    ('Not Supported Operation: DataSetManagement in SAT');
end;

end.
