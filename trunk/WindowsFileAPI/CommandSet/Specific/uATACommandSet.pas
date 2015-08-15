unit uATACommandSet;

interface

uses
  Windows, SysUtils, Dialogs,
  uIoControlFile, uCommandSet, uBufferInterpreter, uSMARTValueList,
  uATABufferInterpreter;

type
  TATACommandSet = class sealed(TCommandSet)
  public
    function IdentifyDevice: TIdentifyDeviceResult; override;
    function SMARTReadData: TSMARTValueList; override;
    function DataSetManagement(StartLBA, LBACount: Int64): Cardinal; override;

    function IsDataSetManagementSupported: Boolean; override;

  private
    type
      ATA_SINGLE_TASK_FILE = record
        Features: UCHAR;
        SectorCount: UCHAR;
        LBALoSectorNumber: UCHAR;
        LBAMidCycleLo: UCHAR;
        LBAHiCycleHi: UCHAR;
        DeviceHead: UCHAR;
        Command: UCHAR;
        Reserved: UCHAR;
      end;
      ATA_TASK_FILES = record
        PreviousTaskFile: ATA_SINGLE_TASK_FILE;
        CurrentTaskFile: ATA_SINGLE_TASK_FILE;
      end;
      ATA_PASS_THROUGH_DIRECT = record
        Length: USHORT;
        AtaFlags: USHORT;
        PathId: UCHAR;
        TargetId: UCHAR;
        Lun: UCHAR;
        ReservedAsUchar: UCHAR;
        DataTransferLength: ULONG;
        TimeOutValue: ULONG;
        ReservedAsUlong: ULONG;
        DataBuffer: PVOID;
        TaskFile: ATA_TASK_FILES;
      end;
      ATA_WITH_BUFFER = record
        Parameter: ATA_PASS_THROUGH_DIRECT;
        Buffer: T512Buffer;
      end;

    const
      ATA_FLAGS_DRDY_REQUIRED = 1;
      ATA_FLAGS_DATA_IN = 1 shl 1;
      ATA_FLAGS_DATA_OUT = 1 shl 2;
      ATA_FLAGS_48BIT_COMMAND = 1 shl 3;
      ATA_FLAGS_USE_DMA = 1 shl 4;
      ATA_FLAGS_NO_MULTIPLE = 1 shl 5;

  private
    IoInnerBuffer: ATA_WITH_BUFFER;
    IoOSBuffer: TIoControlIOBuffer;

    function GetCommonBuffer: ATA_WITH_BUFFER;
    function GetCommonTaskFile: ATA_TASK_FILES;
    procedure SetOSBufferByInnerBuffer;
    procedure SetInnerBufferAsFlagsAndTaskFile(Flags: ULONG;
      TaskFile: ATA_TASK_FILES);
    procedure SetInnerBufferToSMARTReadData;
    procedure SetInnerBufferToDataSetManagement(StartLBA, LBACount: Int64);
    procedure SetInnerBufferToIdentifyDevice;
    procedure SetDataSetManagementBuffer(StartLBA, LBACount: Int64);
    procedure SetLBACountToDataSetManagementBuffer(LBACount: Int64);
    procedure SetStartLBAToDataSetManagementBuffer(StartLBA: Int64);
    function InterpretIdentifyDeviceBuffer: TIdentifyDeviceResult;
    procedure SetBufferAndIdentifyDevice;
    function InterpretSMARTReadDataBuffer: TSMARTValueList;
    procedure SetBufferAndSMARTReadData;
  end;

implementation

{ TATACommandSet }

function TATACommandSet.GetCommonBuffer: ATA_WITH_BUFFER;
begin
  FillChar(result, SizeOf(result), #0);
  result.Parameter.Length := SizeOf(result.Parameter);
  result.Parameter.DataTransferLength := SizeOf(result.Buffer);
  result.Parameter.TimeOutValue := 30;
end;

function TATACommandSet.GetCommonTaskFile: ATA_TASK_FILES;
begin
  FillChar(result, SizeOf(result), #0);
end;

procedure TATACommandSet.SetInnerBufferAsFlagsAndTaskFile
  (Flags: ULONG; TaskFile: ATA_TASK_FILES);
begin
  IoInnerBuffer := GetCommonBuffer;
  IoInnerBuffer.Parameter.AtaFlags := Flags;
  IoInnerBuffer.Parameter.TaskFile := TaskFile;
  IoInnerBuffer.Parameter.DataBuffer := @IoInnerBuffer.Buffer;
end;

procedure TATACommandSet.SetInnerBufferToIdentifyDevice;
const
  IdentifyDeviceCommand = $EC;
var
  IoTaskFile: ATA_TASK_FILES;
begin
  IoTaskFile := GetCommonTaskFile;
  IoTaskFile.CurrentTaskFile.Command := IdentifyDeviceCommand;
  SetInnerBufferAsFlagsAndTaskFile(ATA_FLAGS_DATA_IN, IoTaskFile);
end;

procedure TATACommandSet.SetOSBufferByInnerBuffer;
begin
  IoOSBuffer.InputBuffer.Size := SizeOf(IoInnerBuffer);
  IoOSBuffer.InputBuffer.Buffer := @IOInnerBuffer;

  IoOSBuffer.OutputBuffer.Size := SizeOf(IoInnerBuffer);
  IoOSBuffer.OutputBuffer.Buffer := @IOInnerBuffer;
end;

procedure TATACommandSet.SetBufferAndIdentifyDevice;
begin
  SetInnerBufferToIdentifyDevice;
  SetOSBufferByInnerBuffer;
  IoControl(TIoControlCode.ATAPassThroughDirect, IoOSBuffer);
end;

function TATACommandSet.InterpretIdentifyDeviceBuffer:
  TIdentifyDeviceResult;
var
  ATABufferInterpreter: TATABufferInterpreter;
begin
  ATABufferInterpreter := TATABufferInterpreter.Create;
  result :=
    ATABufferInterpreter.BufferToIdentifyDeviceResult(IoInnerBuffer.Buffer);
  FreeAndNil(ATABufferInterpreter);
end;

function TATACommandSet.IdentifyDevice: TIdentifyDeviceResult;
begin
  SetBufferAndIdentifyDevice;
  result := InterpretIdentifyDeviceBuffer;
  result.StorageInterface := TStorageInterface.ATA;
  result.IsDataSetManagementSupported := IsDataSetManagementSupported;
end;

procedure TATACommandSet.SetInnerBufferToSMARTReadData;
const
  SMARTFeatures = $D0;
  SMARTCycleLo = $4F;
  SMARTCycleHi = $C2;
  SMARTReadDataCommand = $B0;
var
  IoTaskFile: ATA_TASK_FILES;
begin
  IoTaskFile := GetCommonTaskFile;
  IoTaskFile.CurrentTaskFile.Features := SMARTFeatures;
  IoTaskFile.CurrentTaskFile.LBAMidCycleLo := SMARTCycleLo;
  IoTaskFile.CurrentTaskFile.LBAHiCycleHi := SMARTCycleHi;
  IoTaskFile.CurrentTaskFile.Command := SMARTReadDataCommand;
  SetInnerBufferAsFlagsAndTaskFile(ATA_FLAGS_DATA_IN, IoTaskFile);
end;

procedure TATACommandSet.SetBufferAndSMARTReadData;
begin
  SetInnerBufferToSMARTReadData;
  SetOSBufferByInnerBuffer;
  IoControl(TIoControlCode.ATAPassThroughDirect, IoOSBuffer);
end;

function TATACommandSet.InterpretSMARTReadDataBuffer:
  TSMARTValueList;
var
  ATABufferInterpreter: TATABufferInterpreter;
begin
  ATABufferInterpreter := TATABufferInterpreter.Create;
  result := ATABufferInterpreter.BufferToSMARTValueList(IoInnerBuffer.Buffer);
  FreeAndNil(ATABufferInterpreter);
end;

function TATACommandSet.SMARTReadData: TSMARTValueList;
begin
  SetBufferAndSMARTReadData;
  result := InterpretSMARTReadDataBuffer;
end;

function TATACommandSet.IsDataSetManagementSupported: Boolean;
begin
  exit(true);
end;

procedure TATACommandSet.SetStartLBAToDataSetManagementBuffer(StartLBA: Int64);
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

procedure TATACommandSet.SetLBACountToDataSetManagementBuffer(LBACount: Int64);
const
  LBACountHi = 7;
  LBACountLo = 6;
begin
  IoInnerBuffer.Buffer[LBACountLo] := LBACount and 255;
  IoInnerBuffer.Buffer[LBACountHi] := LBACount shr 8;
end;

procedure TATACommandSet.SetDataSetManagementBuffer
  (StartLBA, LBACount: Int64);
begin
  SetStartLBAToDataSetManagementBuffer(StartLBA);
  SetLBACountToDataSetManagementBuffer(LBACount);
end;

procedure TATACommandSet.SetInnerBufferToDataSetManagement
  (StartLBA, LBACount: Int64);
const
  DataSetManagementFlags =
    ATA_FLAGS_48BIT_COMMAND or ATA_FLAGS_DATA_OUT or ATA_FLAGS_USE_DMA;
  DataSetManagementFeatures = $1;
  DataSetManagementSectorCount = $1;
  DataSetManagementCommand = $6;
var
  IoTaskFile: ATA_TASK_FILES;
begin
  IoTaskFile := GetCommonTaskFile;
  IoTaskFile.CurrentTaskFile.Features := DataSetManagementFeatures;
  IoTaskFile.CurrentTaskFile.SectorCount := DataSetManagementSectorCount;
  IoTaskFile.CurrentTaskFile.Command := DataSetManagementCommand;
  SetInnerBufferAsFlagsAndTaskFile(DataSetManagementFlags, IoTaskFile);
  SetDataSetManagementBuffer(StartLBA, LBACount);
end;

function TATACommandSet.DataSetManagement(StartLBA, LBACount: Int64): Cardinal;
begin
  SetInnerBufferToDataSetManagement(StartLBA, LBACount);
  SetOSBufferByInnerBuffer;
  result := ExceptionFreeIoControl
    (TIoControlCode.ATAPassThroughDirect, IoOSBuffer);
end;

end.
