unit CommandSet.NVMe.Intel.PortPart;

interface

uses
  Windows, SysUtils,
  OSFile.IoControl, CommandSet, BufferInterpreter, Device.SMART.List,
  BufferInterpreter.NVMe.Intel, CommandSet.NVMe;

type
  TIntelNVMePortCommandSet = class sealed(TNVMeCommandSet)
  public
    function IdentifyDevice: TIdentifyDeviceResult; override;
    function SMARTReadData: TSMARTValueList; override;
    function RAWIdentifyDevice: String; override;
    function RAWSMARTReadData: String; override;
  private
    const
      NVME_IOCTL_VENDOR_SPECIFIC_DW_SIZE = 6;
      NVME_IOCTL_CMD_DW_SIZE = 16;
      NVME_IOCTL_COMPLETE_DW_SIZE = 4;
      SMARTHealthInformation = 2;
      IntelSpecific = $CA;
      Temperature = $C5;
    type
      SRB_IO_CONTROL = record
        HeaderLength: ULONG;
        Signature: Array[0..7] of UCHAR;
        Timeout: ULONG;
        ControlCode: ULONG;
        ReturnCode: ULONG;
        Length: ULONG;
      end;
      NVME_PASS_THROUGH = packed record
        SrbIoCtrl: SRB_IO_CONTROL;
        VendorSpecific: Array[0..NVME_IOCTL_VENDOR_SPECIFIC_DW_SIZE - 1] of
          DWORD;
        NVMeCmd: Array[0..NVME_IOCTL_CMD_DW_SIZE - 1] of DWORD;
        CplEntry: Array[0..NVME_IOCTL_COMPLETE_DW_SIZE - 1] of DWORD;
        Direction: DWORD;
        QueueId: DWORD;
        DataBufferLen: DWORD;
        MetaDataLen: DWORD;
        ReturnBufferLen: DWORD;
      end;
      NVME_WITH_BUFFER = record
        Parameter: NVME_PASS_THROUGH;
        Buffer: TLargeBuffer;
      end;
    const
      NVME_NO_DATA_TX = 0;
      NVME_FROM_HOST_TO_DEV = 1;
      NVME_FROM_DEV_TO_HOST = 2;
      NVME_BI_DIRECTION = 3;
  private
    IoInnerBuffer: NVME_WITH_BUFFER;
    function GetCommonBuffer: NVME_WITH_BUFFER;
    function GetCommonCommandDescriptorBlock: NVME_PASS_THROUGH;
    procedure SetInnerBufferAsFlagsAndCdb(Flags: ULONG;
      CommandDescriptorBlock: NVME_PASS_THROUGH);
    function InterpretIdentifyDeviceBuffer: TIdentifyDeviceResult;
    procedure SetBufferAndIdentifyDevice;
    procedure SetInnerBufferToIdentifyDeviceCommand;
    function InterpretSMARTBuffer: TSMARTValueList;
    procedure SetBufferAndSMART(const LogIdentifier: Cardinal);
    procedure SetInnerBufferToSMARTCommand(const LogIdentifier: Cardinal);
    function InterpretIntelSpecificSMARTBuffer: TSMARTValueList;
    procedure AppendAndFree(var Destination: TSMARTValueList;
      const Source: TSMARTValueList);
    function NotAllZero: Boolean;
    function IsIntelDevice: Boolean;
  end;

implementation

{ TIntelNVMePortCommandSet }

function TIntelNVMePortCommandSet.IdentifyDevice: TIdentifyDeviceResult;
begin
  SetBufferAndIdentifyDevice;
  result := InterpretIdentifyDeviceBuffer;
  result.StorageInterface := TStorageInterface.NVMe;
end;

procedure TIntelNVMePortCommandSet.SetBufferAndIdentifyDevice;
begin
  SetInnerBufferToIdentifyDeviceCommand;
  IoControl(TIoControlCode.ScsiMiniport,
    BuildOSBufferBy<NVME_WITH_BUFFER, NVME_WITH_BUFFER>(IoInnerBuffer,
      IoInnerBuffer));
end;

procedure TIntelNVMePortCommandSet.SetInnerBufferToIdentifyDeviceCommand;
const
  Identify = 6;
  ReturnToHost = 1;
var
  CommandDescriptorBlock: NVME_PASS_THROUGH;
begin
  CommandDescriptorBlock := GetCommonCommandDescriptorBlock;
  CommandDescriptorBlock.NVMeCmd[0] := Identify;
  CommandDescriptorBlock.NVMeCmd[10] := ReturnToHost;
  SetInnerBufferAsFlagsAndCdb(NVME_FROM_DEV_TO_HOST, CommandDescriptorBlock);
end;

procedure TIntelNVMePortCommandSet.SetInnerBufferAsFlagsAndCdb
  (Flags: ULONG; CommandDescriptorBlock: NVME_PASS_THROUGH);
begin
  IoInnerBuffer := GetCommonBuffer;
  IoInnerBuffer.Parameter := CommandDescriptorBlock;
  IoInnerBuffer.Parameter.Direction := Flags;
end;

function TIntelNVMePortCommandSet.GetCommonCommandDescriptorBlock:
  NVME_PASS_THROUGH;
const
  NVME_SIG_STR: AnsiString = 'NvmeMini';
  NVME_PT_TIMEOUT = $3C;
  IntelNVMePassThough = $E0002000;
begin
  FillChar(result, SizeOf(result), #0);
  result.SrbIoCtrl.ControlCode := IntelNVMePassThough;
  result.SrbIoCtrl.HeaderLength := sizeof(SRB_IO_CONTROL);
  CopyMemory(@result.SrbIoCtrl.Signature[0], @NVME_SIG_STR[1],
    Length(NVME_SIG_STR));
  result.SrbIoCtrl.Timeout := NVME_PT_TIMEOUT;
  result.SrbIoCtrl.Length := SizeOf(NVME_WITH_BUFFER) -
    sizeof(SRB_IO_CONTROL);
  result.DataBufferLen := SizeOf(TLargeBuffer);
  result.ReturnBufferLen := SizeOf(IoInnerBuffer);
end;

function TIntelNVMePortCommandSet.GetCommonBuffer: NVME_WITH_BUFFER;
begin
  FillChar(result, SizeOf(result), #0);
end;

function TIntelNVMePortCommandSet.InterpretIdentifyDeviceBuffer:
  TIdentifyDeviceResult;
var
  SCSIBufferInterpreter: TIntelBufferInterpreter;
begin
  SCSIBufferInterpreter := TIntelBufferInterpreter.Create;
  result :=
    SCSIBufferInterpreter.LargeBufferToIdentifyDeviceResult(
      IoInnerBuffer.Buffer);
  FreeAndNil(SCSIBufferInterpreter);
end;

function TIntelNVMePortCommandSet.IsIntelDevice: Boolean;
begin
  result :=
    Pos('INTEL', IdentifyDevice.Model) = 1;
end;

function TIntelNVMePortCommandSet.SMARTReadData: TSMARTValueList;
begin
  result := nil;
  SetBufferAndSMART(SMARTHealthInformation);
  AppendAndFree(result, InterpretSMARTBuffer);
  if not IsIntelDevice then
    exit;
  SetBufferAndSMART(IntelSpecific);
  AppendAndFree(result, InterpretIntelSpecificSMARTBuffer);
  SetBufferAndSMART(Temperature);
  AppendAndFree(result, InterpretIntelSpecificSMARTBuffer);
end;

procedure TIntelNVMePortCommandSet.AppendAndFree(
  var Destination: TSMARTValueList;
  const Source: TSMARTValueList);
begin
  if Destination = nil then
    Destination := TSMARTValueList.Create;
  Destination.AddRange(Source.ToArray);
  Source.Free;
end;

procedure TIntelNVMePortCommandSet.SetBufferAndSMART(
  const LogIdentifier: Cardinal);
begin
  FillChar(IoInnerBuffer, SizeOf(IoInnerBuffer), #0);
  SetInnerBufferToSMARTCommand(LogIdentifier);
  IoControl(TIoControlCode.ScsiMiniport,
    BuildOSBufferBy<NVME_WITH_BUFFER, NVME_WITH_BUFFER>(IoInnerBuffer,
      IoInnerBuffer));
end;

procedure TIntelNVMePortCommandSet.SetInnerBufferToSMARTCommand(
  const LogIdentifier: Cardinal);
const
  GetLogPage = 2;
  MaxNUMD = $40;
  GlobalLogPage = $FFFFFFFF;
var
  CommandDescriptorBlock: NVME_PASS_THROUGH;
begin
  CommandDescriptorBlock := GetCommonCommandDescriptorBlock;
  CommandDescriptorBlock.NVMeCmd[0] := GetLogPage;
  CommandDescriptorBlock.NVMeCmd[1] := GlobalLogPage;
  CommandDescriptorBlock.NVMeCmd[10] := LogIdentifier or
    (Cardinal(MaxNUMD) shl 16);
  SetInnerBufferAsFlagsAndCdb(NVME_FROM_DEV_TO_HOST, CommandDescriptorBlock);
end;

function TIntelNVMePortCommandSet.InterpretSMARTBuffer: TSMARTValueList;
var
  SCSIBufferInterpreter: TIntelBufferInterpreter;
begin
  SCSIBufferInterpreter := TIntelBufferInterpreter.Create;
  result :=
    SCSIBufferInterpreter.LargeBufferToSMARTValueList(IoInnerBuffer.Buffer);
  FreeAndNil(SCSIBufferInterpreter);
end;

function TIntelNVMePortCommandSet.NotAllZero: Boolean;
var
  CurrentPoint: Byte;
begin
  result := false;
  for CurrentPoint in IoInnerBuffer.Buffer do
    result := result or (IoInnerBuffer.Buffer[CurrentPoint] > 0);
end;

function TIntelNVMePortCommandSet.RAWIdentifyDevice: String;
begin
  SetBufferAndIdentifyDevice;
  result :=
    IdentifyDevicePrefix +
    TBufferInterpreter.BufferToString(IoInnerBuffer.Buffer) + ';';
end;

function TIntelNVMePortCommandSet.RAWSMARTReadData: String;
begin
  SetBufferAndSMART(SMARTHealthInformation);
  result :=
    SMARTPrefix +
    TBufferInterpreter.BufferToString(IoInnerBuffer.Buffer) + ';';
  SetBufferAndSMART(IntelSpecific);
  result := result +
    'IntelSpecific' +
    TBufferInterpreter.BufferToString(IoInnerBuffer.Buffer) + ';';
  SetBufferAndSMART(Temperature);
  result := result +
    'Temperature' +
    TBufferInterpreter.BufferToString(IoInnerBuffer.Buffer) + ';';
end;

function TIntelNVMePortCommandSet.InterpretIntelSpecificSMARTBuffer:
  TSMARTValueList;
var
  SCSIBufferInterpreter: TIntelBufferInterpreter;
begin
  SCSIBufferInterpreter := TIntelBufferInterpreter.Create;
  if NotAllZero then
    result :=
      SCSIBufferInterpreter.VendorSpecificSMARTValueList(IoInnerBuffer.Buffer)
  else
    result := TSMARTValueList.Create;
  FreeAndNil(SCSIBufferInterpreter);
end;

end.
