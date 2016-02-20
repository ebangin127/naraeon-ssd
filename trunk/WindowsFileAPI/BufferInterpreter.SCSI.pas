unit BufferInterpreter.SCSI;

interface

uses
  SysUtils,
  BufferInterpreter, Device.SMART.List;

type
  TSCSIBufferInterpreter = class sealed(TBufferInterpreter)
  public
    function BufferToIdentifyDeviceResult(
      const Buffer: TSmallBuffer): TIdentifyDeviceResult; override;
    function BufferToSMARTValueList(
      const Buffer: TSmallBuffer): TSMARTValueList; override;
    function LargeBufferToIdentifyDeviceResult(
      const Buffer: TLargeBuffer): TIdentifyDeviceResult; override;
    function LargeBufferToSMARTValueList(
      const Buffer: TLargeBuffer): TSMARTValueList; override;
  private
    BufferInterpreting: TSmallBuffer;
    function GetFirmwareFromBuffer: String;
    function GetLBASizeFromBuffer: Cardinal;
    function GetModelFromBuffer: String;
    function GetSerialFromBuffer: String;
  end;

implementation

{ TSCSIBufferInterpreter }

function TSCSIBufferInterpreter.LargeBufferToIdentifyDeviceResult(
  const Buffer: TLargeBuffer): TIdentifyDeviceResult;
var
  SmallBuffer: TSmallBuffer;
begin
  Move(Buffer, SmallBuffer, SizeOf(SmallBuffer));
  result := BufferToIdentifyDeviceResult(SmallBuffer);
end;

function TSCSIBufferInterpreter.LargeBufferToSMARTValueList(
  const Buffer: TLargeBuffer): TSMARTValueList;
var
  SmallBuffer: TSmallBuffer;
begin
  Move(Buffer, SmallBuffer, SizeOf(SmallBuffer));
  result := BufferToSMARTValueList(SmallBuffer);
end;

function TSCSIBufferInterpreter.BufferToSMARTValueList(
  const Buffer: TSmallBuffer): TSMARTValueList;
begin
  raise ENotSupportedException.Create('Not supported for SCSI');
end;

function TSCSIBufferInterpreter.GetModelFromBuffer: String;
const
  ModelStart = 4;
  ModelEnd = 15;
var
  CurrentWord: Integer;
begin
  result := '';
  for CurrentWord := ModelStart to ModelEnd do
    result :=
      result +
      Chr(BufferInterpreting[CurrentWord * 2]) +
      Chr(BufferInterpreting[CurrentWord * 2 + 1]);
  result := Trim(result);
end;

function TSCSIBufferInterpreter.GetFirmwareFromBuffer: String;
const
  FirmwareStart = 16;
  FirmwareEnd = 17;
var
  CurrentWord: Integer;
begin
  result := '';
  for CurrentWord := FirmwareStart to FirmwareEnd do
    result :=
      result +
      Chr(BufferInterpreting[CurrentWord * 2]) +
      Chr(BufferInterpreting[CurrentWord * 2 + 1]);
  result := Trim(result);
end;

function TSCSIBufferInterpreter.GetSerialFromBuffer: String;
const
  SerialStart = 18;
  SerialEnd = 22;
var
  CurrentWord: Integer;
begin
  result := '';
  for CurrentWord := SerialStart to SerialEnd do
    result :=
      result +
      Chr(BufferInterpreting[CurrentWord * 2]) +
      Chr(BufferInterpreting[CurrentWord * 2 + 1]);
  result := Trim(result);
end;

function TSCSIBufferInterpreter.GetLBASizeFromBuffer: Cardinal;
const
  ATA_LBA_SIZE = 512;
begin
  result := ATA_LBA_SIZE;
end;

function TSCSIBufferInterpreter.BufferToIdentifyDeviceResult(
  const Buffer: TSmallBuffer): TIdentifyDeviceResult;
begin
  FillChar(result, SizeOf(result), #0);
  BufferInterpreting := Buffer;
  result.Model := GetModelFromBuffer;
  result.Firmware := GetFirmwareFromBuffer;
  result.Serial := GetSerialFromBuffer;
  result.LBASize := GetLBASizeFromBuffer;
  result.RotationRate.Supported := false;
end;

end.
