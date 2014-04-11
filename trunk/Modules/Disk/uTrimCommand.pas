unit uTrimCommand;

interface

uses Classes, SysUtils, uDiskFunctions, Dialogs, Windows;

function SendTrimCommand(const DriveLetter: String; StartLBA, LBACount: Int64): Cardinal;
function IsZeroSector(const DriveLetter: String; StartLBA: Int64): Byte;

const
  TRIM_ZeroSector = 1;
  TRIM_NonZeroSector = 0;
  TRIM_Error = 2;

implementation

function SendTrimCommand(const DriveLetter: String; StartLBA, LBACount: Int64): Cardinal;
var
  ICDBuffer: ATA_PTH_DIR_BUFFER;
  BytesRead: Cardinal;
  hPhyDevice: THandle;
begin
  result := 0;
  FillChar(ICDBuffer, SizeOf(ICDBuffer), #0);
  hPhyDevice := CreateFile(
                PChar(DriveLetter),
                GENERIC_READ or GENERIC_WRITE,
                FILE_SHARE_READ or FILE_SHARE_WRITE,
                nil,
                OPEN_EXISTING,
                0,
                0);

  if StartLBA <> 0 then
  begin
    ICDBuffer.PTH.Length := SizeOf(ICDBuffer.PTH);
    ICDBuffer.PTH.AtaFlags := ATA_FLAGS_48BIT_COMMAND or ATA_FLAGS_DATA_OUT or ATA_FLAGS_USE_DMA;
    ICDBuffer.PTH.DataTransferLength := SizeOf(ICDBuffer.Buffer);
    ICDBuffer.PTH.TimeOutValue := 30;
    ICDBuffer.PTH.DataBuffer := @ICDBuffer.Buffer;

    ICDBuffer.PTH.CurrentTaskFile[0] := 1;
    ICDBuffer.PTH.CurrentTaskFile[1] := 1;
    ICDBuffer.PTH.CurrentTaskFile[6] := $6;

    ICDBuffer.Buffer[0] := StartLBA and 255;
    StartLBA := StartLBA shr 8;
    ICDBuffer.Buffer[1] := StartLBA and 255;
    StartLBA := StartLBA shr 8;
    ICDBuffer.Buffer[2] := StartLBA and 255;
    StartLBA := StartLBA shr 8;
    ICDBuffer.Buffer[3] := StartLBA and 255;
    StartLBA := StartLBA shr 8;
    ICDBuffer.Buffer[4] := StartLBA and 255;
    StartLBA := StartLBA shr 8;
    ICDBuffer.Buffer[5] := StartLBA;

    ICDBuffer.Buffer[6] := LBACount and 255;
    ICDBuffer.Buffer[7] := LBACount shr 8;

    DeviceIOControl(hPhyDevice, IOCTL_ATA_PASS_THROUGH_DIRECT, @ICDBuffer, SizeOf(ICDBuffer), @ICDBuffer, SizeOf(ICDBuffer), BytesRead, nil);
    result := GetLastError;
  end;
  CloseHandle(hPhyDevice);
end;

function IsZeroSector(const DriveLetter: String; StartLBA: Int64): Byte;
var
  ICBuffer: ATA_PTH_BUFFER_4K;
  BytesRead: Cardinal;
  i: integer;
  hPhyDevice: THandle;
begin
  hPhyDevice := CreateFile(PChar(DriveLetter), GENERIC_READ or GENERIC_WRITE,
                    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);

  result := 0;
  if GetLastError = 0 Then
  begin
    ICBuffer.PTH.Length := SizeOf(ICBuffer.PTH);
    ICBuffer.PTH.AtaFlags := ATA_FLAGS_48BIT_COMMAND or ATA_FLAGS_DATA_IN;
    ICBuffer.PTH.DataTransferLength := SizeOf(ICBuffer.Buffer);
    ICBuffer.PTH.TimeOutValue := 2;
    ICBuffer.PTH.DataBufferOffset := PChar(@ICBuffer.Buffer) - PChar(@ICBuffer.PTH) + 20;

    ICBuffer.PTH.CurrentTaskFile[2] := StartLBA and 255;
    StartLBA := StartLBA shr 8;
    ICBuffer.PTH.CurrentTaskFile[3] := StartLBA and 255;
    StartLBA := StartLBA shr 8;
    ICBuffer.PTH.CurrentTaskFile[4] := StartLBA and 255;
    StartLBA := StartLBA shr 8;
    ICBuffer.PTH.PreviousTaskFile[2] := StartLBA and 255;
    StartLBA := StartLBA shr 8;
    ICBuffer.PTH.PreviousTaskFile[3] := StartLBA and 255;
    StartLBA := StartLBA shr 8;
    ICBuffer.PTH.PreviousTaskFile[4] := StartLBA and 255;

    ICBuffer.PTH.CurrentTaskFile[1] := $8;
    ICBuffer.PTH.CurrentTaskFile[5] := $1 shl 6;
    ICBuffer.PTH.CurrentTaskFile[6] := $24;

    DeviceIOControl(hPhyDevice, IOCTL_ATA_PASS_THROUGH, @ICBuffer, SizeOf(ICBuffer), @ICBuffer, SizeOf(ICBuffer), BytesRead, nil);
    if BytesRead <> 4136 then
      result := TRIM_Error;
  end;

  if result <> TRIM_Error then
  begin
    for i := 0 to 4095 do
      ICBuffer.Buffer[0] := ICBuffer.Buffer[0] or ICBuffer.Buffer[i];

    if ICBuffer.Buffer[0] = 0 then
      result := TRIM_ZeroSector
    else
      result := TRIM_NonZeroSector;
  end;

  CloseHandle(hPhyDevice);
end;
end.
