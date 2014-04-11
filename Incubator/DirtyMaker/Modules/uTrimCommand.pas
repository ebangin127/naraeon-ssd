unit uTrimCommand;

interface

uses Classes, SysUtils, uDiskFunctions, Math, Dialogs, Windows;

procedure SendTrimCommand(const DriveLetter: String; StartLBA, LBACount: Int64);

implementation

procedure SendTrimCommand(const DriveLetter: String; StartLBA, LBACount: Int64);
var
  ICBuffer: ATA_PTH_BUFFER;
  BytesRead: Cardinal;
  hPhyDevice: THandle;
begin
  FillChar(ICBuffer, SizeOf(ICBuffer), #0);
  hPhyDevice := CreateFile(
                PChar('\\.\' + DriveLetter),
                GENERIC_READ or GENERIC_WRITE,
                FILE_SHARE_READ or FILE_SHARE_WRITE,
                nil,
                OPEN_EXISTING,
                0,
                0);

  if StartLBA <> 0 then
  begin
    ICBuffer.PTH.Length := SizeOf(ICBuffer.PTH);
    ICBuffer.PTH.AtaFlags := ATA_FLAGS_DRDY_REQUIRED or ATA_FLAGS_DATA_OUT or ATA_FLAGS_USE_DMA;
    ICBuffer.PTH.DataTransferLength := 512;
    ICBuffer.PTH.TimeOutValue := 180;
    ICBuffer.PTH.DataBufferOffset := SizeOf(ICBuffer.PTH);

    ICBuffer.PTH.CurrentTaskFile[0] := 1;
    ICBuffer.PTH.CurrentTaskFile[1] := 1;
    ICBuffer.PTH.CurrentTaskFile[5] := $40;
    ICBuffer.PTH.CurrentTaskFile[6] := $6;

    ICBuffer.Buffer[0] := StartLBA and 255;
    StartLBA := StartLBA shr 8;
    ICBuffer.Buffer[1] := StartLBA and 255;
    StartLBA := StartLBA shr 8;
    ICBuffer.Buffer[2] := StartLBA and 255;
    StartLBA := StartLBA shr 8;
    ICBuffer.Buffer[3] := StartLBA and 255;
    StartLBA := StartLBA shr 8;
    ICBuffer.Buffer[4] := StartLBA and 255;
    StartLBA := StartLBA shr 8;
    ICBuffer.Buffer[5] := StartLBA;

    ICBuffer.Buffer[6] := LBACount and 255;
    ICBuffer.Buffer[7] := LBACount shr 8;

    DeviceIOControl(hPhyDevice, IOCTL_ATA_PASS_THROUGH, @ICBuffer, SizeOf(ICBuffer), @ICBuffer, SizeOf(ICBuffer), BytesRead, nil);
  end;
  CloseHandle(hPhyDevice);
end;
end.
