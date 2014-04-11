unit uTrimCommand;

interface

uses Classes, SysUtils, uDiskFunctions, Math, Dialogs, Windows;

procedure SendTrimCommand(const DriveLetter: String; StartLBA, LBACount: Int64);
procedure SendTrimCommandSCSI(const DriveLetter: String; StartLBA, LBACount: Int64);

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

procedure SendTrimCommandSCSI(const DriveLetter: String; StartLBA, LBACount: Int64);
var
  hdrive: Cardinal;
  dwBytesReturned: DWORD;
  Status: Longbool;
  ICBuffer: SCSI_PTH_BUFFER;
  CurrBuf: Integer;
begin
  fillchar(ICBuffer, SizeOf(ICBuffer), #0);
  hdrive := CreateFile(
                PChar('\\.\' + DriveLetter),
                GENERIC_READ or GENERIC_WRITE,
                FILE_SHARE_READ or FILE_SHARE_WRITE,
                nil,
                OPEN_EXISTING,
                0,
                0);

	ICBuffer.spt.Length     := sizeof(SCSI_PASS_THROUGH);
  ICBuffer.spt.TargetId   := 1;
  ICBuffer.spt.CdbLength  := 12;
	ICBuffer.spt.SenseInfoLength := 24;
	ICBuffer.spt.DataIn  := 0;
	ICBuffer.spt.DataTransferLength := 512;
	ICBuffer.spt.TimeOutValue := 2;
	ICBuffer.spt.DataBufferOffset := pansichar(@ICBuffer.Buffer)-pansichar(@ICBuffer);
	ICBuffer.spt.SenseInfoOffset  := pansichar(@ICBuffer.SenseBuf)-pansichar(@ICBuffer);
  ICBuffer.spt.Cdb[0] := $A1;
  ICBuffer.spt.Cdb[1] := (5 or 6) shl 1;
  ICBuffer.spt.Cdb[2] := $6;
  ICBuffer.spt.Cdb[3] := $1;
  ICBuffer.spt.Cdb[4] := $1;
  ICBuffer.spt.Cdb[8] := $40;
	ICBuffer.spt.Cdb[9] := $6;

  if StartLBA <> 0 then
  begin
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

    Status := DeviceIOControl(hdrive, IOCTL_SCSI_PASS_THROUGH, @ICBuffer, SizeOf(ICBuffer), @ICBuffer, SizeOf(ICBuffer), dwBytesReturned, nil);
  end;
  ShowMessage(IntToStr(GetLastError));
  CloseHandle(hdrive);
end;
end.
