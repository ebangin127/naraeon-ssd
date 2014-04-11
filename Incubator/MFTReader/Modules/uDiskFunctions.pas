unit uDiskFunctions;

interface

uses Windows, SysUtils, Dialogs, Math, uRegFunctions, Classes, uAlert, URLMon, ComObj, Variants, ActiveX, OleCtrls;

type
  //--GetMotherDrive--//
  DISK_EXTENT = RECORD
    DiskNumber: DWORD;
    StartingOffset: TLargeInteger;
    ExtentLength: TLargeInteger;
  end;

  VOLUME_DISK_EXTENTS = Record
    NumberOfDiskExtents: DWORD;
    Extents: Array[0..50] of DISK_EXTENT;
  end;
  //---GetMotherDrive---//


  //---GetDiskSize---//
  DISK_GEOMETRY = Record
    Cylinders: TLargeInteger;
    MediaType: Byte;
    TracksPerCylinder: DWORD;
    SectorsPerTrack: DWORD;
    BytesPerSector: DWORD;
  end;

  DISK_GEOMETRY_EX = Record
    Geometry: DISK_GEOMETRY;
    DiskSize: TLargeInteger;
    Data: Array[0..1] of UChar;
  end;
  //---GetDiskSize---//


  //---DeviceIOCtl에 필수---//
  TDRIVERSTATUS = Record
    bDriverError: UChar;
    bIDEError: UChar;
    bReserved: Array[0..1] of UCHAR;
    dwReserved: Array[0..1] of UCHAR;
  end;

  SENDCMDOUTPARAMS  = Record
    cBufferSize: DWORD;
    DriverStatus: TDRIVERSTATUS;
    bBuffer: Array[0..1023] of UCHAR;
  end;

  IDEREGS  = packed Record
    bFeaturesReg: UCHAR;
    bSectorCountReg: UCHAR;
    bSectorNumberReg: UCHAR;
    bCylLowReg: UCHAR;
    bCylHighReg: UCHAR;
    bDriveHeadReg: UCHAR;
    bCommandReg: UCHAR;
    bReserved: UCHAR;
  end;

  SENDCMDINPARAMS  = Record
    cBufferSize: dword;
    irDriveRegs: IDEREGS;
    bDriveNumber: byte;
    bReserved: Array[0..2] of byte;
    dwReserved: Array[0..3] of dword;
  end;
  //---DeviceIOCtl에 필수---//


  //---ATA + DeviceIOCtl---//
  ATA_PASS_THROUGH_EX = Record
    Length: USHORT;
    AtaFlags: USHORT;
    PathId: UCHAR;
    TargetId: UCHAR;
    Lun: UCHAR;
    ReservedAsUchar: UCHAR;
    DataTransferLength: ULONG;
    TimeOutValue: ULONG;
    ReservedAsUlong: ULONG;
    DataBufferOffset: ULONG_PTR;
    PreviousTaskFile: Array[0..7] of UCHAR;
    CurrentTaskFile: Array[0..7] of UCHAR;
  end;

  ATA_PTH_BUFFER = Record
    PTH: ATA_PASS_THROUGH_EX;
    Buffer: Array[0..511] of Byte;
  end;
  //---ATA + DeviceIOCtl---//


  //---SAT + DeviceIOCtl---//
  SCSI_PASS_THROUGH = record
    Length: Word;
    ScsiStatus: Byte;
    PathId: Byte;
    TargetId: Byte;
    Lun: Byte;
    CdbLength: Byte;
    SenseInfoLength: Byte;
    DataIn: Byte;
    DataTransferLength: ULong;
    TimeOutValue: ULong;
    DataBufferOffset: ULong;
    SenseInfoOffset: ULong;
    Cdb: array[0..12] of UCHAR;
  end;

  SCSI_PTH_BUFFER = record
    spt: SCSI_PASS_THROUGH;
    SenseBuf: array[0..31] of UCHAR;
    Buffer: array[0..511] of UCHAR;
  end;
  //---SAT + DeviceIOCtl---//


  //---GetPartitionList---//
  TDriveLetters = Record
    Letters: String;
    StartOffset: Array[0..99] of TLargeInteger;
  end;
  //---GetPartitionList---//


  //---Trim Command--//
  PSTARTING_LCN_INPUT_BUFFER = ^STARTING_LCN_INPUT_BUFFER;
  {$EXTERNALSYM PSTARTING_LCN_INPUT_BUFFER}
  STARTING_LCN_INPUT_BUFFER = record
    StartingLcn: LARGE_INTEGER;
  end;

  PVOLUME_BITMAP_BUFFER = ^VOLUME_BITMAP_BUFFER;
  {$EXTERNALSYM PVOLUME_BITMAP_BUFFER}
  VOLUME_BITMAP_BUFFER = record
    StartingLcn: LARGE_INTEGER;
    BitmapSize: LARGE_INTEGER;
    Buffer: array [0..511] of Byte;
  end;

  NTFS_INFO = record
    ErrorCode: Integer;
    SectorPerCluster: Integer;
    FirstMFTStart: _LARGE_INTEGER;
    FirstMFTEnd: _LARGE_INTEGER;
    SecondMFTStart: _LARGE_INTEGER;
    SecondMFTEnd: _LARGE_INTEGER;
    MFTZoneStart: _LARGE_INTEGER;
    MFTZoneEnd: _LARGE_INTEGER;
  end;

  NTFS_VOLUME_DATA_BUFFER = record
    VolumeSerialNumber: _LARGE_INTEGER;
    NumberSectors: _LARGE_INTEGER;
    TotalClusters: _LARGE_INTEGER;
    FreeClusters: _LARGE_INTEGER;
    TotalReserved: _LARGE_INTEGER;
    BytesPerSector: DWORD;
    BytesPerCluster: DWORD;
    BytesPerFileRecordSegment: DWORD;
    ClustersPerFileRecordSegment: DWORD;
    MftValidDataLength: _LARGE_INTEGER;
    MftStartLcn: _LARGE_INTEGER;
    Mft2StartLcn: _LARGE_INTEGER;
    MftZoneStart: _LARGE_INTEGER;
    MftZoneEnd: _LARGE_INTEGER;
  end;

  RETRIEVAL_POINTER_BASE = record
    FileAreaOffset: _LARGE_INTEGER;
  end;
  //---Trim Command--//

  //---Firmware--//
  FirmCheck = record
    FirmExists: Boolean;
    FirmPath: String;
  end;
  //---Firmware--//


//디스크 - 파티션 간 관계 얻어오기
function GetPartitionList(DiskNumber: String): TDriveLetters;
function GetMotherDrive(const DriveLetter: String): VOLUME_DISK_EXTENTS;
function GetPartitionLength(DriveLetter: String): Int64;
procedure GetChildDrives(DiskNumber: String; ChildDrives: TStrings);

//SMART, 용량, 볼륨 이름 얻어오기
function GetSmartDataATA(const Harddisk: String): SENDCMDOUTPARAMS;
function GetSmartDataSCSI(const Harddisk: String): SENDCMDOUTPARAMS;
function GetNTFSVolumeData(const DriveLetter: String): NTFS_INFO;
function GetDiskSize(const DiskNumber: String): TLargeInteger;
function GetVolumeLabel(DriveName: String): string;
function GetFirstSector(const DriveLetter: String): Int64;
function ReadNativeMaxAddress(DeviceName: String): UInt64; // 최대 HPA 용량
function SetMaxAddress(DeviceName: String; MaxLBA: UInt64): Boolean;

//Fixed HDD, USB Mass Storage 정보 얻어오기
function GetSSDList: TStringList;
procedure GetUSBDrives(USBDrives: TStrings);

//기타 기능들
function WriteBufferCheck: TStringList; //쓰기 버퍼 체크되어있나 확인
function ExtractDrvNum(const Input: String): String; //PhysicalDrive0 -> 0
                                                     //\\.\PhyscialDrive0 -> 0
function OpenProcWithOutput(Path: String; Command: String): AnsiString; //프로그램 열기

var
  AllDrives: String;
  WMIEnabled: Boolean;

const
  IOCTL_SCSI_BASE = FILE_DEVICE_CONTROLLER;
  IOCTL_ATA_PASS_THROUGH = (IOCTL_SCSI_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14)
                            or ($040B shl 2) or (METHOD_BUFFERED);
  IOCTL_ATA_PASS_THROUGH_DIRECT = (IOCTL_SCSI_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14)
                            or ($0405 shl 2) or (METHOD_BUFFERED);
  IOCTL_SCSI_PASS_THROUGH      =  $0004D004;

  SMART_READ_ATTRIBUTE_VALUES = $D0;
  SMART_CYL_LOW = $4F;
  SMART_CYL_HI = $C2;
  SMART_CMD = $B0;

  ATA_FLAGS_DRDY_REQUIRED = 1;
  ATA_FLAGS_DATA_IN = 1 shl 1;
  ATA_FLAGS_DATA_OUT = 1 shl 2;
  ATA_FLAGS_48BIT_COMMAND = 1 shl 3;
  ATA_FLAGS_USE_DMA = 1 shl 4;

  ATAMode = false;
  SCSIMode = true;

  VolumeNames = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';

implementation

uses uSSDInfo;

function GetSmartDataATA(const Harddisk: String): SENDCMDOUTPARAMS;
var
  hdrive: Cardinal;
  dwBytesReturned: DWORD;
  opar: SENDCMDOUTPARAMS;
  opar2: SENDCMDOUTPARAMS;
  Status: Longbool;
  ipar2: SENDCMDINPARAMS;
begin
  ipar2.cBufferSize := 512;
  ipar2.bDriveNumber := StrToInt(Harddisk[Length(Harddisk)]);
  ipar2.irDriveRegs.bFeaturesReg := SMART_READ_ATTRIBUTE_VALUES;
  ipar2.irDriveRegs.bSectorCountReg := 1;
  ipar2.irDriveRegs.bSectorNumberReg := 1;
  ipar2.irDriveRegs.bCylLowReg := SMART_CYL_LOW;
  ipar2.irDriveRegs.bCylHighReg := SMART_CYL_HI;
  ipar2.irDriveRegs.bDriveHeadReg := ((StrToInt(Harddisk[Length(Harddisk)]) and 1) shl 4) or $a0;
  ipar2.irDriveRegs.bCommandReg := SMART_CMD;

  fillchar(opar, SizeOf(opar), #0);

  hdrive := CreateFile(PChar(Harddisk), GENERIC_READ or GENERIC_WRITE,
                    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);

  If GetLastError = 0 Then
  begin
    Status := DeviceIoControl(hdrive, SMART_RCV_DRIVE_DATA, @ipar2, SizeOf(SENDCMDINPARAMS), @opar, SizeOf(SENDCMDOUTPARAMS), dwBytesReturned, nil);
    if (status = false) or (getLastError <> 0) then
      result := opar2;
    CloseHandle(hdrive);
  end
  else
  begin
      AlertCreate(nil, 'Failed Getting SMART : '+inttostr(getLastError()) + Chr(13) + Chr(10) +
                            'Drive : ' + Harddisk + Chr(13) + Chr(10) +
                            'And There is NO DRIVE');
  end;
  Result := opar;
end;

function GetSmartDataSCSI(const Harddisk: String): SENDCMDOUTPARAMS;
var
  hdrive: Cardinal;
  dwBytesReturned: DWORD;
  opar: SENDCMDOUTPARAMS;
  opar2: SENDCMDOUTPARAMS;
  Status: Longbool;
  TempInfo: TSSDInfo;
  ipar: SCSI_PTH_BUFFER;
  CurrBuf: Integer;
begin
  fillchar(ipar, SizeOf(ipar), #0);
  fillchar(opar, SizeOf(opar), #0);
  fillchar(opar2, SizeOf(opar2), #0);
	ipar.spt.Length     := sizeof(SCSI_PASS_THROUGH);
  ipar.spt.TargetId   := 1;
  ipar.spt.CdbLength  := 12;
	ipar.spt.SenseInfoLength := 24;
	ipar.spt.DataIn  := 1;
	ipar.spt.DataTransferLength := 512;
	ipar.spt.TimeOutValue := 2;
	ipar.spt.DataBufferOffset := pansichar(@ipar.Buffer)-pansichar(@ipar);
	ipar.spt.SenseInfoOffset  := pansichar(@ipar.SenseBuf)-pansichar(@ipar);
  ipar.spt.Cdb[0] := $A1;
  ipar.spt.Cdb[1] := $8;
  ipar.spt.Cdb[2] := $E;
	ipar.spt.Cdb[3] := $D0;
  ipar.spt.Cdb[4] := $1;
  ipar.spt.Cdb[5] := $0;
  ipar.spt.Cdb[6] := $4F;
  ipar.spt.Cdb[7] := $C2;
  ipar.spt.Cdb[8] := $0;
	ipar.spt.Cdb[9] := $B0;

  fillchar(opar, SizeOf(opar), #0);

  hdrive := CreateFile(PChar(Harddisk), GENERIC_READ or GENERIC_WRITE,
                    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);

  If GetLastError = 0 Then
  begin
    Status := DeviceIoControl(hdrive, IOCTL_SCSI_PASS_THROUGH, @ipar, SizeOf(ipar), @ipar, SizeOf(ipar), dwBytesReturned, nil);
    if (status = false) and (fAlert = nil) and (getLastError <> 21) then
    begin
      TempInfo := TSSDInfo.Create;
      TempInfo.SetDeviceName(Copy(Harddisk, 5, Length(Harddisk) - 4));
      AlertCreate(nil, 'Failed Getting SMART : '+inttostr(getLastError()) + Chr(13) + Chr(10) +
                            'Drive : ' + Harddisk + Chr(13) + Chr(10) +
                            'Model : ' + TempInfo.Model + Chr(13) + Chr(10) +
                            'Firmware : ' + TempInfo.Firmware);
      FreeAndNil(TempInfo);
    end
    else if (status = false) and (getLastError = 21) then
      result := opar2;
    CloseHandle(hdrive);
  end
  else
  begin
      AlertCreate(nil, 'Failed Getting SMART : '+inttostr(getLastError()) + Chr(13) + Chr(10) +
                            'Drive : ' + Harddisk + Chr(13) + Chr(10) +
                            'And There is NO DRIVE');
  end;

  for CurrBuf := 0 to 511 do
    opar.bBuffer[CurrBuf] := ipar.Buffer[CurrBuf];
  Result := opar;
end;

function GetNTFSVolumeData(const DriveLetter: String): NTFS_INFO;
var
  hdrive: Cardinal;
  dwBytesReturned: DWORD;
  Status: Longbool;
  NTFSInfo: NTFS_VOLUME_DATA_BUFFER;
begin
  fillchar(NTFSInfo, SizeOf(NTFSInfo), #0);
  fillchar(result, SizeOf(result), #0);

  hdrive := CreateFile(PChar('\\.\' + DriveLetter), GENERIC_READ or GENERIC_WRITE,
                    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);

  If GetLastError = 0 Then
  begin
    Status := DeviceIoControl(hdrive, FSCTL_GET_NTFS_VOLUME_DATA, nil, 0, @NTFSInfo, SizeOf(NTFSInfo), dwBytesReturned, nil);
    if Status then
    begin
      result.SectorPerCluster := NTFSInfo.BytesPerCluster div NTFSInfo.BytesPerSector;
      result.FirstMFTStart := NTFSInfo.MftStartLcn;
      result.FirstMFTEnd.QuadPart := NTFSInfo.MftStartLcn.QuadPart + ceil(NTFSInfo.MftValidDataLength.QuadPart / NTFSInfo.BytesPerCluster);
      result.SecondMFTStart := NTFSInfo.Mft2StartLcn;
      result.SecondMFTEnd.QuadPart := NTFSInfo.Mft2StartLcn.QuadPart + ceil(NTFSInfo.MftValidDataLength.QuadPart / NTFSInfo.BytesPerCluster);
      result.MFTZoneStart := NTFSInfo.MftZoneStart;
      result.MFTZoneEnd := NTFSInfo.MftZoneEnd;
      result.ErrorCode := 0;
    end
    else
      result.ErrorCode := GetLastError;
    CloseHandle(hdrive);
  end;
end;

function GetVolumeLabel(DriveName: String): string;
var
  NotUsed:     DWORD;
  VolumeFlags: DWORD;
  VolumeSerialNumber: DWORD;
  Buf: array [0..MAX_PATH] of Char;
begin
    GetVolumeInformation(PChar(DriveName),
    Buf, SizeOf(Buf), @VolumeSerialNumber, NotUsed,
    VolumeFlags, nil, 0);

    if Buf[0] <> #0 then
      Result := DriveName + ' (' + Buf + ' - ' + IntToStr(DiskSize(Pos(DriveName[1], VolumeNames)) div 1024 div 1024) + 'MB)'
    else
      Result := DriveName + ' (이동식 디스크 - ' + IntToStr(DiskSize(Pos(DriveName[1], VolumeNames)) div 1024 div 1024) + 'MB)';
end;

function GetVolumeLabelFixed(DriveName: String): string;
var
  NotUsed:     DWORD;
  VolumeFlags: DWORD;
  VolumeSerialNumber: DWORD;
  Buf: array [0..MAX_PATH] of Char;
begin
  GetVolumeInformation(PChar(DriveName),
                      Buf, SizeOf(Buf), @VolumeSerialNumber, NotUsed,
                      VolumeFlags, nil, 0);

  if Buf[0] <> #0 then
  begin
    if DiskSize(Pos(DriveName[1], VolumeNames)) div 1024 div 1024 < 1024 then
      Result := DriveName + ' (' + Buf + ' - ' + IntToStr(round(DiskSize(Pos(DriveName[1], VolumeNames)) / 1024 / 1024)) + 'MB)'
    else if DiskSize(Pos(DriveName[1], VolumeNames)) div 1024 div 1024 < 1048576 then
      Result := DriveName + ' (' + Buf + ' - ' + IntToStr(round(DiskSize(Pos(DriveName[1], VolumeNames)) / 1024 / 1024 / 1024)) + 'GB)'
    else if DiskSize(Pos(DriveName[1], VolumeNames)) div 1024 div 1024 < 1073741824 then
      Result := DriveName + ' (' + Buf + ' - ' + IntToStr(round(DiskSize(Pos(DriveName[1], VolumeNames)) / 1024 / 1024 / 1024 / 1024)) + 'TB)';
  end
  else
  begin
    if DiskSize(Pos(DriveName[1], VolumeNames)) div 1024 div 1024 < 1024 then
      Result := DriveName + ' (로컬 디스크 - ' + IntToStr(round(DiskSize(Pos(DriveName[1], VolumeNames)) / 1024 / 1024)) + 'MB)'
    else if DiskSize(Pos(DriveName[1], VolumeNames)) div 1024 div 1024 < 1048576 then
      Result := DriveName + ' (로컬 디스크 - ' + IntToStr(round(DiskSize(Pos(DriveName[1], VolumeNames)) / 1024 / 1024 / 1024)) + 'GB)'
    else if DiskSize(Pos(DriveName[1], VolumeNames)) div 1024 div 1024 < 1073741824 then
      Result := DriveName + ' (로컬 디스크 - ' + IntToStr(round(DiskSize(Pos(DriveName[1], VolumeNames)) / 1024 / 1024 / 1024 / 1024)) + 'TB)';
  end;
end;

procedure GetUSBDrives(USBDrives: TStrings);
var
  CurrDrv, DriveCount: Integer;
  Drives: Array[0..255] of char;
  DrvName: String;
begin
  USBDrives.Clear;
  FillChar(Drives, 256, #0 );
  DriveCount := GetLogicalDriveStrings(256, Drives);
  for CurrDrv := 0 to DriveCount - 1 do
  begin
    if Drives[CurrDrv] = #0  then
    begin
      if GetDriveType(PChar(DrvName)) = DRIVE_REMOVABLE then
      begin
        SetErrorMode(SEM_FailCriticalErrors);
        CreateFile(PChar('\\.\' + Copy(DrvName, 1, 2)), GENERIC_READ,
                FILE_SHARE_WRITE or FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
        if GetLastError = 0 then
        begin
          if (DiskSize(Pos(DrvName[1], VolumeNames)) > 0) then
            USBDrives.Add(GetVolumeLabel(DrvName));
        end
        else
          AlertCreate(nil, IntToStr(GetLastError));
      end;
      DrvName := '';
    end
    else
      DrvName := DrvName + Drives[CurrDrv];
  end;
end;

procedure GetChildDrives(DiskNumber: String; ChildDrives: TStrings);
var
  CurrDrv, DriveCount: Integer;
  DrvNames: TDriveLetters;
begin
  ChildDrives.Clear;
  DrvNames := GetPartitionList(DiskNumber);
  DriveCount := Length(DrvNames.Letters);
  for CurrDrv := 0 to DriveCount - 1 do
    ChildDrives.Add(GetVolumeLabelFixed(DrvNames.Letters[CurrDrv + 1] + ':\'));
end;


function GetFixedDrivesFunction: String;
var
  CurrDrv, DriveCount: Integer;
  Drives: Array[0..255] of char;
  DrvName: String;
begin
  FillChar(Drives, 256, #0 );
  DriveCount := GetLogicalDriveStrings(256, Drives);
  for CurrDrv := 0 to DriveCount - 1 do
  begin
    if Drives[CurrDrv] = #0  then
    begin
      if GetDriveType(PChar(DrvName + ':\')) = DRIVE_FIXED then
        result := result + DrvName;
      DrvName := '';
    end
    else if (Drives[CurrDrv] <> ':') and (Drives[CurrDrv] <> '\') then
      DrvName := DrvName + Drives[CurrDrv];
  end;
end;

function GetDiskGeometry(const DiskNumber: String): DISK_GEOMETRY_EX;
Var
  RetBytes: DWORD;
  hDevice: Longint;
  Status: Longbool;
  TempInfo: TSSDInfo;
begin
  hDevice := CreateFile(PChar('\\.\PhysicalDrive' + DiskNumber), GENERIC_READ,
              FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  If hDevice <> -1 Then
  begin
    Status := DeviceIoControl(hDevice, IOCTL_DISK_GET_DRIVE_GEOMETRY_EX,
              nil, 0, @result, Sizeof(DISK_GEOMETRY_EX), RetBytes, nil);
    if (status = false) and (fAlert = nil) and (getLastError <> 21) then
    begin
      TempInfo := TSSDInfo.Create;
      TempInfo.SetDeviceName('PhysicalDrive' + DiskNumber);
      AlertCreate(nil, 'Failed Getting Geometry : '+inttostr(getLastError) + Chr(13) + Chr(10) +
                            'Drive : \\.\PhysicalDrive' + DiskNumber + Chr(13) + Chr(10) +
                            'Model : ' + TempInfo.Model + Chr(13) + Chr(10) +
                            'Firmware : ' + TempInfo.Firmware);
      FreeAndNil(TempInfo);
    end
    else if (status = false) and (getLastError = 21) then
      result.DiskSize := 0;
    CloseHandle(hDevice);
  end
  else
  begin
      AlertCreate(nil, 'Failed Getting Geometry : '+inttostr(getLastError) + Chr(13) + Chr(10) +
                            'Drive : \\.\PhysicalDrive' + DiskNumber + Chr(13) + Chr(10) +
                            'And There is NO DRIVE');
  end;
end;

function ReadNativeMaxAddress(DeviceName: String): UInt64;
var
  ICBuffer: ATA_PTH_BUFFER;
  hdrive: THandle;
  bResult: Boolean;
  BytesRead: Cardinal;
  CurrBuf: Integer;
begin
  bResult := false;
  FillChar(ICBuffer, SizeOf(ICBuffer), #0);

  hdrive := CreateFile(PChar('\\.\' + DeviceName), GENERIC_READ or GENERIC_WRITE,
                    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);

  If GetLastError = 0 Then
  begin
    ICBuffer.PTH.Length := SizeOf(ICBuffer.PTH);
    ICBuffer.PTH.AtaFlags := ATA_FLAGS_48BIT_COMMAND;
    ICBuffer.PTH.DataTransferLength := 512;
    ICBuffer.PTH.TimeOutValue := 2;
    ICBuffer.PTH.DataBufferOffset := PChar(@ICBuffer.Buffer) - PChar(@ICBuffer.PTH) + 20;

    ICBuffer.PTH.CurrentTaskFile[5] := $40;
    ICBuffer.PTH.CurrentTaskFile[6] := $27;

    bResult := DeviceIOControl(hdrive, IOCTL_ATA_PASS_THROUGH, @ICBuffer, SizeOf(ICBuffer), @ICBuffer, SizeOf(ICBuffer), BytesRead, nil);
    result := (ICBuffer.PTH.CurrentTaskFile[3]) + (ICBuffer.PTH.CurrentTaskFile[4] shl 8) +
              (ICBuffer.PTH.PreviousTaskFile[2] shl 16) + (ICBuffer.PTH.PreviousTaskFile[3] shl 24) + (ICBuffer.PTH.PreviousTaskFile[4] shl 32);
    result := result shl 8;
    result := result + ICBuffer.PTH.CurrentTaskFile[2];
    CloseHandle(hdrive);
  end;
end;

procedure LockDrive(DeviceName: String);
var
  ICBuffer: ATA_PTH_BUFFER;
  hdrive: THandle;
  bResult: Boolean;
  BytesRead: Cardinal;
  CurrBuf: Integer;
begin
  bResult := false;
  FillChar(ICBuffer, SizeOf(ICBuffer), #0);

  hdrive := CreateFile(PChar('\\.\' + DeviceName), GENERIC_READ or GENERIC_WRITE,
                    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);

  If GetLastError = 0 Then
  begin
    ICBuffer.PTH.Length := SizeOf(ICBuffer.PTH);
    ICBuffer.PTH.AtaFlags := ATA_FLAGS_DATA_OUT;
    ICBuffer.PTH.DataTransferLength := 512;
    ICBuffer.PTH.TimeOutValue := 2;
    ICBuffer.PTH.DataBufferOffset := PChar(@ICBuffer.Buffer) - PChar(@ICBuffer.PTH) + 20;

    ICBuffer.PTH.CurrentTaskFile[0] := 1;

    ICBuffer.PTH.CurrentTaskFile[6] := $F9;

    bResult := DeviceIOControl(hdrive, IOCTL_ATA_PASS_THROUGH, @ICBuffer, SizeOf(ICBuffer), @ICBuffer, SizeOf(ICBuffer), BytesRead, nil);
    CloseHandle(hdrive);
  end;
end;

procedure UnlockDrive(DeviceName: String; MaxLBA: UInt64);
var
  ICBuffer: ATA_PTH_BUFFER;
  hdrive: THandle;
  bResult: Boolean;
  BytesRead: Cardinal;
  CurrBuf: Integer;
begin
  bResult := false;
  FillChar(ICBuffer, SizeOf(ICBuffer), #0);

  hdrive := CreateFile(PChar('\\.\' + DeviceName), GENERIC_READ or GENERIC_WRITE,
                    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);

  If GetLastError = 0 Then
  begin
    ICBuffer.PTH.Length := SizeOf(ICBuffer.PTH);
    ICBuffer.PTH.AtaFlags := ATA_FLAGS_DATA_OUT;
    ICBuffer.PTH.DataTransferLength := 512;
    ICBuffer.PTH.TimeOutValue := 2;
    ICBuffer.PTH.DataBufferOffset := PChar(@ICBuffer.Buffer) - PChar(@ICBuffer.PTH) + 20;

    ICBuffer.PTH.CurrentTaskFile[0] := 3;

    ICBuffer.PTH.CurrentTaskFile[6] := $F9;

    bResult := DeviceIOControl(hdrive, IOCTL_ATA_PASS_THROUGH, @ICBuffer, SizeOf(ICBuffer), @ICBuffer, SizeOf(ICBuffer), BytesRead, nil);
    CloseHandle(hdrive);
  end;
end;

function SetMaxAddress(DeviceName: String; MaxLBA: UInt64): Boolean;
var
  ICBuffer: ATA_PTH_BUFFER;
  hdrive: THandle;
  BytesRead: Cardinal;
  CurrBuf: Integer;
begin
  result := false;
  FillChar(ICBuffer, SizeOf(ICBuffer), #0);

  hdrive := CreateFile(PChar('\\.\' + DeviceName), GENERIC_READ or GENERIC_WRITE,
                    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);

  If GetLastError = 0 Then
  begin
    ICBuffer.PTH.Length := SizeOf(ICBuffer.PTH);
    ICBuffer.PTH.AtaFlags := ATA_FLAGS_48BIT_COMMAND;
    ICBuffer.PTH.DataTransferLength := 512;
    ICBuffer.PTH.TimeOutValue := 2;
    ICBuffer.PTH.DataBufferOffset := PChar(@ICBuffer.Buffer) - PChar(@ICBuffer.PTH) + 20;

    ICBuffer.PTH.CurrentTaskFile[2] := MaxLBA and 255;
    MaxLBA := MaxLBA shr 8;
    ICBuffer.PTH.CurrentTaskFile[3] := MaxLBA and 255;
    MaxLBA := MaxLBA shr 8;
    ICBuffer.PTH.CurrentTaskFile[4] := MaxLBA and 255;
    MaxLBA := MaxLBA shr 8;
    ICBuffer.PTH.PreviousTaskFile[2] := MaxLBA and 255;
    MaxLBA := MaxLBA shr 8;
    ICBuffer.PTH.PreviousTaskFile[3] := MaxLBA and 255;
    MaxLBA := MaxLBA shr 8;
    ICBuffer.PTH.PreviousTaskFile[4] := MaxLBA and 255;
    MaxLBA := MaxLBA shr 8;

    ICBuffer.PTH.CurrentTaskFile[5] := $50;
    ICBuffer.PTH.CurrentTaskFile[6] := $37;

    ReadNativeMaxAddress(DeviceName);
    result := DeviceIOControl(hdrive, IOCTL_ATA_PASS_THROUGH, @ICBuffer, SizeOf(ICBuffer), @ICBuffer, SizeOf(ICBuffer), BytesRead, nil);
    CloseHandle(hdrive);
    if ICBuffer.PTH.CurrentTaskFile[0] <> 0 then
      result := false;
  end;
end;

function GetDiskSize(const DiskNumber: String): TLargeInteger;
Var
  Geometry: DISK_GEOMETRY_EX;
begin
  Geometry := GetDiskGeometry(DiskNumber);
  //실제로 수는 다르므로 확인작업
  result := Geometry.DiskSize;
end;

function GetMotherDrive(const DriveLetter: String): VOLUME_DISK_EXTENTS;
var
  RetBytes: DWORD;
  hDevice: Longint;
  Status: Longbool;
  VolumeName: Array[0..MAX_PATH] of Char;
  i: Integer;
begin
  for i := 0 to MAX_PATH do
    VolumeName[i] := #0;
  QueryDosDeviceW(PChar(DriveLetter), VolumeName, MAX_PATH);
  try
    hDevice := CreateFile(PChar('\\.\' + DriveLetter), GENERIC_READ,
                FILE_SHARE_WRITE or FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
  except
    exit;
  end;
  if Pos('ramdriv', lowercase(VolumeName)) > 0 then
    result.NumberOfDiskExtents := 0
  else
  begin
    If hDevice <> -1 Then
    begin
      Status := DeviceIoControl (hDevice, IOCTL_VOLUME_GET_VOLUME_DISK_EXTENTS,
                nil, 0, @result, Sizeof(VOLUME_DISK_EXTENTS), RetBytes, nil);
      if (status = false) then
      begin
        result.NumberOfDiskExtents := 0;
      end;
      CloseHandle(hDevice);
    end
    else
    begin
      result.NumberOfDiskExtents := 0;
    end;
  end;
end;

function GetPartitionList(DiskNumber: String): TDriveLetters;
var
  CurrDrv, CurrExtents: Integer;
  CurrDrvInfo: VOLUME_DISK_EXTENTS;
  CurrPartition, DiskNumberInt: Integer;
begin
  AllDrives := GetFixedDrivesFunction;
  CurrPartition := 0;
  DiskNumberInt := StrToInt(DiskNumber);
  result.Letters := '';
  for CurrDrv := 1 to Length(AllDrives) do
  begin
    CurrDrvInfo := GetMotherDrive(AllDrives[CurrDrv] + ':');
    for CurrExtents := 0 to (CurrDrvInfo.NumberOfDiskExtents - 1) do
    begin
      if CurrDrvInfo.Extents[CurrExtents].DiskNumber = DiskNumberInt then
      begin
        result.Letters := result.Letters + AllDrives[CurrDrv];
        result.StartOffset[CurrPartition] :=
          CurrDrvInfo.Extents[CurrExtents].StartingOffset;
        Inc(CurrPartition);
      end;
    end;
  end;
end;

function GetPartitionLength(DriveLetter: String): Int64;
var
  RetBytes: DWORD;
  hDevice: Longint;
  Status: Longbool;
  VolumeName: Array[0..MAX_PATH] of Char;
  i: Integer;
  TempResult: VOLUME_DISK_EXTENTS;
begin
  result := 0;
  for i := 0 to MAX_PATH do
    VolumeName[i] := #0;
  QueryDosDeviceW(PChar(DriveLetter), VolumeName, MAX_PATH);
  try
    hDevice := CreateFile(PChar('\\.\' + DriveLetter), GENERIC_READ,
                FILE_SHARE_WRITE or FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
  except
    exit;
  end;
  if Pos('ramdriv', lowercase(VolumeName)) > 0 then
    TempResult.NumberOfDiskExtents := 0
  else
  begin
    If hDevice <> -1 Then
    begin
      Status := DeviceIoControl (hDevice, IOCTL_VOLUME_GET_VOLUME_DISK_EXTENTS,
                nil, 0, @TempResult, Sizeof(VOLUME_DISK_EXTENTS), RetBytes, nil);
      if (status = false) then
      begin
        TempResult.NumberOfDiskExtents := 0;
      end;
      result := TempResult.Extents[0].ExtentLength;
      CloseHandle(hDevice);
    end
    else
    begin
      TempResult.NumberOfDiskExtents := 0;
    end;
  end;
end;

function GetFirstSector(const DriveLetter: String): Int64;
var
  hdrive: Cardinal;
  dwBytesReturned: DWORD;
  Status: Longbool;
  FirstSectorInfo: RETRIEVAL_POINTER_BASE;
begin
  fillchar(result, SizeOf(result), #0);

  hdrive := CreateFile(PChar('\\.\' + DriveLetter), GENERIC_READ or GENERIC_WRITE,
                    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);

  If GetLastError = 0 Then
  begin
    Status := DeviceIoControl(hdrive, FSCTL_GET_RETRIEVAL_POINTER_BASE, nil, 0, @FirstSectorInfo, SizeOf(RETRIEVAL_POINTER_BASE), dwBytesReturned, nil);
    if Status then
    begin
      result := FirstSectorInfo.FileAreaOffset.QuadPart;
    end
    else
      result := 0;
    CloseHandle(hdrive);
  end;
end;

function WriteBufferCheck: TStringList;
var
  DevicesList, SerialsList: TStringList;
  ValueList: TStringList;
  CurrDev, CurrSer: Integer;
begin
  result := TStringList.Create;
  DevicesList := TStringList.Create;
  SerialsList := TStringList.Create;
  ValueList := TStringList.Create;
  GetKeyList('LM', 'SYSTEM\CurrentControlSet\Enum\IDE', DevicesList);
  for CurrDev := 0 to DevicesList.Count - 1 do
  begin
    if ((Pos('SAMSUNG', DevicesList[CurrDev]) > 0) and (Pos('SSD', DevicesList[CurrDev]) > 0)) or
        (Pos('LITEONIT', DevicesList[CurrDev]) > 0) or
        ((Pos('PLEXTOR', DevicesList[CurrDev]) > 0) and (Pos('PX', DevicesList[CurrDev]) > 0)) or
        ((Pos('MXSSD', DevicesList[CurrDev]) > 0) and (Pos('JT', DevicesList[CurrDev]) > 0)) or
        ((Pos('MXSSD', DevicesList[CurrDev]) > 0) and (Pos('MMY', DevicesList[CurrDev]) > 0)) then
    begin
      GetKeyList('LM', 'SYSTEM\CurrentControlSet\Enum\IDE\' + DevicesList[CurrDev], SerialsList);
      for CurrSer := 0 to SerialsList.Count - 1 do
      begin
        GetValueList('LM', 'SYSTEM\CurrentControlSet\Enum\IDE\' + DevicesList[CurrDev] + '\' + SerialsList[CurrSer] +
                            '\Device Parameters\Disk', ValueList);
        if GetRegInt('LM', 'SYSTEM\CurrentControlSet\Enum\IDE\' + DevicesList[CurrDev] + '\' + SerialsList[CurrSer] +
                            '\Device Parameters\Disk', 'UserWriteCacheSetting') = 0 then
        begin
          SetRegInt('LM', 'SYSTEM\CurrentControlSet\Enum\IDE\' + DevicesList[CurrDev] + '\' + SerialsList[CurrSer] +
                              '\Device Parameters\Disk', 'UserWriteCacheSetting', 1);
          result.Add(GetRegStr('LM', 'SYSTEM\CurrentControlSet\Enum\IDE\' + DevicesList[CurrDev] + '\' + SerialsList[CurrSer]
                    , 'FriendlyName'));
        end;
      end;
    end;
  end;
  GetKeyList('LM', 'SYSTEM\CurrentControlSet\Enum\SCSI', DevicesList);
  for CurrDev := 0 to DevicesList.Count - 1 do
  begin
    if ((Pos('SAMSUNG', DevicesList[CurrDev]) > 0) and (Pos('SSD', DevicesList[CurrDev]) > 0)) or
        (Pos('LITEONIT', DevicesList[CurrDev]) > 0) or
        ((Pos('PLEXTOR', DevicesList[CurrDev]) > 0) and (Pos('PX', DevicesList[CurrDev]) > 0)) or
        ((Pos('MXSSD', DevicesList[CurrDev]) > 0) and (Pos('JT', DevicesList[CurrDev]) > 0)) or
        ((Pos('MXSSD', DevicesList[CurrDev]) > 0) and (Pos('MMY', DevicesList[CurrDev]) > 0)) then
    begin
      GetKeyList('LM', 'SYSTEM\CurrentControlSet\Enum\SCSI\' + DevicesList[CurrDev], SerialsList);
      for CurrSer := 0 to SerialsList.Count - 1 do
      begin
        GetValueList('LM', 'SYSTEM\CurrentControlSet\Enum\SCSI\' + DevicesList[CurrDev] + '\' + SerialsList[CurrSer] +
                            '\Device Parameters\Disk', ValueList);
        if GetRegInt('LM', 'SYSTEM\CurrentControlSet\Enum\SCSI\' + DevicesList[CurrDev] + '\' + SerialsList[CurrSer] +
                            '\Device Parameters\Disk', 'UserWriteCacheSetting') = 0 then
        begin
          SetRegInt('LM', 'SYSTEM\CurrentControlSet\Enum\SCSI\' + DevicesList[CurrDev] + '\' + SerialsList[CurrSer] +
                              '\Device Parameters\Disk', 'UserWriteCacheSetting', 1);
          result.Add(GetRegStr('LM', 'SYSTEM\CurrentControlSet\Enum\SCSI\' + DevicesList[CurrDev] + '\' + SerialsList[CurrSer]
                    , 'FriendlyName'));
        end;
      end;
    end;
  end;
  FreeAndNil(DevicesList);
  FreeAndNil(SerialsList);
  FreeAndNil(ValueList);
end;

function ExtractDrvNum(const Input: String): String;
begin
  if Input[1] = '\' then result := Copy(Input, 18, Length(Input) - 17)
  else if Input[1] = 'P' then result := Copy(Input, 14, Length(Input) - 13);
end;

function GetSSDList: TStringList;
var
  i: Integer;
  iValue, iValue2: LongWord;
  wsFileObj: WideString;
  OleDrives, OleDrives2: OleVariant;
  Dispatch: IDispatch;
  OleDrivesVar, OleDrivesVar2: OleVariant;
  OleEnum, OleEnum2: IEnumvariant;
  OleCtx: IBindCtx;
  OleMoniker: IMoniker;
  ATAList, SCSIList: TStringList;
begin
  wsFileObj := 'winmgmts:\\localhost\root\cimv2';
  ATAList := TStringList.Create;
  SCSIList := TStringList.Create;
  WMIEnabled := true;
  try
    OleCheck(CreateBindCtx(0, OleCtx));
    OleCheck(MkParseDisplayName(OleCtx, PWideChar(wsFileObj), i, OleMoniker));
    OleCheck(OleMoniker.BindToObject(OleCtx, nil, IUnknown, Dispatch));


    OleDrivesVar := OleVariant(Dispatch).ExecQuery('Select * from Win32_DiskDrive');
    OleEnum := IUnknown(OleDrivesVar._NewEnum) as IEnumVariant;


    while OleEnum.Next(1, OleDrives, iValue) = 0 do
    begin
      if (not VarIsNull(OleDrives.DeviceID <> '')) and (OleDrives.MediaLoaded) and (not VarIsNull(OleDrives.MediaType))then
      begin
        if Pos('hard', Lowercase(OleDrives.MediaType)) >= 0 then
          if OleDrives.InterfaceType = 'IDE' then
            ATAList.Add(OleDrives.DeviceID)
          else if OleDrives.InterfaceType = 'USB' then
            SCSIList.Add(OleDrives.DeviceID + 'U')
          else if OleDrives.InterfaceType = 'SCSI' then
            SCSIList.Add(OleDrives.DeviceID + 'H');
      end;
      OleDrives := Unassigned;
    end;
    OleDrivesVar :=  Unassigned;
  except
    on e: Exception do
    begin
      ATAList.Add(e.Message);
      ATAList.SaveToFile('C:\NSTwmierror.txt');
      ATAList.Clear;
      WMIEnabled := false;
    end;
  end;
  for i := 0 to ATAList.Count - 1 do
    ATAList[i] := ExtractDrvNum(ATAList[i]);
  for i := 0 to SCSIList.Count - 1 do
    SCSIList[i] := ExtractDrvNum(SCSIList[i]);
  result := TStringList.Create;
  result.AddStrings(ATAList);
  result.Add('/');
  result.AddStrings(SCSIList);
  FreeAndNil(ATAList);
  FreeAndNil(SCSIList);
end;

function OpenProcWithOutput(Path: String; Command: String): AnsiString;
var
  start: TStartupInfo;
  sec: TSecurityAttributes;
  pinfo: TProcessInformation;
  hwrite, hread: THandle;
  BytesRead: DWORD;
  Buffer: array[0..512] of Ansichar;
  ResultString: AnsiString;
  PathW, CommandW: WideString;
begin
  sec.nLength := sizeof(sec);
  sec.lpSecurityDescriptor := nil;
  sec.bInheritHandle := true;
  if CreatePipe(hread, hwrite, @sec, 0)<>true then
  begin
    exit;
  end;
  FillChar(start, sizeof(STARTUPINFO), 0);
  start.cb := sizeof(STARTUPINFO);
  start.wShowWindow := SW_HIDE;
  start.dwFlags := STARTF_USESHOWWINDOW;
  start.dwFlags := start.dwFlags or STARTF_USESTDHANDLES;
  start.hStdOutput := hwrite;
  start.hStdError := hwrite;
  PathW := Path;
  CommandW := Command;
  if not CreateProcess(nil, PWideChar(CommandW), nil, nil, True, 0, nil, PWideChar(PathW), start, pinfo) then ShowMessage('Error!');
  CloseHandle(hwrite);
  while ReadFile(hread, Buffer, Length(buffer)-1, BytesRead, nil) and (BytesRead>0) do
  begin
    Buffer[BytesRead] := #0;
    ResultString := ResultString + Buffer;
  end;
  CloseHandle(hread);
  Result := ResultString;
end;
end.
