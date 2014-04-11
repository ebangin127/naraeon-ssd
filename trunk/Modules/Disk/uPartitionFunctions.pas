unit uPartitionFunctions;

interface

uses Windows, Math, SysUtils,
      uLanguageSettings;

type
  //---GetNTFSVolumeData---//
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
  //---GetNTFSVolumeData---//

  //---GetDiskGeometry---//
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
  //---GetDiskGeometry---//

//디스크 용량 받아오는 함수
function GetDiskGeometry(const DiskNumber: String): DISK_GEOMETRY_EX;
function GetDiskSize(const DiskNumber: String): TLargeInteger;

//파티션 용량 받아오는 함수
function GetPartitionLength(DriveLetter: String): Int64;
function GetNTFSVolumeData(const DriveLetter: String): NTFS_INFO;
function GetVolumeLabel(DriveName: String): string;
function GetFirstSector(const DriveLetter: String): Int64;

implementation

uses uDiskFunctions;

function GetDiskGeometry(const DiskNumber: String): DISK_GEOMETRY_EX;
Var
  RetBytes: DWORD;
  hDevice: Longint;
  Status: Longbool;
begin
  hDevice := CreateFile(PChar('\\.\PhysicalDrive' + DiskNumber), GENERIC_READ,
              FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  If hDevice <> -1 Then
  begin
    Status := DeviceIoControl(hDevice, IOCTL_DISK_GET_DRIVE_GEOMETRY_EX,
              nil, 0, @result, Sizeof(DISK_GEOMETRY_EX), RetBytes, nil);
    if status = false then
      result.DiskSize := 0;
    CloseHandle(hDevice);
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
    GetVolumeInformation(PChar(DriveName), Buf, SizeOf(Buf), @VolumeSerialNumber, NotUsed, VolumeFlags, nil, 0);

    if Buf[0] <> #0 then
      Result := DriveName + ' (' + Buf + ' - ' + IntToStr(DiskSize(Pos(DriveName[1], VolumeNames)) div 1024 div 1024) + 'MB)'
    else
      Result := DriveName + ' (' + CapRemvDisk[CurrLang] + ' - ' + IntToStr(DiskSize(Pos(DriveName[1], VolumeNames)) div 1024 div 1024) + 'MB)';
end;

function GetDiskSize(const DiskNumber: String): TLargeInteger;
var
  Geometry: DISK_GEOMETRY_EX;
begin
  Geometry := GetDiskGeometry(DiskNumber);
  result := Geometry.DiskSize;
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

end.
