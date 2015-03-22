unit uPartition;

interface

uses
  Generics.Collections;

type
  TMotherDriveList = TList<Integer>;

  TPartitionInfo = record
    BitmapStartPaddingLBA: Int64;
    LBAPerCluster: Cardinal;
  end;

  TPartition = class
    constructor Create;
    destructor Destroy;

  public
    function GetPartitionInfo: TPartitionInfo; virtual; abstract;
    function GetMotherDrive: TMotherDriveList;
    function GetPartitionLength: Int64;

  private
    DriveHandle: THandle;
  end;

implementation

{ TPartition }

constructor TPartition.Create;
begin
  CreateFile(PChar('\\.\' + VolumeToGet), GENERIC_READ,
    FILE_SHARE_WRITE or FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
end;

destructor TPartition.Destroy;
begin

end;

function TPartition.GetMotherDrive: TMotherDriveList;
begin

end;

function TPartition.GetPartitionLength: Int64;
begin

end;

function GetMotherDrive(const VolumeToGet: String): VOLUME_DISK_EXTENTS;
var
  RetBytes: DWORD;
  hDevice: Longint;
  Status: Longbool;
  VolumeName: Array[0..MAX_PATH] of Char;
  i: Integer;
begin
  for i := 0 to MAX_PATH do
    VolumeName[i] := #0;
  QueryDosDeviceW(PChar(VolumeToGet), VolumeName, MAX_PATH);
  try
    hDevice := CreateFile(PChar('\\.\' + VolumeToGet), GENERIC_READ,
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

{function GetNTFSVolumeData(const DriveLetter: String): NTFS_INFO;
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
end;   }

end.
