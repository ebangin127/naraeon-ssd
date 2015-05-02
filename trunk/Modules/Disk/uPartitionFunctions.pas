unit uPartitionFunctions;

interface

uses Windows, Math, SysUtils;

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

//��Ƽ�� �뷮 �޾ƿ��� �Լ�
function GetNTFSVolumeData(const DriveLetter: String): NTFS_INFO;

implementation

uses uDiskFunctions;

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

end.
