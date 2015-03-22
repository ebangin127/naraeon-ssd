unit uNTFSStruct;

interface

uses
  Windows;

type
  NTFSTranslated = record
    SectorPerCluster: Integer;
    FirstMFTStart: TLargeInteger;
    FirstMFTEnd: TLargeInteger;
    SecondMFTStart: TLargeInteger;
    SecondMFTEnd: TLargeInteger;
    MFTZoneStart: TLargeInteger;
    MFTZoneEnd: TLargeInteger;
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

implementation

end.
