unit Getter.TrimBasics.NTFS;

interface

uses
  Windows,
  OSFile.IoControl, Getter.TrimBasics;

type
  TNTFSTrimBasicsGetter = class(TTrimBasicsGetter)
  public
    function IsPartitionMyResponsibility: Boolean; override;
    function GetTrimBasicsToInitialize: TTrimBasicsToInitialize; override;
  private
    type
      TNTFSVolumeDataBuffer = record
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
  private
    InnerBuffer: TNTFSVolumeDataBuffer;
    function GetLBAPerCluster: Cardinal;
    function GetIOBuffer: TIoControlIOBuffer;
  end;

implementation

{ TNTFSTrimBasicsGetter }

function TNTFSTrimBasicsGetter.IsPartitionMyResponsibility: Boolean;
begin
  result := GetFileSystemName = 'NTFS';
end;

function TNTFSTrimBasicsGetter.GetTrimBasicsToInitialize:
  TTrimBasicsToInitialize;
begin
  result := inherited;
  result.PaddingLBA := 0;
  result.LBAPerCluster := GetLBAPerCluster;
end;

function TNTFSTrimBasicsGetter.GetIOBuffer: TIoControlIOBuffer;
begin
  result.InputBuffer.Buffer := nil;
  result.InputBuffer.Size := 0;

  result.OutputBuffer.Buffer := @InnerBuffer;
  result.OutputBuffer.Size := SizeOf(InnerBuffer);
end;

function TNTFSTrimBasicsGetter.GetLBAPerCluster: Cardinal;
begin
  IoControl(TIoControlCode.GetNTFSVolumeData, GetIOBuffer);
  result := InnerBuffer.BytesPerCluster div InnerBuffer.BytesPerSector;
end;

end.
