unit Getter.TrimBasics.FAT;

interface

uses
  SysUtils, Windows,
  Partition, Getter.TrimBasics, Getter.VolumeBitmap;

type
  TFATTrimBasicsGetter = class(TTrimBasicsGetter)
  public
    function IsPartitionMyResponsibility: Boolean; override;
    function GetTrimBasicsToInitialize: TTrimBasicsToInitialize; override;

  private
    function GetLBAPerCluster: Cardinal;
    function GetPartitionSizeCoveredByBitmapInCluster: UInt64;
    function GetPartitionSizeInByte: UInt64;
    function GetPaddingLBA(LBAPerCluster: Integer): Integer;
  end;

implementation

{ TFATTrimBasicsGetter }

function TFATTrimBasicsGetter.IsPartitionMyResponsibility: Boolean;
const
  FromFirst = 1;
  NotFound = 0;
begin
  result := Pos('FAT', GetFileSystemName, FromFirst) > NotFound;
end;

function TFATTrimBasicsGetter.GetTrimBasicsToInitialize:
  TTrimBasicsToInitialize;
begin
  result := inherited;
  result.LBAPerCluster := GetLBAPerCluster;
  result.PaddingLBA := GetPaddingLBA(result.LBAPerCluster);
end;

function TFATTrimBasicsGetter.GetPartitionSizeCoveredByBitmapInCluster: UInt64;
var
  VolumeBitmapGetter: TVolumeBitmapGetter;
  FirstPointLCN: LARGE_INTEGER;
begin
  VolumeBitmapGetter := TVolumeBitmapGetter.Create(GetPathOfFileAccessing);
  FirstPointLCN.QuadPart := 0;
  result :=
    VolumeBitmapGetter.GetVolumeBitmap(FirstPointLCN).PositionSize.
    BitmapSize.QuadPart;
  FreeAndNil(VolumeBitmapGetter);
end;

function TFATTrimBasicsGetter.GetPartitionSizeInByte: UInt64;
var
  Partition: TPartition;
begin
  Partition := TPartition.Create(GetPathOfFileAccessing);
  result := Partition.PartitionLengthInByte;
  FreeAndNil(Partition);
end;

function TFATTrimBasicsGetter.GetLBAPerCluster: Cardinal;
var
  NotUsed: Array[0..2] of Cardinal;
begin
  GetDiskFreeSpace(PChar(GetPathOfFileAccessing + '\'), result,
    NotUsed[0], NotUsed[1], NotUsed[2]);
end;

function TFATTrimBasicsGetter.GetPaddingLBA(LBAPerCluster: Integer): Integer;
var
  PartitionSizeInLBA: UInt64;
  PartitionSizeCoveredByBitmapInLBA: UInt64;
begin
  PartitionSizeInLBA :=
    GetPartitionSizeInByte div BytePerLBA;
  PartitionSizeCoveredByBitmapInLBA :=
    GetPartitionSizeCoveredByBitmapInCluster * LBAPerCluster;
  result := PartitionSizeInLBA - PartitionSizeCoveredByBitmapInLBA;
end;
end.
