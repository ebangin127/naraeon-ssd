unit uFATFileSystem;

interface

uses
  Windows, 
  uOSFile, uFileSystem;

type
  TFATFileSystem = class(TFileSystem)
  private
    LBAPerCluster: UInt64;
    FATLength: Cardinal;
  public
    function GetPartitionInformationForDataSetManagement:
      TPartitionInformationForDataSetManagement;
  end;

implementation

procedure TFATFileSystem.SetFATLength;
begin
  FATLength :=
    (GetPartitionLength(DriveLetter) -
      (PartSize * LBAPerCluster * LBASize))
    div LBASize;
end;

procedure TFATFileSystem.SetLBAPerCluster;
var
  NotUsed: Array[0..2] of Cardinal;
begin
  GetDiskFreeSpace(PChar(DriveLetter + '\'), result,
    NotUsed[0], NotUsed[1], NotUsed[2]);
  IfOSErrorRaiseException;
end;

function TFATFileSystem.GetPartitionInformationForDataSetManagement:
  TPartitionInformationForDataSetManagement;
begin
  SetLBAPerCluster;
  SetFATLength;

  result.StartPadding := FATLength;
  result.LBAPerCluster := LBAPerCluster;
end;
  
end.
