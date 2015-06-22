unit uNTFSFileSystem;

interface

uses
  Windows,
  uIoControlFile, uFileSystem;

type
  TNTFSFileSystem = class(TFileSystem)
  public
    function GetPartitionInformationForDataSetManagement:
      TPartitionInformationForDataSetManagement;
  end;

implementation

function TNTFSFileSystem.GetPartitionInformationForDataSetManagement:
  TPartitionInformationForDataSetManagement;
begin
  result.StartPadding := 0;
  result.LBAPerCluster := 0;
end;

end.
