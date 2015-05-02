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

function TFATFileSystem.GetPartitionInformationForDataSetManagement:
  TPartitionInformationForDataSetManagement;
begin
end;

end.
