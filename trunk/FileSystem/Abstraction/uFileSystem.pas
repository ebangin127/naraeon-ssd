unit uFileSystem;

interface


//volume bitmap이랑 Partition Size랑 다 밑으로 내려버려야 할듯.
//매우 곤란함
uses
  Windows,
  uIoControlFile;

type
  TPartitionInformationForDataSetManagement = record
    StartPaddingInLBA: UInt64;
    LBAPerCluster: Cardinal;
  end;

  TFileSystem = class abstract(TIoControlFile)
  private
    
  
  protected
    function GetMinimumPrivilege: TCreateFileDesiredAccess;
    
  public
    function GetPartitionSizeInCluster: UInt64;
    function GetPartitionInformationForDataSetManagement:
      TPartitionInformationForDataSetManagement; virtual; abstract;
  end;

implementation


  DeviceIoControl(
    PartHandle,
    FSCTL_GET_VOLUME_BITMAP,
    @StartingBuffer, SizeOf(STARTING_LCN_INPUT_BUFFER),
    @BitmapBuffer, VOLUME_BITMAP_BYTES,
    BytesRead, nil);

function TFileSystem.GetPartitionSizeInCluster: UInt64;
begin
  
end;

function GetMinimumPrivilege: TCreateFileDesiredAccess;
begin
  result := DesiredReadWrite;
end;

end.
