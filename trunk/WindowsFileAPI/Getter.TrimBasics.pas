unit Getter.TrimBasics;

interface

uses
  SysUtils, Windows,
  OSFile, OSFile.IoControl, OSFile.Handle,
  Partition, Getter.PartitionExtent;

type
  TTrimBasicsToInitialize = record
    PaddingLBA: Integer;
    LBAPerCluster: Cardinal;
    StartLBA: UInt64;
  end;
  EUnknownPartition = class(Exception);

  TTrimBasicsGetter = class abstract(TIoControlFile)
  private
    FileSystemNameInCharArray: Array[0..MAX_PATH - 1] of Char;
    function GetStartLBA: UInt64;
  protected
    function GetFileSystemName: String;
    function GetMinimumPrivilege: TCreateFileDesiredAccess; override;
    const
      BytePerLBA = 512;
  public
    constructor Create(const FileToGetAccess: String); override;
    function IsPartitionMyResponsibility: Boolean; virtual; abstract;
    function GetTrimBasicsToInitialize: TTrimBasicsToInitialize;
      virtual;
  end;
  
implementation

{ TTrimBasicsGetter }

function TTrimBasicsGetter.GetFileSystemName: String;
var
  Useless: DWORD;
  PathToGetFileSystemName: String;
begin
  Useless := 0;
  PathToGetFileSystemName := GetPathOfFileAccessing + '\';
  GetVolumeInformation(PChar(PathToGetFileSystemName), nil, 0, nil, Useless,
    Useless, FileSystemNameInCharArray, SizeOf(FileSystemNameInCharArray));
  result := PChar(@FileSystemNameInCharArray[0]);
end;

function TTrimBasicsGetter.GetMinimumPrivilege: TCreateFileDesiredAccess;
begin
  result := TCreateFileDesiredAccess.DesiredReadOnly;
end;

function TTrimBasicsGetter.GetStartLBA: UInt64;
var
  Partition: TPartition;
  PartitionExtentList: TPartitionExtentList;
begin
  Partition := TPartition.Create(GetPathOfFileAccessing);
  PartitionExtentList := Partition.GetPartitionExtentList;
  result := PartitionExtentList[0].StartingOffset div BytePerLBA;
  FreeAndNil(PartitionExtentList);
  FreeAndNil(Partition);
end;

function TTrimBasicsGetter.GetTrimBasicsToInitialize: TTrimBasicsToInitialize;
begin
  result.StartLBA := GetStartLBA;
end;

constructor TTrimBasicsGetter.Create(const FileToGetAccess: String);
begin
  CreateHandle(FileToGetAccess, DesiredReadOnly);
end;

end.