unit Getter.TrimBasics;

interface

uses
  SysUtils, Windows,
  OSFile, OSFile.IoControl, OSFile.Handle,
  Partition, Getter.PartitionExtent, Getter.Filesystem.Name;

type
  TTrimBasicsToInitialize = record
    PaddingLBA: Integer;
    LBAPerCluster: Cardinal;
    StartLBA: UInt64;
  end;
  EUnknownPartition = class(Exception);

  TTrimBasicsGetter = class abstract(TIoControlFile)
  private
    Handle: THandle;
    FFileSystemName: String;
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
    procedure SetHandle(const Handle: THandle);
    procedure SetFileSystemName(const FileSystemName: String);
  end;
  
implementation

{ TTrimBasicsGetter }

constructor TTrimBasicsGetter.Create(const FileToGetAccess: String);
begin
  inherited;
  try
    CreateHandle(FileToGetAccess, DesiredReadOnly);
  except
    on E: EOSError do;
    else raise;
  end;
end;

function TTrimBasicsGetter.GetFileSystemName: String;
var
  FileSystemNameGetter: TFileSystemNameGetter;
begin
  if FFileSystemName <> '' then
    exit(FFileSystemName);
  FileSystemNameGetter := TFileSystemNameGetter.Create(GetPathOfFileAccessing);
  try
    result := FileSystemNameGetter.GetFileSystemName;
  finally
    FreeAndNil(FileSystemNameGetter);
  end;
end;

function TTrimBasicsGetter.GetMinimumPrivilege: TCreateFileDesiredAccess;
begin
  result := TCreateFileDesiredAccess.DesiredReadOnly;
end;

function TTrimBasicsGetter.GetStartLBA: UInt64;
var
  PartitionExtentGetter: TPartitionExtentGetter;
  PartitionExtentList: TPartitionExtentList;
begin
  PartitionExtentGetter :=
    TPartitionExtentGetter.Create(GetPathOfFileAccessing);
  try
    PartitionExtentList := PartitionExtentGetter.GetPartitionExtentList;
    result := PartitionExtentList[0].StartingOffset div BytePerLBA;
    FreeAndNil(PartitionExtentList);
  finally
    FreeAndNil(PartitionExtentGetter);
  end;
end;

function TTrimBasicsGetter.GetTrimBasicsToInitialize: TTrimBasicsToInitialize;
begin
  result.StartLBA := GetStartLBA;
end;

procedure TTrimBasicsGetter.SetFileSystemName(const FileSystemName: String);
begin
  FFileSystemName := FileSystemName;
end;

procedure TTrimBasicsGetter.SetHandle(const Handle: THandle);
begin
  self.Handle := Handle;
end;

end.
