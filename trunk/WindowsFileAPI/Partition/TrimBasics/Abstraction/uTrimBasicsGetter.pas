unit uTrimBasicsGetter;

interface

uses
  SysUtils, Windows,
  uOSFile, uIoControlFile, uOSFileWithHandle;

type
  TTrimBasicsToInitialize = record
    PaddingLBA: Integer;
    LBAPerCluster: Cardinal;
    StartLBA: UInt64;
  end;

  TTrimBasicsGetter = class abstract(TIoControlFile)
  private
    FileSystemNameInCharArray: Array[0..MAX_PATH - 1] of Char;

  protected
    function GetFileSystemName: String;
    function GetMinimumPrivilege: TCreateFileDesiredAccess; override;

    const
      BytePerLBA = 512;

  public
    constructor Create(FileToGetAccess: String); override;
    function IsPartitionMyResponsibility: Boolean; virtual; abstract;
    function GetTrimBasicsToInitialize: TTrimBasicsToInitialize;
      virtual; abstract;
  end;

  EUnknownPartition = class(Exception);

implementation

{ TTrimBasicsGetter }

function TTrimBasicsGetter.GetFileSystemName: String;
var
  Useless: DWORD;
  PathToGetFileSystemName: String;
begin
  Useless := 0;
  PathToGetFileSystemName :=
    Copy(GetPathOfFileAccessing, Length(ThisComputerPrefix) + 1,
      Length(GetPathOfFileAccessing) - Length(ThisComputerPrefix));
  GetVolumeInformation(PChar(PathToGetFileSystemName), nil, 0, nil, Useless,
    Useless, FileSystemNameInCharArray, SizeOf(FileSystemNameInCharArray));
  result := PChar(@FileSystemNameInCharArray[0]);
end;

function TTrimBasicsGetter.GetMinimumPrivilege: TCreateFileDesiredAccess;
begin
  result := TCreateFileDesiredAccess.DesiredReadOnly;
end;

constructor TTrimBasicsGetter.Create(FileToGetAccess: String);
begin
  inherited Create(FileToGetAccess, DesiredReadOnly);
end;

end.
