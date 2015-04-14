unit uTrimBasicsGetter;

interface

uses
  SysUtils, Windows,
  uOSFileWithHandle, uIoControlFile;

type
  TTrimBasicsToInitialize = record
    PaddingLBA: Integer;
    LBAPerCluster: Cardinal;
    StartLBA: UInt64;
  end;

  TTrimBasicsGetter = class abstract(TIoControlFile)
  private
    const
      FileSystemLength = 9;

    type
      TFileSystemName = record
        FileSystem: Array[0..FileSystemLength - 1] of Char;
      end;

  private
    InnerBuffer: TFileSystemName;
    function GetIOBuffer: TIoControlIOBuffer;

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

function TTrimBasicsGetter.GetIOBuffer: TIoControlIOBuffer;
begin
  result.InputBuffer.Buffer := nil;
  result.InputBuffer.Size := 0;

  result.OutputBuffer.Buffer := @InnerBuffer;
  result.OutputBuffer.Size := SizeOf(InnerBuffer);
end;

function TTrimBasicsGetter.GetMinimumPrivilege: TCreateFileDesiredAccess;
begin
  result := TCreateFileDesiredAccess.DesiredReadOnly;
end;

constructor TTrimBasicsGetter.Create(FileToGetAccess: String);
begin
  inherited Create(FileToGetAccess, DesiredReadOnly);
end;

function TTrimBasicsGetter.GetFileSystemName: String;
const
  FromFirst = 1;
begin
  IoControl(TIoControlCode.QueryFileSystemName, GetIOBuffer);
  result :=
    Copy(PChar(@InnerBuffer.FileSystem[0]),
      FromFirst,
      Length(PChar(@InnerBuffer.FileSystem[0])));
end;

end.
