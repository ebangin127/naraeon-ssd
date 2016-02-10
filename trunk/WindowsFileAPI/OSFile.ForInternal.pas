unit OSFile.ForInternal;

interface

uses
  OSFile;

type
  TOSFileForInternal = class abstract
  public
    constructor Create(const FileToGetAccess: String); virtual;
    destructor Destroy; override;
  protected
    function GetPathOfFileAccessing: String; virtual;
    function GetPathOfFileAccessingWithoutPrefix: String; virtual;
  private
    OSFileToUse: TOSFile;
  end;

implementation

{ TOSFileForInternal }

constructor TOSFileForInternal.Create(const FileToGetAccess: String);
begin
  inherited Create;
  OSFileToUse := TOSFile.Create(FileToGetAccess);
end;

destructor TOSFileForInternal.Destroy;
begin
  OSFileToUse.Free;
  inherited;
end;

function TOSFileForInternal.GetPathOfFileAccessing: String;
begin
  result := OSFileToUse.GetPathOfFileAccessing;
end;

function TOSFileForInternal.GetPathOfFileAccessingWithoutPrefix: String;
begin
  result := OSFileToUse.GetPathOfFileAccessingWithoutPrefix;
end;

end.
