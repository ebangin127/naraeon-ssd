unit OSFile.Handle;

interface

uses
  Windows, SysUtils, Dialogs,
  OSFile, OS.SecurityDescriptor, OS.Handle;

type
  TOSFileWithHandle = class abstract(TOSFile)
  public
    destructor Destroy; override;
    function Unlock: IOSFileUnlock;
  protected
    procedure CreateHandle(const FileToGetAccess: String;
      const DesiredAccess: TCreateFileDesiredAccess);
    function GetFileHandle: THandle;
    function GetAccessPrivilege: TCreateFileDesiredAccess;
    function GetMinimumPrivilege: TCreateFileDesiredAccess; virtual; abstract;
  private
    FileHandle: TFileHandle;
  end;

  EInsufficientPrivilege = class(Exception);

implementation

function TOSFileWithHandle.GetAccessPrivilege: TCreateFileDesiredAccess;
begin
  result := FileHandle.GetAccessPrivilege;
end;

function TOSFileWithHandle.GetFileHandle: THandle;
begin
  exit(FileHandle.GetFileHandle);
end;

function TOSFileWithHandle.Unlock: IOSFileUnlock;
begin
  result := FileHandle.Unlock;
end;

procedure TOSFileWithHandle.CreateHandle(const FileToGetAccess: String;
  const DesiredAccess: TCreateFileDesiredAccess);
begin
  if FileHandle <> nil then
    raise EInvalidOp.Create('Invalid Operation: Don''t create handle twice');
  inherited Create(FileToGetAccess);
  FileHandle := TFileHandle.Create(FileToGetAccess, DesiredAccess);
end;

destructor TOSFileWithHandle.Destroy;
begin
  if FileHandle <> nil then
    FreeAndNil(FileHandle);
  inherited Destroy;
end;

end.
