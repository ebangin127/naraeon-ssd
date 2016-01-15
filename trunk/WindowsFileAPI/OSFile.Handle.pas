unit OSFile.Handle;

interface

uses
  Windows, SysUtils, Dialogs, OSFile,
  OS.SecurityDescriptor;

type
  TCreateFileDesiredAccess =
    (DesiredNone, DesiredReadOnly, DesiredReadWrite);

  TOSFileWithHandle = class abstract(TOSFile)
  public
    destructor Destroy; override;
  protected
    procedure CreateHandle(const FileToGetAccess: String;
      const DesiredAccess: TCreateFileDesiredAccess);
    function GetFileHandle: THandle;
    function GetAccessPrivilege: TCreateFileDesiredAccess;
    function GetMinimumPrivilege: TCreateFileDesiredAccess; virtual; abstract;
    function IsHandleValid(const HandleToCheck: THandle): Boolean;
    procedure IfInsufficientPrivilegeRaiseException(
      const DesiredAccess: TCreateFileDesiredAccess);
  private
    FileHandle: THandle;
    AccessPrivilege: TCreateFileDesiredAccess;
    function GetDesiredAccessFromTCreateFileDesiredAccess
      (const Source: TCreateFileDesiredAccess): DWORD;
    function CreateFileSystemCall(FileToGetAccess: LPCWSTR;
      const DesiredAccessInDWORD: DWORD): THandle;
    function IsPrivilegeValid(const PrivilegeToTest: TCreateFileDesiredAccess):
      Boolean;
  end;

  EInsufficientPrivilege = class(Exception);

implementation

function TOSFileWithHandle.GetDesiredAccessFromTCreateFileDesiredAccess
  (const Source: TCreateFileDesiredAccess): DWORD;
const
  AccessNothing = 0;
begin
  case Source of
    DesiredNone:
      exit(AccessNothing);
    DesiredReadOnly:
      exit(GENERIC_READ);
    DesiredReadWrite:
      exit(GENERIC_READ or GENERIC_WRITE);
    else
      raise
        EArgumentOutOfRangeException.Create
          ('ArgumentOutOfRange: Wrong Desired Access Parameter');
  end;
end;

function TOSFileWithHandle.GetFileHandle: THandle;
begin
  exit(FileHandle);
end;

function TOSFileWithHandle.GetAccessPrivilege: TCreateFileDesiredAccess;
begin
  exit(AccessPrivilege);
end;

function TOSFileWithHandle.IsHandleValid(const HandleToCheck: THandle): Boolean;
begin
  result :=
    (HandleToCheck <> INVALID_HANDLE_VALUE) and
    (HandleToCheck <> 0);
end;

function TOSFileWithHandle.IsPrivilegeValid(
  const PrivilegeToTest: TCreateFileDesiredAccess): Boolean;
begin
  result := PrivilegeToTest >= GetMinimumPrivilege;
end;

function TOSFileWithHandle.CreateFileSystemCall(
  const FileToGetAccess: LPCWSTR; const DesiredAccessInDWORD: DWORD): THandle;
var
  SecurityDescriptorManipulator: TSecurityDescriptorManipulator;
const
  OtherHandlesCanReadWrite = FILE_SHARE_WRITE or FILE_SHARE_READ;
  NoSecurityDescriptor = nil;
  NoFileAttributeFlag = 0;
  NoTemplateFile = 0;
begin
  SecurityDescriptorManipulator := TSecurityDescriptorManipulator.Create;
  result :=
    Windows.CreateFile
      (FileToGetAccess,
       DesiredAccessInDWORD,
       OtherHandlesCanReadWrite,
       SecurityDescriptorManipulator.GetSecurityDescriptor,
       OPEN_EXISTING,
       NoFileAttributeFlag,
       NoTemplateFile);
  FreeAndNil(SecurityDescriptorManipulator);
end;

procedure TOSFileWithHandle.IfInsufficientPrivilegeRaiseException
  (const DesiredAccess: TCreateFileDesiredAccess);
begin
  if not IsPrivilegeValid(DesiredAccess) then
    raise EInsufficientPrivilege.Create
      ('InsufficientPrevilege: More privilege is required');
end;

procedure TOSFileWithHandle.CreateHandle(FileToGetAccess: String;
  const DesiredAccess: TCreateFileDesiredAccess);
var
  DesiredAccessInDWORD: DWORD;
begin
  if IsHandleValid(FileHandle) then
    raise EInvalidOp.Create('Invalid Operation: Don''t create handle twice');
  inherited Create(FileToGetAccess);
  DesiredAccessInDWORD :=
    GetDesiredAccessFromTCreateFileDesiredAccess(DesiredAccess);
  FileHandle :=
    CreateFileSystemCall(PWideChar(FileToGetAccess), DesiredAccessInDWORD);
  IfOSErrorRaiseException;
  AccessPrivilege := DesiredAccess;
end;

destructor TOSFileWithHandle.Destroy;
begin
  if IsHandleValid(FileHandle) then
    CloseHandle(FileHandle);
  inherited Destroy;
end;

end.
