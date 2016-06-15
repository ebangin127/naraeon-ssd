unit OS.Handle;

interface

uses
  SysUtils, Windows,
  OSFile, OS.SecurityDescriptor;

type
  TCreateFileDesiredAccess =
    (DesiredNone, DesiredReadOnly, DesiredReadWrite);
  PTHandle = ^THandle;
  IOSFileUnlock = interface['{D572D64B-2F90-44EF-AE59-98415B186D8B}']
  end;
  TOSFileUnlock = class(TInterfacedObject, IOSFileUnlock)
  public
    constructor Create(const Handle: PTHandle; const Path: String;
      const AccessPrevilege: TCreateFileDesiredAccess);
    destructor Destroy; override;
  private
    FileHandle: PTHandle;
    Path: String;
    AccessPrivilege: TCreateFileDesiredAccess;
  end;
  TFileHandle = class
  public
    constructor Create(const FileToGetAccess: String;
      const DesiredAccess: TCreateFileDesiredAccess);
    destructor Destroy; override;
    function Unlock: IOSFileUnlock;
    function IsFileHandleValid: Boolean;
    function GetFileHandle: THandle;
    function GetAccessPrivilege: TCreateFileDesiredAccess;
  private
    FileHandle: THandle;
    Path: String;
    AccessPrivilege: TCreateFileDesiredAccess;
  end;

function IsHandleValid(const HandleToCheck: THandle): Boolean;
function GetDesiredAccessFromTCreateFileDesiredAccess
  (const Source: TCreateFileDesiredAccess): DWORD;

implementation

function IsHandleValid(const HandleToCheck: THandle): Boolean;
begin
  result :=
    (HandleToCheck <> INVALID_HANDLE_VALUE) and
    (HandleToCheck <> 0);
end;

function GetDesiredAccessFromTCreateFileDesiredAccess
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

function CreateFileSystemCall(
  const FileToGetAccess: LPCWSTR; const DesiredAccessInDWORD: DWORD): THandle;
var
  SecurityDescriptorManipulator: TSecurityDescriptorManipulator;
const
  OtherHandlesCanReadWrite = FILE_SHARE_WRITE or FILE_SHARE_READ;
  NoFileAttributeFlag = 0;
  NoTemplateFile = 0;
begin
  SecurityDescriptorManipulator := TSecurityDescriptorManipulator.Create;
  try
    result :=
      Windows.CreateFile
        (FileToGetAccess,
         DesiredAccessInDWORD,
         OtherHandlesCanReadWrite,
         SecurityDescriptorManipulator.GetSecurityDescriptor,
         OPEN_EXISTING,
         NoFileAttributeFlag,
         NoTemplateFile);
  finally
    FreeAndNil(SecurityDescriptorManipulator);
  end;
end;

function IsLastSystemCallSucceed: Boolean;
begin
  result :=
    GetLastError = ERROR_SUCCESS;
end;

function GetOSErrorString(const OSErrorCode: Integer): String;
begin
  result :=
    'OS Error: ' +
      SysErrorMessage(OSErrorCode) + ' (' + IntToStr(OSErrorCode) + ')';
end;

procedure IfOSErrorRaiseException;
var
  OSErrorException: EOSError;
begin
  if not IsLastSystemCallSucceed then
  begin
    OSErrorException := EOSError.Create(GetOSErrorString(GetLastError));
    OSErrorException.ErrorCode := GetLastError;
    raise OSErrorException;
  end;
end;

function CreateHandle(const FileToGetAccess: String;
  const DesiredAccess: TCreateFileDesiredAccess): THandle;
var
  DesiredAccessInDWORD: DWORD;
begin
  DesiredAccessInDWORD :=
    GetDesiredAccessFromTCreateFileDesiredAccess(DesiredAccess);
  result :=
    CreateFileSystemCall(PWideChar(FileToGetAccess), DesiredAccessInDWORD);
  IfOSErrorRaiseException;
end;

function TFileHandle.IsFileHandleValid: Boolean;
begin
  result := IsHandleValid(FileHandle);
end;

function TFileHandle.Unlock: IOSFileUnlock;
begin
  result := TOSFileUnlock.Create(@FileHandle, Path, AccessPrivilege);
end;

constructor TFileHandle.Create(const FileToGetAccess: String;
  const DesiredAccess: TCreateFileDesiredAccess);
begin
  inherited Create();
  FileHandle := CreateHandle(FileToGetAccess, DesiredAccess);
  Path := FileToGetAccess;
  AccessPrivilege := DesiredAccess;
end;

destructor TFileHandle.Destroy;
begin
  if IsFileHandleValid then
    CloseHandle(FileHandle);
  inherited Destroy;
end;

function TFileHandle.GetAccessPrivilege: TCreateFileDesiredAccess;
begin
  result := AccessPrivilege;
end;

function TFileHandle.GetFileHandle: THandle;
begin
  result := FileHandle;
end;

{ TOSFileLock }

constructor TOSFileUnlock.Create(const Handle: PTHandle; const Path: String;
  const AccessPrevilege: TCreateFileDesiredAccess);
begin
  inherited Create();
  FileHandle := Handle;
  if IsHandleValid(FileHandle^) then
    CloseHandle(FileHandle^);
  self.Path := Path;
  self.AccessPrivilege := AccessPrevilege;
end;

destructor TOSFileUnlock.Destroy;
begin
  FileHandle^ := CreateHandle(Path, AccessPrivilege);
  inherited;
end;

end.
