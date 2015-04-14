unit uOSFile;

interface

uses
  Windows, SysUtils, Dialogs;

const
  ThisComputerPrefix = '\\.\';
  PhysicalDrivePrefix = 'PHYSICALDRIVE';

type
  TOSFile = class abstract(TObject)
  public
    constructor Create(FileToGetAccess: String); virtual;

    function GetPathOfFileAccessing: String; virtual;
    function GetPathOfFileAccessingWithoutPrefix: String; virtual;

  protected
    procedure IfOSErrorRaiseException;

  private
    PathOfFileAccessing: String;

    function DeletePrefix(PrefixToDelete: String): String;
    function IsPathOfFileAccessingHavePrefix
      (PrefixToCheck: String): Boolean;
  end;

  EInsufficientPrivilege = class(Exception);

implementation

function IsLastSystemCallSucceed: Boolean;
begin
  result :=
    GetLastError = ERROR_SUCCESS;
end;

function GetOSErrorString(OSErrorCode: Integer): String;
begin
  result :=
    'OS Error: ' +
      SysErrorMessage(OSErrorCode) + ' (' + IntToStr(OSErrorCode) + ')';
end;

procedure TOSFile.IfOSErrorRaiseException;
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

function TOSFile.GetPathOfFileAccessing: String;
begin
  exit(PathOfFileAccessing);
end;

function TOSFile.DeletePrefix(PrefixToDelete: String): String;
var
  PathToDeletePrefix: String;
begin
  PathToDeletePrefix := GetPathOfFileAccessing;
  result :=
    Copy(PathToDeletePrefix, Length(PrefixToDelete) + 1,
      Length(PathToDeletePrefix) - Length(PrefixToDelete));
end;

function TOSFile.IsPathOfFileAccessingHavePrefix
  (PrefixToCheck: String): Boolean;
begin
  result :=
    Copy(GetPathOfFileAccessing, 0, Length(PrefixToCheck)) = PrefixToCheck;
end;

function TOSFile.GetPathOfFileAccessingWithoutPrefix: String;
begin
  if IsPathOfFileAccessingHavePrefix
    (ThisComputerPrefix + PhysicalDrivePrefix) then
    exit(DeletePrefix(ThisComputerPrefix + PhysicalDrivePrefix))
  else if IsPathOfFileAccessingHavePrefix(ThisComputerPrefix) then
    exit(DeletePrefix(ThisComputerPrefix))
  else
    exit(GetPathOfFileAccessing);
end;

constructor TOSFile.Create(FileToGetAccess: String);
begin
  inherited Create;
  PathOfFileAccessing := UpperCase(FileToGetAccess);
end;

end.
