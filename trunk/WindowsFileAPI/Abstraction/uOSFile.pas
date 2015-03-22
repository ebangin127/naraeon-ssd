unit uOSFile;

interface

uses
  Windows, SysUtils, Dialogs;

const
  ThisComputerPrefix = '\\.\';
  PhysicalDrivePrefix = 'PhysicalDrive';

type
  TOSFile = class abstract(TObject)
  public
    constructor Create(FileToGetAccess: String); virtual;

  protected
    procedure IfOSErrorRaiseException;

    function GetPathOfFileAccessing: String;
    function GetPathOfFileAccessingWithoutPrefix: String;

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
begin
  if not IsLastSystemCallSucceed then
    raise EOSError.Create(GetOSErrorString(GetLastError));
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
  PathOfFileAccessing := FileToGetAccess;
end;

end.
