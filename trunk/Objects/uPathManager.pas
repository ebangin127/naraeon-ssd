unit uPathManager;

interface

uses
  Forms, Windows, SysUtils, Classes, ShlObj, Character,
  uNSTRegistry, uRegistryHelper, AsciiCheck;

type
  TPathManager = class
  private
    function AppendRandomNSTFooter(BasePath: String): String;
    function RootTempFolder: String;
    procedure SetPathForNormalInstance(Application: TApplication);
    procedure SetPathForServiceInstance;

  var
    FAppPath: String;
    FWinDir: String;
    FWinDrive: String;
    FAllDesktopPath: String;
    FDesktopPathInChar: array[0..MAX_PATH] of char;

  public
    procedure SetPath(Application: TApplication);

    function AppPath: String;
    function WinDir: String;
    function WinDrive: String;
    function TempFolder(AnsiOnly: Boolean): String;
    function AllDesktopPath: String;

    class function Create: TPathManager;
  end;

var
  PathManager: TPathManager;

implementation

procedure TPathManager.SetPathForNormalInstance(
  Application: TApplication);
begin
  FAppPath := ExtractFilePath(Application.ExeName);
  FWinDir := GetEnvironmentVariable('windir');
  FWinDrive := ExtractFileDrive(WinDir);
  SHGetFolderPath(0, CSIDL_COMMON_DESKTOPDIRECTORY, 0, 0,
    @FDesktopPathInChar[0]);
end;

procedure TPathManager.SetPathForServiceInstance;
const
  ServiceInstancePath: TRegistryPath =
    (Root: LocalMachine;
     PathUnderHKEY: 'SYSTEM\CurrentControlSet\services\NaraeonSSDToolsDiag';
     ValueName: 'ImagePath');
begin
  FAppPath := ExtractFilePath(NSTRegistry.GetRegStr(ServiceInstancePath));
  FAllDesktopPath := FDesktopPathInChar;
end;

procedure TPathManager.SetPath(Application: TApplication);
begin
  Randomize;
  if Application <> nil then
    SetPathForNormalInstance(Application)
  else
    SetPathForServiceInstance;
end;

function TPathManager.AllDesktopPath: String;
begin
  exit(FAllDesktopPath);
end;

function TPathManager.AppPath: String;
begin
  exit(FAppPath);
end;

function TPathManager.WinDir: String;
begin
  exit(FWinDir);
end;

function TPathManager.WinDrive: String;
begin
  exit(FWinDrive);
end;

function TPathManager.AppendRandomNSTFooter(BasePath: String): String;
var
  OptionalBackslash: String;
begin
  OptionalBackslash := '';
  if BasePath[Length(BasePath)] <> '\'  then
    OptionalBackslash := '\';

  result := BasePath + OptionalBackslash +
    'NST' + IntToStr(Random(2147483647)) + '\';
  while DirectoryExists(result) do
    result :=
      BasePath + OptionalBackslash +
      'NST' + IntToStr(Random(2147483647)) + '\';
end;

function TPathManager.RootTempFolder: String;
begin
  result := AppendRandomNSTFooter(FWinDrive);
end;

function TPathManager.TempFolder(AnsiOnly: Boolean): String;
var
  TempPath: String;
begin
  SetLength(TempPath, MAX_PATH + 1);
  SetLength(TempPath, GetTempPath(MAX_PATH, PChar(TempPath)));
  if (not AnsiOnly) or (StringHelper.IsAscii(TempPath)) then
    result := AppendRandomNSTFooter(TempPath)
  else
    result := RootTempFolder;
end;

class function TPathManager.Create: TPathManager;
begin
  if PathManager = nil then
    result := inherited Create as self
  else
    result := PathManager;
end;

initialization
  PathManager := TPathManager.Create;
finalization
  PathManager.Free;
end.
