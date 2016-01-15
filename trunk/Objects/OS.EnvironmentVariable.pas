unit OS.EnvironmentVariable;

interface

uses
  Forms, Windows, SysUtils, Classes, ShlObj, Character,
  Registry.Helper, Registry.Helper.Internal, AsciiCheck;

type
  TEnvironmentVariable = class
  private
    function AppendRandomNSTFooter(const BasePath: String): String;
    function RootTempFolder: String;
    procedure SetPathForNormalInstance(const Application: TApplication);
    procedure SetPathForServiceInstance;
  var
    FAppPath: String;
    FWinDir: String;
    FWinDrive: String;
    FAllDesktopPath: String;
    FDesktopPathInChar: array[0..MAX_PATH] of char;
  public
    procedure SetPath(const Application: TApplication);
    function AppPath: String;
    function WinDir: String;
    function WinDrive: String;
    function TempFolder(const AnsiOnly: Boolean): String;
    function AllDesktopPath: String;
    class function Create: TEnvironmentVariable;
  end;

var
  EnvironmentVariable: TEnvironmentVariable;

implementation

procedure TEnvironmentVariable.SetPathForNormalInstance(
  const Application: TApplication);
begin
  FAppPath := ExtractFilePath(Application.ExeName);
  FWinDir := GetEnvironmentVariable('windir');
  FWinDrive := ExtractFileDrive(WinDir);
  SHGetFolderPath(0, CSIDL_COMMON_DESKTOPDIRECTORY, 0, 0,
    @FDesktopPathInChar[0]);
end;

procedure TEnvironmentVariable.SetPathForServiceInstance;
const
  ServiceInstancePath: TRegistryPath =
    (Root: LocalMachine;
     PathUnderHKEY: 'SYSTEM\CurrentControlSet\services\NaraeonSSDToolsDiag';
     ValueName: 'ImagePath');
begin
  FAppPath := ExtractFilePath(NSTRegistry.GetRegStr(ServiceInstancePath));
  FAllDesktopPath := FDesktopPathInChar;
end;

procedure TEnvironmentVariable.SetPath(const Application: TApplication);
begin
  Randomize;
  if Application <> nil then
    SetPathForNormalInstance(Application)
  else
    SetPathForServiceInstance;
end;

function TEnvironmentVariable.AllDesktopPath: String;
begin
  exit(FAllDesktopPath);
end;

function TEnvironmentVariable.AppPath: String;
begin
  exit(FAppPath);
end;

function TEnvironmentVariable.WinDir: String;
begin
  exit(FWinDir);
end;

function TEnvironmentVariable.WinDrive: String;
begin
  exit(FWinDrive);
end;

function TEnvironmentVariable.AppendRandomNSTFooter(
  const BasePath: String): String;
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

function TEnvironmentVariable.RootTempFolder: String;
begin
  result := AppendRandomNSTFooter(FWinDrive);
end;

function TEnvironmentVariable.TempFolder(const AnsiOnly: Boolean): String;
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

class function TEnvironmentVariable.Create: TEnvironmentVariable;
begin
  if EnvironmentVariable = nil then
    result := inherited Create as self
  else
    result := EnvironmentVariable;
end;

initialization
  EnvironmentVariable := TEnvironmentVariable.Create;
finalization
  EnvironmentVariable.Free;
end.
