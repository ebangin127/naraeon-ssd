unit uPathManager;

interface

uses
  Forms, Windows, SysUtils, Classes, ShlObj, Character,
  uRegFunctions;

type
  TPathManager = class
  private
    class function AppendRandomNSTFooter(BasePath: String): String; static;
    class function RootTempFolder: String; static;

    class var FAppPath: String;
    class var FWinDir: String;
    class var FWinDrive: String;
    class var FAllDesktopPath: String;
    class var FDesktopPathInChar: array[0..MAX_PATH] of char;
    class function IsValidPath(PathToValidate: String): Boolean; static;
    class procedure SetPathForNormalInstance(Application: TApplication); static;
    class procedure SetPathForServiceInstance; static;

  public
    class procedure SetPath(Application: TApplication);

    class function AppPath: String;
    class function WinDir: String;
    class function WinDrive: String;
    class function TempFolder(AnsiOnly: Boolean): String;
    class function AllDesktopPath: String;
  end;

implementation

class procedure TPathManager.SetPathForNormalInstance(
  Application: TApplication);
begin
  FAppPath := ExtractFilePath(Application.ExeName);
  FWinDir := GetEnvironmentVariable('windir');
  FWinDrive := ExtractFileDrive(WinDir);
  SHGetFolderPath(0, CSIDL_COMMON_DESKTOPDIRECTORY, 0, 0,
    @FDesktopPathInChar[0]);
end;

class procedure TPathManager.SetPathForServiceInstance;
begin
  FAppPath := ExtractFilePath(GetRegStr('LM',
    'SYSTEM\CurrentControlSet\services\NaraeonSSDToolsDiag', 'ImagePath'));
  FAllDesktopPath := FDesktopPathInChar;
end;

class procedure TPathManager.SetPath(Application: TApplication);
begin
  Randomize;
  if Application <> nil then
    SetPathForNormalInstance(Application)
  else
    SetPathForServiceInstance;
end;

class function TPathManager.AllDesktopPath: String;
begin
  exit(FAllDesktopPath);
end;

class function TPathManager.AppPath: String;
begin
  exit(FAppPath);
end;

class function TPathManager.WinDir: String;
begin
  exit(FWinDir);
end;

class function TPathManager.WinDrive: String;
begin
  exit(FWinDrive);
end;

class function TPathManager.AppendRandomNSTFooter(BasePath: String): String;
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

class function TPathManager.RootTempFolder: String;
begin
  result := AppendRandomNSTFooter(FWinDrive);
end;

class function TPathManager.IsValidPath(PathToValidate: String): Boolean;
var
  CurrentCharacter: Char;
begin
  result := true;
  for CurrentCharacter in PathToValidate do
    if Ord(CurrentCharacter) > Byte.MaxValue then
      exit(false);
end;

class function TPathManager.TempFolder(AnsiOnly: Boolean): String;
var
  TempPath: String;
begin
  SetLength(TempPath, MAX_PATH + 1);
  SetLength(TempPath, GetTempPath(MAX_PATH, PChar(TempPath)));
  if (not AnsiOnly) or (IsValidPath(TempPath)) then
    result := AppendRandomNSTFooter(TempPath)
  else
    result := RootTempFolder;
end;

end.
