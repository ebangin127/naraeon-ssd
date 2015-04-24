unit uPathManager;

interface

uses
  Forms, Windows, SysUtils, Classes, ShlObj,
  uRegFunctions;

type
  TPathManager = class
  protected
    class var FAppPath: String;
    class var FWinDir: String;
    class var FWinDrive: String;
    class var FTempFolder: String;
    class var FAllDesktopPath: String;
    class var FDesktopPathInChar: array[0..MAX_PATH] of char;

  public
    class procedure SetPath(Application: TApplication);

    class function AppPath: String;
    class function WinDir: String;
    class function WinDrive: String;
    class function TempFolder: String;
    class function AllDesktopPath: String;
  end;

implementation

class procedure TPathManager.SetPath(Application: TApplication);
begin
  Randomize;
  if Application <> nil then
    FAppPath := ExtractFilePath(Application.ExeName)
  else
    FAppPath := ExtractFilePath(GetRegStr('LM',
      'Software\Microsoft\Windows\CurrentVersion\Uninstall\Naraeon SSD Tools\',
      'UninstallString'));
  FWinDir := GetEnvironmentVariable('windir');
  FWinDrive := ExtractFileDrive(WinDir);
  SHGetFolderPath(0, CSIDL_COMMON_DESKTOPDIRECTORY, 0, 0,
    @FDesktopPathInChar[0]);
  FAllDesktopPath := FDesktopPathInChar;
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

class function TPathManager.TempFolder: String;
begin
  result :=
    FWinDrive +
    '\NST' + IntToStr(Random(2147483647)) + '\';
  while DirectoryExists(result) do
    result :=
      FWinDrive +
      '\NST' + IntToStr(Random(2147483647)) + '\';
end;

end.
