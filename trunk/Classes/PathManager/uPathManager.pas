unit uPathManager;

interface

uses
  Forms, Windows, SysUtils, Classes;

type
  TPathManager = class
  protected
    class var FAppPath: String;
    class var FWinDir: String;
    class var FWinDrive: String;
    class var FTempFolder: String;

  public
    class procedure SetPath(Application: TApplication);

    class function AppPath: String;
    class function WinDir: String;
    class function WinDrive: String;
    class function TempFolder: String;
  end;

implementation

class procedure TPathManager.SetPath(Application: TApplication);
begin
  Randomize;
  FAppPath := ExtractFilePath(Application.ExeName);
  FWinDir := GetEnvironmentVariable('windir');
  FWinDrive := ExtractFileDrive(WinDir);
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
