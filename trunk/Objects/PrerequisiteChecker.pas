unit PrerequisiteChecker;

interface

uses
  Forms, SysUtils, Windows,
  Form.Alert, uLanguageSettings, uPathManager, OS.VersionHelper;

procedure CheckPrerequisite;

implementation

uses Form.Main;

procedure CheckVersion;
begin
  if VersionHelper.MajorVersion < 5 then
  begin
    AlertCreate(fMain, AlrtOSError[CurrLang]);
    Application.Terminate;
  end;
end;

procedure CheckEssentialDir;
begin
  if FileExists(PathManager.AppPath + 'Setup.exe') then
    SysUtils.DeleteFile(PathManager.AppPath + 'Setup.exe');
  if DirectoryExists(PathManager.AppPath + 'Image') = false then
    CreateDirectory(PChar(PathManager.AppPath + 'Image'), nil);
  if DirectoryExists(PathManager.AppPath + 'Erase') = false then
    CreateDirectory(PChar(PathManager.AppPath + 'Erase'), nil);
  if DirectoryExists(PathManager.AppPath + 'Rufus') = false then
    CreateDirectory(PChar(PathManager.AppPath + 'Rufus'), nil);
end;

procedure CheckPrerequisite;
begin
  CheckVersion;
  CheckEssentialDir;
end;
end.
