unit uInitializer;

interface

uses
  Forms, SysUtils, Windows, Classes, Graphics, Controls,
  uAlert, uButtonGroup, uLanguageSettings, uPathManager, uVersionHelper;

procedure InitMainForm;

implementation

uses uMain;

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
  if DirectoryExists(PathManager.AppPath + 'Unetbootin') = false then
    CreateDirectory(PChar(PathManager.AppPath + 'Unetbootin'), nil);
end;

procedure InitMainForm;
begin
  CheckVersion;
  CheckEssentialDir;
end;
end.
