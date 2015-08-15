unit uInitializer;

interface

uses
  Forms, SysUtils, Windows, Classes, Graphics, Controls,
  uAlert, uButtonGroup, uLanguageSettings, uPathManager;

procedure InitMainForm;

implementation

uses uMain;

procedure CheckVersion;
begin
  if Win32MajorVersion < 5 then
  begin
    AlertCreate(fMain, AlrtOSError[CurrLang]);
    Application.Terminate;
  end;
end;

procedure CheckEssentialDir;
begin
  if FileExists(TPathManager.AppPath + 'Setup.exe') then
    SysUtils.DeleteFile(TPathManager.AppPath + 'Setup.exe');
  if DirectoryExists(TPathManager.AppPath + 'Image') = false then
    CreateDirectory(PChar(TPathManager.AppPath + 'Image'), nil);
  if DirectoryExists(TPathManager.AppPath + 'Erase') = false then
    CreateDirectory(PChar(TPathManager.AppPath + 'Erase'), nil);
  if DirectoryExists(TPathManager.AppPath + 'Unetbootin') = false then
    CreateDirectory(PChar(TPathManager.AppPath + 'Unetbootin'), nil);
end;

procedure InitMainForm;
begin
  CheckVersion;
  CheckEssentialDir;
end;
end.
