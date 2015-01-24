unit uImager;

interface

uses
  ShellAPI, Windows, SysUtils, uPathManager,
  uExeFunctions, uLanguageSettings, uRufus;

procedure ProcessImager(DestDrive, FromISO: String);

implementation

procedure ProcessImager(DestDrive, FromISO: String);
begin
  DestDrive := Copy(DestDrive, 1, 2);

  TRufus.SetRufus(
    TPathManager.AppPath + '\Rufus\rufus.exe',
    DestDrive, FromISO);

  DeleteFile(FromISO);
end;
end.
