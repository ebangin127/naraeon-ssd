unit uImager;

interface

uses
  ShellAPI, Windows, SysUtils,
  uExeFunctions, uLanguageSettings, uRufus;

procedure ProcessImager(DestDrive, FromISO: String);

implementation

uses uMain;

procedure ProcessImager(DestDrive, FromISO: String);
var
  aHandle: THandle;
begin
  DestDrive := Copy(DestDrive, 1, 2);

  TRufus.SetRufus(
    AppPath + '\Rufus\rufus.exe',
    DestDrive, FromISO);

  DeleteFile(FromISO);
end;
end.
