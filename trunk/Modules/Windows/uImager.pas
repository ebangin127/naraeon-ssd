unit uImager;

interface

uses
  ShellAPI, Windows, SysUtils,
  uExeFunctions, uLanguageSettings;

procedure ProcessImager(DestDrive, FromISO: String);

implementation

uses uMain;

procedure ProcessImager(DestDrive, FromISO: String);
var
  aHandle: THandle;
begin
  OpenProcWithOutput('C:\', WinDir + '\System32\cmd.exe /C format ' +
                      Copy(DestDrive,1,2) + ' /fs:fat32 /q /y');

  ShellExecute(0, 'open',
    PChar(AppPath + 'unetbootin\unetbootin.exe'),
    PChar('method=diskimage isofile="' + FromISO + '" installtype=usb ' +
      'targetdrive=' +
      Copy(DestDrive, 1, 3) + ' autoinstall=y')
    , nil, SW_NORMAL);

  aHandle := FindWindow(Nil, 'UNetbootin');
  while aHandle = 0 do
  begin
    aHandle := FindWindow(Nil, 'UNetbootin');
    Sleep(10);
  end;
  while aHandle <> 0 do
  begin
    aHandle := FindWindow(Nil, 'UNetbootin');
    Sleep(10);
  end;

  DeleteFile(FromISO);
end;
end.
