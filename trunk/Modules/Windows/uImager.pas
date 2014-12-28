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
  TempFolder: String;
begin
  TempFolder := GetEnvironmentVariable('TMP');
  OpenProcWithOutput('C:\', WinDir + '\System32\cmd.exe /C format ' +
                      Copy(DestDrive,1,2) + ' /fs:fat32 /q /y');

  OpenProcWithOutput('C:\', AppPath + '7z\7z.exe e -y -o"'
                      + TempFolder + '\Erase'              //경로
                      + '\" "' + FromISO + '" ' +
                      '-p"' +                              //비번
                        CapTrimName[LANG_ENGLISH] +
                        CapStartManTrim[LANG_ENGLISH] +
                        BtSemiAutoTrim[LANG_ENGLISH] +
                        CapLocalDisk[LANG_ENGLISH] +
                        CapRemvDisk[LANG_ENGLISH] +
                        CapProg1[LANG_ENGLISH] +
                        CapProg2[LANG_ENGLISH] +
                        CapProg3[LANG_ENGLISH] +
                      '"');
  FromISO := TempFolder + '\Erase\pmagic.iso';

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
