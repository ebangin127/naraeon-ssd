unit uFirmware;

interface

uses
  Windows, SysUtils,
  uAlert, uLanguageSettings,
  uStrFunctions, uExeFunctions,
  uFileFunctions, uDownloadPath, uSSDInfo;

type
  FirmCheck = record
    FirmExists: Boolean;
    FirmPath: String;
  end;

function DownloadFirmware(AppPath: String; SSDInfo: TSSDInfo): FirmCheck;

implementation

uses uMain;

function FindFirmware(FirmPath: String): String;
var
  FirmSR : TSearchrec;
  FindFile : integer;
  SearchDir : string;
begin
  SearchDir := FirmPath + '\*.*';
  FindFile := FindFirst(SearchDir,faAnyFile, FirmSR);
  while FindFile = 0 do
  begin
    if (ExtractFileExt(FirmSR.Name) = '.exe') or
       (ExtractFileExt(FirmSR.Name) = '.iso') then
    begin
      result := FirmPath + '\' + FirmSR.Name;
      break;
    end;
    FindNext(FirmSR);
  end;
  FindClose(FirmSR);
end;

function DownloadFirmware(AppPath: String; SSDInfo: TSSDInfo): FirmCheck;
var
  FirmName, FirmPath: String;
  FileEx1, FileEx2, DirEx: Boolean;
  Src, Dest: TDownloadFile;
  DownloadResult: Boolean;
  TempFolder: String;
begin
  TempFolder := GetEnvironmentVariable('TMP');
  result.FirmExists := false;

  FileEx1 := FileExists(TempFolder + SSDInfo.Model + '.exe');
  FileEx2 := FileExists(TempFolder + SSDInfo.Model + '.iso');
  DirEx := DirectoryExists(TempFolder + SSDInfo.Model);

  if FileEx1 then
    DeleteFile(TempFolder + SSDInfo.Model + '.exe');
  if FileEx2 then
    DeleteFile(TempFolder + SSDInfo.Model + '.iso');
  if DirEx then
    DeleteDirectory(TempFolder + SSDInfo.Model + '.iso');

  AlertCreate(fMain, AlrtFirmStart[CurrLang]);

  Src.FBaseAddress := '';
  Src.FFileAddress := 'http://www.naraeon.net/SSDTools_Common/Firmware/'
                      + TrimEx(SSDInfo.Model) + 'path.htm';
  Src.FType := dftGetFromWeb;

  Dest.FBaseAddress := TempFolder;
  Dest.FFileAddress := 'http://www.naraeon.net/SSDTools_Common/Firmware/'
                        + TrimEx(SSDInfo.Model) + 'name.htm';
  Dest.FPostAddress := '_tmp';
  Dest.FType := dftGetFromWeb;

  FirmName := GetDownloadPath(Dest);

  with fMain do
  begin
    gFirmware.Visible := false;
    DownloadResult := DownloadFile(Src, Dest, CapFirmDwld[CurrLang],
                                   bCancel.Caption);
    gFirmware.Visible := true;
  end;

  if fAlert <> Nil then FreeAndNil(fAlert);
  if DownloadResult = false then
  begin
    AlertCreate(fMain, AlrtFirmCanc[CurrLang]);
    exit;
  end;
  FirmPath := Copy(FirmName, 0, Length(FirmName) - Length('_tmp'));
  RenameFile(FirmName, FirmPath);

  if (ExtractFileExt(FirmPath) = '.zip') or
     (ExtractFileExt(FirmPath) = '.7z') then
  begin
    OpenProcWithOutput('C:\', AppPath + '7z\7z.exe e -y -o"'
                        + ExtractFilePath(FirmPath) + SSDInfo.Model
                        + '\" "' + FirmPath + '"');
    DeleteFile(TempFolder + FirmName);
  end;

  if (FileExists(
        TempFolder + SSDInfo.Model + '.exe') = false) and
     (FileExists(
        TempFolder + SSDInfo.Model + '.iso') = false) and
     (DirectoryExists(
        TempFolder + SSDInfo.Model) = false) then
  begin
    DeleteFile(TempFolder + FirmName);
    AlertCreate(fMain, AlrtFirmFail[CurrLang]);
  end
  else
    result.FirmExists := true;

  if result.FirmExists then
    if FileExists(TempFolder + SSDInfo.Model + '.iso') then
      result.FirmPath := TempFolder + SSDInfo.Model + '.iso'
    else if FileExists(
        TempFolder + SSDInfo.Model + '.exe') then
      result.FirmPath := TempFolder + SSDInfo.Model + '.exe'
    else
      result.FirmPath :=
        FindFirmware(TempFolder + SSDInfo.Model);
end;
end.
