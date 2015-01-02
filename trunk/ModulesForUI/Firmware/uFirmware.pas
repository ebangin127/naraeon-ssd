unit uFirmware;

interface

uses
  Windows, SysUtils,
  uAlert, uLanguageSettings,
  uStrFunctions, uExeFunctions, uGetFirm,
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
  FirmPath: String;
  FileEx1, FileEx2, DirEx: Boolean;
  Src, Dest: TDownloadFile;
  DownloadResult: Boolean;
  TempFolder: String;
  GetFirm: TGetFirm;
begin
  Randomize;
  TempFolder :=
    GetEnvironmentVariable('TMP') +
    '\NST' + IntToStr(Random(2147483647)) + '\';
  while DirectoryExists(TempFolder) do
    TempFolder :=
      GetEnvironmentVariable('TMP') +
      '\NST' + IntToStr(Random(2147483647)) + '\';

  CreateDir(TempFolder);
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

  GetFirm := TGetFirm.Create(SSDInfo.Model, SSDInfo.Firmware);

  Src.FBaseAddress := '';
  Src.FFileAddress :=
    'http://nstfirmware.naraeon.net/nst_firmdown.php?' +
    'Model=' + SSDInfo.Model + '&' +
    'Firmware=' + SSDInfo.Firmware;
  Src.FType := dftPlain;

  Dest.FBaseAddress := TempFolder;
  Dest.FFileAddress := GetFirm.GetVersion.FirmFileName;
  Dest.FPostAddress := '_tmp';
  Dest.FType := dftPlain;

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
  FirmPath := TempFolder + GetFirm.GetVersion.FirmFileName;
  RenameFile(FirmPath + '_tmp', FirmPath);

  FreeAndNil(GetFirm);

  if (ExtractFileExt(FirmPath) = '.zip') or
     (ExtractFileExt(FirmPath) = '.7z') then
  begin
    OpenProcWithOutput('C:\', AppPath + '7z\7z.exe e -y -o"'
                        + ExtractFilePath(FirmPath) + SSDInfo.Model
                        + '\" "' + FirmPath + '"');
    DeleteFile(FirmPath);
  end;

  if (FileExists(
        TempFolder + SSDInfo.Model + '.exe') = false) and
     (FileExists(
        TempFolder + SSDInfo.Model + '.iso') = false) and
     (DirectoryExists(
        TempFolder + SSDInfo.Model) = false) then
  begin
    DeleteFile(FirmPath + '_tmp');
    DeleteFile(FirmPath);
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
