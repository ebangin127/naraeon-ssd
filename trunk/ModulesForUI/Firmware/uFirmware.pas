unit uFirmware;

interface

uses
  Windows, SysUtils, IdURI, Dialogs,
  uAlert, uLanguageSettings, uPathManager,
  uStrFunctions, uExeFunctions, uSevenZip, uPhysicalDrive,
  uFileFunctions, uDownloadPath, uFirmwareGetter;

type
  TDownloadedFirmware = record
    IsFirmwareExists: Boolean;
    FirmwarePath: String;
    UsedTempFolder: String;
  end;

function DownloadLatestFirmware(PhysicalDrive: TPhysicalDrive;
  FirmwareGetter: TFirmwareGetter): TDownloadedFirmware;

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

function DownloadLatestFirmware(PhysicalDrive: TPhysicalDrive;
  FirmwareGetter: TFirmwareGetter): TDownloadedFirmware;
var
  FirmPath: String;
  FileEx1, FileEx2, DirEx: Boolean;
  Src, Dest: TDownloadFile;
  DownloadResult: Boolean;
  TempFolder: String;
  Query: TFirmwareQuery;
  QueryResult: TFirmwareQueryResult;
begin
  Randomize;

  TempFolder := TPathManager.TempFolder;
  CreateDir(TempFolder);
  result.UsedTempFolder := TempFolder;
  result.IsFirmwareExists := false;

  FileEx1 := FileExists(TempFolder +
    PhysicalDrive.IdentifyDeviceResult.Model + '.exe');
  FileEx2 := FileExists(TempFolder +
    PhysicalDrive.IdentifyDeviceResult.Model + '.iso');
  DirEx := DirectoryExists(TempFolder +
    PhysicalDrive.IdentifyDeviceResult.Model);

  if FileEx1 then
    DeleteFile(TempFolder +
      PhysicalDrive.IdentifyDeviceResult.Model + '.exe');
  if FileEx2 then
    DeleteFile(TempFolder +
      PhysicalDrive.IdentifyDeviceResult.Model + '.iso');
  if DirEx then
    DeleteDirectory(TempFolder +
      PhysicalDrive.IdentifyDeviceResult.Model + '.iso');

  AlertCreate(fMain, AlrtFirmStart[CurrLang]);

  Src.FBaseAddress := '';
  Src.FFileAddress := TIdURI.URLEncode(
    'http://nstfirmware.naraeon.net/nst_firmdown.php?' +
    'Model=' +
      PhysicalDrive.IdentifyDeviceResult.Model + '&' +
    'Firmware=' +
      PhysicalDrive.IdentifyDeviceResult.Firmware);
  Src.FType := dftPlain;

  Query.Model := PhysicalDrive.IdentifyDeviceResult.Model;
  Query.Firmware := PhysicalDrive.IdentifyDeviceResult.Firmware;
  QueryResult := FirmwareGetter.CheckFirmware(Query);

  Dest.FBaseAddress := TempFolder;
  Dest.FFileAddress := QueryResult.FirmwarePath;
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
  FirmPath := TempFolder + QueryResult.FirmwarePath;
  RenameFile(FirmPath + '_tmp', FirmPath);

  if (ExtractFileExt(FirmPath) = '.zip') or
     (ExtractFileExt(FirmPath) = '.7z') then
  begin
    TSevenZip.Extract(
      TPathManager.AppPath + '7z\7z.exe',
      FirmPath,
      ExtractFilePath(FirmPath) + PhysicalDrive.IdentifyDeviceResult.Model
    );
    DeleteFile(FirmPath);
  end;

  if (FileExists(
        TempFolder +
          PhysicalDrive.IdentifyDeviceResult.Model + '.exe') = false) and
     (FileExists(
        TempFolder +
          PhysicalDrive.IdentifyDeviceResult.Model + '.iso') = false) and
     (DirectoryExists(
        TempFolder +
          PhysicalDrive.IdentifyDeviceResult.Model) = false) then
  begin
    DeleteFile(FirmPath + '_tmp');
    DeleteFile(FirmPath);
    AlertCreate(fMain, AlrtFirmFail[CurrLang]);
  end
  else
    result.IsFirmwareExists := true;

  if result.IsFirmwareExists then
    if FileExists(
      TempFolder +
      PhysicalDrive.IdentifyDeviceResult.Model +
      '.iso') then
        result.FirmwarePath := TempFolder +
          PhysicalDrive.IdentifyDeviceResult.Model + '.iso'
    else if FileExists(
      TempFolder +
      PhysicalDrive.IdentifyDeviceResult.Model + '.exe') then
        result.FirmwarePath :=
          TempFolder +
          PhysicalDrive.IdentifyDeviceResult.Model + '.exe'
    else
      result.FirmwarePath :=
        FindFirmware(TempFolder +
          PhysicalDrive.IdentifyDeviceResult.Model);
end;
end.
