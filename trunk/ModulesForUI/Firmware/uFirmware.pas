unit uFirmware;

interface

uses
  Windows, SysUtils, IdURI, Dialogs,
  uAlert, uLanguageSettings,
  uStrFunctions, uExeFunctions, uGetFirm, uSevenZip, uPhysicalDrive,
  uFileFunctions, uDownloadPath, uPathManager;

type
  FirmCheck = record
    FirmExists: Boolean;
    FirmPath: String;
    TempFolder: String;
  end;

function DownloadFirmware(AppPath: String;
  PhysicalDrive: TPhysicalDrive): FirmCheck;

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

function DownloadFirmware(AppPath: String;
  PhysicalDrive: TPhysicalDrive): FirmCheck;
var
  FirmPath: String;
  FileEx1, FileEx2, DirEx: Boolean;
  Src, Dest: TDownloadFile;
  DownloadResult: Boolean;
  TempFolder: String;
  GetFirm: TGetFirm;
begin
  Randomize;

  TempFolder := TPathManager.TempFolder;
  CreateDir(TempFolder);
  result.TempFolder := TempFolder;
  result.FirmExists := false;

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

  GetFirm := TGetFirm.Create(
    PhysicalDrive.IdentifyDeviceResult.Model,
    PhysicalDrive.IdentifyDeviceResult.Firmware);

  Src.FBaseAddress := '';
  Src.FFileAddress := TIdURI.URLEncode(
    'http://nstfirmware.naraeon.net/nst_firmdown.php?' +
    'Model=' +
      PhysicalDrive.IdentifyDeviceResult.Model + '&' +
    'Firmware=' +
      PhysicalDrive.IdentifyDeviceResult.Firmware);
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
    TSevenZip.Extract(
      AppPath + '7z\7z.exe',
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
    result.FirmExists := true;

  if result.FirmExists then
    if FileExists(
      TempFolder +
      PhysicalDrive.IdentifyDeviceResult.Model +
      '.iso') then
        result.FirmPath := TempFolder +
          PhysicalDrive.IdentifyDeviceResult.Model + '.iso'
    else if FileExists(
      TempFolder +
      PhysicalDrive.IdentifyDeviceResult.Model + '.exe') then
        result.FirmPath :=
          TempFolder +
          PhysicalDrive.IdentifyDeviceResult.Model + '.exe'
    else
      result.FirmPath :=
        FindFirmware(TempFolder +
          PhysicalDrive.IdentifyDeviceResult.Model);
end;
end.
