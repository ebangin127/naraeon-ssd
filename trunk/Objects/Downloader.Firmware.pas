﻿unit Downloader.Firmware;

interface

uses
  Windows, SysUtils, IdURI, Dialogs, ShellApi,
  Form.Alert, Global.LanguageString, OS.EnvironmentVariable, Global.Constant,
  OS.ProcessOpener, Device.PhysicalDrive, Thread.Download,
  OS.DeleteDirectory, Getter.WebPath, Getter.LatestFirmware,
  Getter.CodesignVerifier, Thread.Download.Helper, Extern.Rufus, OS.Handle,
  Unlocker;

type
  TFirmwareDownloader = class
  private
    TempFolder: String;
    DownloadThread: TDownloadThread;
    Request: TDownloadRequest;
    PhysicalDrive: IPhysicalDrive;
    FirmwareGetter: TFirmwareGetter;
    FirmwareQueryResult: TFirmwareQueryResult;
    FirmwarePath: String;
    procedure PrepareTempFolder;
    procedure PrepareRequest;
    procedure PostDownloadMethod;
    procedure StartDownload;
    procedure DownloadLatestFirmware;
    procedure RenameTempFileAndSetFirmwarePath;
    procedure ExpandAndSetFirmwarePath;
    function VerifyCodesign: Boolean;
    procedure AlertAndDeleteInvalidFile;
    procedure Burn;
    procedure UnlockedBurn(const Letter: string);
    procedure VerifyAndRunRufus;
    procedure CheckFirmwarePathAndRunRufus;
    procedure CheckTypeAndRunOrBurn;
    function IsExe(const Path: String): Boolean;
  public
    procedure DownloadFirmware;
  end;

implementation

uses Form.Main;

procedure TFirmwareDownloader.PrepareTempFolder;
begin
  TempFolder := EnvironmentVariable.TempFolder(true);
  CreateDir(TempFolder);
end;

procedure TFirmwareDownloader.PrepareRequest;
var
  Query: TFirmwareQuery;
begin
  Request.Source.FBaseAddress := '';
  Request.Source.FFileAddress := TIdURI.URLEncode(
    'http://nstfirmware.naraeon.net/NSTFirmwareDownload.php?' +
    'Model=' +
      PhysicalDrive.IdentifyDeviceResult.Model + '&' +
    'Firmware=' +
      PhysicalDrive.IdentifyDeviceResult.Firmware);
  Request.Source.FType := dftPlain;

  Query.Model := PhysicalDrive.IdentifyDeviceResult.Model;
  Query.Firmware := PhysicalDrive.IdentifyDeviceResult.Firmware;
  FirmwareQueryResult := FirmwareGetter.CheckFirmware(Query);

  Request.Destination.FBaseAddress := TempFolder;
  Request.Destination.FFileAddress := FirmwareQueryResult.FirmwarePath;
  Request.Destination.FPostAddress := '_tmp';
  Request.Destination.FType := dftPlain;
  
  Request.DownloadModelStrings.Download := CapFirmDwld[CurrLang];
  Request.DownloadModelStrings.Cancel := BtDnldCncl[CurrLang];
end;

procedure TFirmwareDownloader.StartDownload;
begin
  DownloadThread := TDownloadThread.Create;
  DownloadThread.SetRequest(Request);
  DownloadThread.SetPostDownloadMethod(PostDownloadMethod);
  DownloadThread.Start;
end;

procedure TFirmwareDownloader.DownloadFirmware;
begin
  self.PhysicalDrive := fMain.SelectedDrive;
  fMain.tRefresh.Enabled := false;
  fMain.gFirmware.Visible := false;
  DownloadLatestFirmware;
end;

procedure TFirmwareDownloader.Burn;
var
  Letter: String;
  Unlock: IDriveHandleUnlocker;
begin
  Letter := Copy(fMain.cUSB.Items[fMain.cUSB.ItemIndex], 1, 3);
  Unlock := TDriveHandleUnlocker.Create(Letter, fMain.IdentifiedDriveList,
    fMain.SelectedDrive);
  UnlockedBurn(Letter);
end;

procedure TFirmwareDownloader.UnlockedBurn(const Letter: string);
begin
  AlertCreate(fMain, AlrtStartFormat[CurrLang]);
  Rufus.RunRufus(Letter, FirmwarePath, false);
  AlertCreate(fMain, AlrtFirmEnd[CurrLang]);
  DeleteDirectory(ExtractFilePath(FirmwarePath));
  DeleteDirectory(TempFolder);
end;

procedure TFirmwareDownloader.DownloadLatestFirmware;
begin
  AlertCreate(fMain, AlrtFirmStart[CurrLang]);
  FirmwareGetter := TFirmwareGetter.Create;
  try
    PrepareTempFolder;
    PrepareRequest;
    StartDownload;
  finally
    FreeAndNil(FirmwareGetter);
  end;
end;

procedure TFirmwareDownloader.RenameTempFileAndSetFirmwarePath;
begin
  RenameFile(Request.Destination.FBaseAddress +
    Request.Destination.FFileAddress +
    Request.Destination.FPostAddress,
    TempFolder + FirmwareQueryResult.FirmwarePath);
  FirmwarePath := TempFolder + FirmwareQueryResult.FirmwarePath;
end;

procedure TFirmwareDownloader.ExpandAndSetFirmwarePath;
var
  NewPath: String;
begin
  if ExtractFileExt(FirmwarePath) = '.is_' then
  begin
    NewPath := Copy(FirmwarePath, 1, Length(FirmwarePath) -
      Length(ExtractFileExt(FirmwarePath))) + '.iso';
    ProcessOpener.OpenProcWithOutput(EnvironmentVariable.WinDir,
      'expand.exe "' + FirmwarePath + '" "' + NewPath + '"');
    DeleteFile(FirmwarePath);
    FirmwarePath := NewPath;
  end;
end;

procedure TFirmwareDownloader.AlertAndDeleteInvalidFile;
begin
  AlertCreate(fMain, AlrtWrongCodesign[CurrLang]);
  DeleteFile(FirmwarePath);
  DeleteDirectory(ExtractFilePath(TempFolder));
end;

function TFirmwareDownloader.VerifyCodesign: Boolean;
var
  CodesignVerifier: TCodesignVerifier;
begin
  result := false;
  if ExtractFileExt(FirmwarePath)
    [Length(ExtractFileExt(FirmwarePath))] = '_' then
  begin
    CodesignVerifier := TCodesignVerifier.Create;
    result := CodesignVerifier.VerifySignByPublisher(FirmwarePath,
      NaraeonPublisher);
    if not result then
      AlertAndDeleteInvalidFile;
    FreeAndNil(CodesignVerifier);
  end;
end;

procedure TFirmwareDownloader.PostDownloadMethod;
begin
  if not FileExists(Request.Destination.FBaseAddress +
    Request.Destination.FFileAddress +
    Request.Destination.FPostAddress) then
  begin
    AlertCreate(fMain, AlrtFirmCanc[CurrLang]);
    fMain.gFirmware.Visible := true;
    fMain.tRefresh.Enabled := true;
    Free; //FI:W515
  end
  else
    VerifyAndRunRufus;
end;

procedure TFirmwareDownloader.VerifyAndRunRufus;
begin
  RenameTempFileAndSetFirmwarePath;
  if not VerifyCodesign then
  begin
    fMain.gFirmware.Visible := true;
    fMain.tRefresh.Enabled := true;
    Free; //FI:W515
  end
  else
  begin
    ExpandAndSetFirmwarePath;
    CheckFirmwarePathAndRunRufus;
  end;
end;

procedure TFirmwareDownloader.CheckFirmwarePathAndRunRufus;
begin
  if not FileExists(FirmwarePath) then
  begin
    AlertCreate(fMain, AlrtFirmFail[CurrLang]);
    fMain.gFirmware.Visible := true;
    fMain.tRefresh.Enabled := true;
    Free; //FI:W515
  end
  else
    CheckTypeAndRunOrBurn;
end;

function TFirmwareDownloader.IsExe(const Path: String): Boolean;
begin
  result := ExtractFileExt(Path) = '.exe';
end;

procedure TFirmwareDownloader.CheckTypeAndRunOrBurn;
begin
  if (not IsExe(FirmwarePath)) and (not Rufus.CheckRufus) then
  begin
    AlertCreate(fMain, AlrtFirmFail[CurrLang]);
    fMain.gFirmware.Visible := true;
    fMain.tRefresh.Enabled := true;
    Free; //FI:W515
  end;

  if IsExe(FirmwarePath) then
  begin
    ShellExecute(0, 'open', PChar(FirmwarePath), nil, nil, SW_SHOW);
    fMain.iFirmUp.OnClick(nil);
  end
  else
  begin
    Burn;
  end;

  fMain.gFirmware.Visible := true;
  fMain.tRefresh.Enabled := true;
  Free; //FI:W515
end;

end.
