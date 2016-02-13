unit Thread.Update.Helper;

interface

uses
  Classes, SysUtils, Windows, ShellApi,
  Version, Web.HTTP, Global.Constant, Thread.Download, Getter.WebPath,
  Getter.CodesignVerifier, Global.LanguageString, OS.EnvironmentVariable, Form.Alert,
  Thread.Download.Helper, OS.DeleteDirectory;

type
  TCheckUpdateResult = record
    IsUpdateNeeded: Boolean;
    LatestVersion: TVersion;
    UpdateNotice: String;
  end;

  TUpdater = class
  private
    HTTPWeb: THTTPWeb;
    DownloadThread: TDownloadThread;
    DestinationPath: String;
    DestinationDirectory: String;
    function IsNewerVersionExistsGetLatestVersion: TCheckUpdateResult;
    function GetUpdateCheckResultByGottenPage(
      GottenPage: TStringList): TCheckUpdateResult;
    function GetUpdateNotice: String;
    function GetUpdateNoticeByGottenPage(GottenPage: TStringStream): String;
    procedure PostDownloadMethod;
    procedure VerifyCodesignAndExecute;
    procedure WrongCodesignAlertAndDelete;
    procedure MoveSetupAndExecute;
    function GetLatestVersion: TStringList;
  public
    function CheckUpdate: TCheckUpdateResult;
    procedure StartUpdate;
  end;

implementation

uses Form.Main;

{ TUpdater }

function TUpdater.GetUpdateCheckResultByGottenPage(GottenPage: TStringList):
  TCheckUpdateResult;
var
  CurrentVersionInTVersion: TVersion;
begin
  CurrentVersionInTVersion := VersionStringToTVersion(CurrentVersion);
  result.LatestVersion := VersionStringToTVersion(GottenPage[0]);
  result.IsUpdateNeeded := (CurrentVersionInTVersion < result.LatestVersion);
end;

function TUpdater.GetLatestVersion: TStringList;
begin
  try
    result := HTTPWeb.GetToStringList(AddrUpdChk[CurrLang]);
  except
    result := TStringList.Create;
  end;
end;

function TUpdater.IsNewerVersionExistsGetLatestVersion: TCheckUpdateResult;
var
  LatestSSDToolGetResult: TStringList;
begin
  LatestSSDToolGetResult := GetLatestVersion;
  try
    if LatestSSDToolGetResult.Count = 0 then
      result.IsUpdateNeeded := false
    else
      result := GetUpdateCheckResultByGottenPage(LatestSSDToolGetResult);
  finally
    FreeAndNil(LatestSSDToolGetResult);
  end;
end;

function TUpdater.GetUpdateNoticeByGottenPage(GottenPage: TStringStream):
  String;
const
  FromFirst = 1;
  MaximumCharacters = 300;
  LowerLimit = 200;
  VersionStart = #$D#$A#$D#$A'- ';
  VersionEnd = ' -';
var
  VersionInLog: String;
  GottenPageDataString: String;
  CurrentVersionPosInLog: Integer;
begin
  VersionInLog :=
    VersionStart + CurrentVersion + VersionEnd;
  GottenPageDataString := GottenPage.DataString;
  CurrentVersionPosInLog := Pos(VersionInLog, GottenPageDataString);
  if CurrentVersionPosInLog = 0 then
    CurrentVersionPosInLog := Length(GottenPageDataString);
  if CurrentVersionPosInLog > MaximumCharacters then
    CurrentVersionPosInLog :=
      Pos(VersionStart, GottenPageDataString, LowerLimit);
  result :=
    Copy(GottenPageDataString, FromFirst, CurrentVersionPosInLog);
end;

function TUpdater.GetUpdateNotice: String;
const
  VersionLogHeader = 'http://nstupdate.naraeon.net/ChangeLog';
  VersionLogExtension = '.htm';
var
  UpdateNoticeStream: TStringStream;
begin
  UpdateNoticeStream :=
    HTTPWeb.GetToStringStream(VersionLogHeader + VersionLogExtension);
  if UpdateNoticeStream.Size > 0 then
    result := GetUpdateNoticeByGottenPage(UpdateNoticeStream);
  FreeAndNil(UpdateNoticeStream);
end;

function TUpdater.CheckUpdate: TCheckUpdateResult;
begin
  HTTPWeb := THTTPWeb.Create;
  result := IsNewerVersionExistsGetLatestVersion;
  if result.IsUpdateNeeded then
    result.UpdateNotice := GetUpdateNotice;
  FreeAndNil(HTTPWeb);
end;

procedure TUpdater.StartUpdate;
const
  CancelButton = 1;
var
  Request: TDownloadRequest;
  TempFolder: String;
begin
  if MessageBox(
    fMain.Handle,
    PChar(fMain.GetUpdateNotice),
    PChar(AlrtNewVer[CurrLang]),
    MB_OKCANCEL  + MB_IconInformation) <> CancelButton then
  begin
    fMain.TerminateUpdateThread;
    exit;
  end;

  Request.Source.FBaseAddress := 'http://nstupdate.naraeon.net';
  Request.Source.FFileAddress := '/Setup.exe';
  Request.Source.FType := dftPlain;

  TempFolder := EnvironmentVariable.TempFolder(false);
  CreateDir(TempFolder);
  Request.Destination.FBaseAddress := TempFolder;
  Request.Destination.FFileAddress := 'Setup.exe';
  Request.Destination.FType := dftPlain;
  DestinationPath := TempFolder + 'Setup.exe';
  DestinationDirectory := TempFolder;

  Request.DownloadModelStrings.Download := CapUpdDwld[CurrLang];
  Request.DownloadModelStrings.Cancel := fMain.bCancel.Caption;

  DownloadThread := TDownloadThread.Create;
  DownloadThread.SetRequest(Request);
  DownloadThread.SetPostDownloadMethod(PostDownloadMethod);
  DownloadThread.Start;
end;

procedure TUpdater.WrongCodesignAlertAndDelete;
begin
  AlertCreate(fMain, AlrtWrongCodesign[CurrLang]);
  DeleteFile(PChar(DestinationPath));
  DeleteDirectory(DestinationDirectory);
end;

procedure TUpdater.MoveSetupAndExecute;
begin
  AlertCreate(fMain, AlrtUpdateExit[CurrLang]);
  MoveFile(PChar(DestinationPath),
    PChar(EnvironmentVariable.AppPath + 'Setup.exe'));
  DeleteDirectory(DestinationDirectory);
  ShellExecute(0, nil,
    PChar(EnvironmentVariable.AppPath + 'Setup.exe'), nil, nil, SW_NORMAL);
  fMain.Close;
end;

procedure TUpdater.VerifyCodesignAndExecute;
var
  CodesignVerifier: TCodesignVerifier;
begin
  CodesignVerifier := TCodesignVerifier.Create;
  if not CodesignVerifier.VerifySignByPublisher(
    DestinationPath, NaraeonPublisher) then
    WrongCodesignAlertAndDelete
  else
    MoveSetupAndExecute;
  FreeAndNil(CodesignVerifier);
end;

procedure TUpdater.PostDownloadMethod;
begin
  fMain.CloseButtonGroup;
  if FileExists(DestinationPath) = false then
  begin
    AlertCreate(fMain, AlrtVerCanc[CurrLang]);
    fMain.TerminateUpdateThread;
    exit;
  end;
  VerifyCodesignAndExecute;
  fMain.TerminateUpdateThread;
end;

end.
