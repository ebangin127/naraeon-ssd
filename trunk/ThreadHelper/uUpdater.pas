unit uUpdater;

interface

uses
  Classes, SysUtils,
  uVersion, uHTTPWeb, uGlobalSettings;

type
  TCheckUpdateResult = record
    IsUpdateNeeded: Boolean;
    LatestVersion: TVersion;
    UpdateNotice: String;
  end;

  TUpdater = class
  private
    HTTPWeb: THTTPWeb;
    function IsNewerVersionExistsGetLatestVersion: TCheckUpdateResult;
    function GetUpdateCheckResultByGottenPage(
      GottenPage: TStringList): TCheckUpdateResult;
    function GetUpdateNotice: String;
    function GetUpdateNoticeByGottenPage(GottenPage: TStringStream): String;
  public
    function CheckUpdate: TCheckUpdateResult;
    procedure StartUpdate;
  end;

implementation

uses uMain;

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

function TUpdater.IsNewerVersionExistsGetLatestVersion: TCheckUpdateResult;
const
  LatestSSDToolURL = 'http://nstupdate.naraeon.net/latestSSDTools.htm';
var
  LatestSSDToolGetResult: TStringList;
begin
  LatestSSDToolGetResult := HTTPWeb.GetToStringList(LatestSSDToolURL);
  if LatestSSDToolGetResult.Count = 0 then
    result.IsUpdateNeeded := false
  else
    result := GetUpdateCheckResultByGottenPage(LatestSSDToolGetResult);
  FreeAndNil(LatestSSDToolGetResult);
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
begin
  fMain.ProgressDownload;
end;

end.
