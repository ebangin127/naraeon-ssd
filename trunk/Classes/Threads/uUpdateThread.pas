unit uUpdateThread;

interface

uses Classes, SysUtils, Math, Dialogs, Windows,
    IdBaseComponent, IdRawBase, IdRawClient, idHttp,
    uDiskFunctions, IdIcmpClient, uLanguageSettings,
    uVersion;

type
  TUpdateThread = class(TThread)
  protected
    procedure Execute; override;
  end;

var
  VersionLoader: TIdHttp;
  ServerVersion: String;
  CurrChr: Integer;
  ChangeLog: String;

implementation

uses
  uMain, uSSDInfo;


procedure TUpdateThread.Execute;
var
  ConnectionChecker: TIdHttp;
  Connected: Boolean;

  CurrStat: Integer;
  ClientVer, ServerVer: TVersion;
  SMajorVer, SMinorVer, SBuildVer: String;
  LangFileName: String;
  CurrVer: Integer;
  LogStream: TStringStream;
begin
  ConnectionChecker := TIdHttp.Create(nil);
  try
    ConnectionChecker.HandleRedirects := True;
    ConnectionChecker.Request.UserAgent := 'Naraeon SSD Tools';
    ConnectionChecker.Head(
      'http://www.naraeon.net/SSDTools/latestSSDTools.htm');
    Connected := (ConnectionChecker.response.ResponseCode = 200);
  except
    Connected := false;
  end;
  FreeAndNil(ConnectionChecker);

  if not Connected then
    exit;

  VersionLoader := TIdHttp.Create(nil);
  LogStream := TStringStream.Create('', TEncoding.Unicode);
  VersionLoader.Request.UserAgent := 'Naraeon SSD Tools';
  ServerVersion :=
    VersionLoader.Get('http://www.naraeon.net/SSDTools/latestSSDTools.htm');
  VersionLoader.Request.CharSet := 'Unicode';
  VersionLoader.Get('http://www.naraeon.net/SSDTools/ChangeLog' +
                      LangFileName + '.htm', LogStream);
  ChangeLog := LogStream.DataString;

  FreeAndNil(LogStream);

  ClientVer := ExtractVersion(CurrentVersion);
  ServerVer := ExtractVersion(ServerVersion);

  ServerVersion := IntToStr(ServerVer.FMajorVer) + '.' +
                    IntToStr(ServerVer.FMinorVer) + '.' +
                    IntToStr(ServerVer.FBuildVer);

  if ClientVer >= ServerVer then
  begin
    FreeAndNil(VersionLoader);
    exit;
  end;

  CurrChr := 0;
  while CurrChr < Length(ChangeLog) do
  begin
    Inc(CurrChr, 1);
    if ChangeLog[CurrChr] <> '-' then
      Continue;

    CurrVer := CurrChr;
    if Length(ChangeLog) < (CurrChr + 8) then
      Continue;

    SMajorVer := '';
    SMinorVer := '';
    SBuildVer := '';
    CurrStat := 0;
    Inc(CurrChr, 1);

    while ChangeLog[CurrChr] <> '-' do
    begin
      if ChangeLog[CurrChr] = ' ' then
      begin
        Inc(CurrChr, 1);
        Continue;
      end;

      if ChangeLog[CurrChr] = '.' then
      begin
        CurrStat := CurrStat + 1;
        if CurrStat = 3 then
          break;

        Inc(CurrChr, 1);
        Continue;
      end;

      case CurrStat of
        0: SMajorVer := SMajorVer + ChangeLog[CurrChr];
        1: SMinorVer := SMinorVer + ChangeLog[CurrChr];
        2: SBuildVer := SBuildVer + ChangeLog[CurrChr];
      end;
      Inc(CurrChr, 1);
    end;

    if (ClientVer.FMajorVer >= StrToInt(SMajorVer)) and
       (ClientVer.FMinorVer >= StrToInt(SMinorVer)) and
       (ClientVer.FBuildVer >= StrToInt(SBuildVer)) or
       (CurrChr > 300) then
    begin
      CurrChr := CurrVer - 1;
      break;
    end;
  end;
  Dec(CurrChr, 1);

  while Ord(ChangeLog[CurrChr]) = $A do
  begin
    Dec(CurrChr, 1);
  end;

  Synchronize(fMain.ProgressDownload);
  FreeAndNil(VersionLoader);
end;
end.
