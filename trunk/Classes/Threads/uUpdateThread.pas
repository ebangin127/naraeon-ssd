unit uUpdateThread;

interface

uses Classes, SysUtils, Math, Dialogs, Windows,
    IdBaseComponent, IdRawBase, IdRawClient, idHttp,
    uDiskFunctions, uTrimCommand, IdIcmpClient, uLanguageSettings,
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
    ConnectionChecker.Head('http://www.naraeon.net/SSDTools/latestSSDTools.htm');
    Connected := (ConnectionChecker.response.ResponseCode = 200);
  except
    Connected := false;
  end;
  FreeAndNil(ConnectionChecker);

  if Connected then
  begin
    FreeAndNil(ConnectionChecker);
    VersionLoader := TIdHttp.Create(nil);
    LogStream := TStringStream.Create('', TEncoding.Unicode);
    VersionLoader.Request.UserAgent := 'Naraeon SSD Tools';
    ServerVersion := VersionLoader.Get('http://www.naraeon.net/SSDTools/latestSSDTools.htm');
    VersionLoader.Request.CharSet := 'Unicode';
    VersionLoader.Get('http://www.naraeon.net/SSDTools/ChangeLog' + LangFileName + '.htm', LogStream);
    ChangeLog := LogStream.DataString;

    FreeAndNil(LogStream);

    ClientVer := ExtractVersion(CurrentVersion);
    ServerVer := ExtractVersion(ServerVersion);

    ServerVersion := IntToStr(ServerVer.FMajorVer) + '.' + IntToStr(ServerVer.FMinorVer) + '.' + IntToStr(ServerVer.FBuildVer);

    if ServerVersion <> CurrentVersion then
    begin
      if  (CurrentVersion < ServerVersion) then
      begin
        CurrChr := 0;
        while CurrChr < Length(ChangeLog) do
        begin
          Inc(CurrChr, 1);
          if ChangeLog[CurrChr] = '-' then
          begin
            CurrVer := CurrChr;
            if Length(ChangeLog) >= (CurrChr + 8) then
            begin
              SMajorVer := '';
              SMinorVer := '';
              SBuildVer := '';
              CurrStat := 0;
              Inc(CurrChr, 1);
              while ChangeLog[CurrChr] <> '-' do
              begin
                if ChangeLog[CurrChr] <> ' ' then
                begin
                  if (ChangeLog[CurrChr] <> '.') and (CurrStat = 0) then
                    SMajorVer := SMajorVer + ChangeLog[CurrChr]
                  else if (ChangeLog[CurrChr] <> '.') and (CurrStat = 1) then
                    SMinorVer := SMinorVer + ChangeLog[CurrChr]
                  else if (ChangeLog[CurrChr] <> '.') and (CurrStat = 2) then
                    SBuildVer := SBuildVer + ChangeLog[CurrChr]
                  else if ChangeLog[CurrChr] = '.' then
                  begin
                    CurrStat := CurrStat + 1;
                    if CurrStat = 3 then
                      break;
                  end;
                end;
                Inc(CurrChr, 1);
              end;
              if (ClientVer.FMajorVer >= StrToInt(SMajorVer)) and (ClientVer.FMinorVer >= StrToInt(SMinorVer)) and
                  (ClientVer.FBuildVer >= StrToInt(SBuildVer)) or (CurrChr > 300) then
              begin
                CurrChr := CurrVer - 1;
                break;
              end;
            end;
          end;
        end;
        Dec(CurrChr, 1);

        while Ord(ChangeLog[CurrChr]) = $A do
        begin
          Dec(CurrChr, 1);
        end;

        Synchronize(fMain.ProgressDownload);
      end;
    end;
    FreeAndNil(VersionLoader);
  end;
end;
end.
