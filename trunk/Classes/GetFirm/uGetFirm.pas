unit uGetFirm;

interface

uses Classes, SysUtils, Math, Dialogs, Windows, Generics.Collections,
    IdBaseComponent, IdRawBase, IdRawClient, IdHttp, IdURI,
    uDiskFunctions, IdIcmpClient, uLanguageSettings;

type
  TFirmVersion = (NOT_MINE, OLD_VERSION, NEW_VERSION, NOT_FOUND);

  TGetFirmQuery = record
    Model, Firmware: String;
  end;

  TGetFirmResult = record
    CurrVersion: TFirmVersion;
    LatestVersion: String;
    FirmFileName: String;
  end;
  PTGetFirmResult = ^TGetFirmResult;

  TCacheLine = record
    Query: TGetFirmQuery;
    Result: TGetFirmResult;
  end;

  TFirmCache = class(TList<TCacheLine>)
  public
    constructor Create;
    function Find(Model, Firmware: String): TGetFirmResult;
  end;

  TGetFirm = class
  protected
    VersionLoader: TIdHttp;
    Model, Firmware: String;

  public
    constructor Create(iModel, iFirm: String);
    function GetVersion: TGetFirmResult;

    class procedure CreateCache;
    class procedure DestroyCache;
  end;

implementation

var
  Cache: TFirmCache;

constructor TGetFirm.Create(iModel, iFirm: String);
begin
  Model := iModel;
  Firmware := iFirm;
end;

class procedure TGetFirm.CreateCache;
begin
  Cache := TFirmCache.Create;
end;

class procedure TGetFirm.DestroyCache;
begin
  FreeAndNil(Cache);
end;

function TGetFirm.GetVersion: TGetFirmResult;
var
  FirmChkContents: TStringList;
  CurrCacheLine: TCacheLine;
  CacheResult: TGetFirmResult;
begin
  //캐시에서 먼저 찾음
  CacheResult := Cache.Find(Model, Firmware);

  if CacheResult.CurrVersion <> NOT_FOUND then
    exit(CacheResult);

  //없으면 만들기
  CurrCacheLine.Query.Model := Model;
  CurrCacheLine.Query.Firmware := Firmware;

  FillChar(result, SizeOf(result), #0);
  VersionLoader := TIdHttp.Create(nil);
  FirmChkContents := TStringList.Create;

  VersionLoader.Request.UserAgent := 'Naraeon SSD Tools';
  VersionLoader.Request.CharSet := 'Unicode';
  try
    FirmChkContents.Text := VersionLoader.Get(TIdURI.URLEncode(
        'http://nstfirmware.naraeon.net/nst_firmchk.php?' +
        'Model=' + Model + '&' +
        'Firmware=' + Firmware));
    result.CurrVersion := TFirmVersion(StrToInt(FirmChkContents[0]));
    if result.CurrVersion <> NOT_MINE then
    begin
      result.LatestVersion := FirmChkContents[1];
      result.FirmFileName := FirmChkContents[2];
    end;

    CurrCacheLine.Result := result;
    Cache.Add(CurrCacheLine);
  finally
    FreeAndNil(FirmChkContents);
    FreeAndNil(VersionLoader);
  end;
end;

{ TFirmCache }

constructor TFirmCache.Create;
begin
  inherited Create;
end;

function TFirmCache.Find(Model, Firmware: String): TGetFirmResult;
var
  CurrLine: TCacheLine;
begin
  FillChar(result, SizeOf(result), #0);
  result.CurrVersion := NOT_FOUND;

  for CurrLine in self.List do
  begin
    if (CurrLine.Query.Model = Model) and
       (CurrLine.Query.Firmware = Firmware) then
      exit(CurrLine.Result);
  end;
end;

end.
