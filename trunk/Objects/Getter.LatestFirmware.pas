unit Getter.LatestFirmware;

interface

uses
  Classes, Windows, SysUtils, Generics.Collections,
  Web.HTTP;

type
  TFirmwareVersion = (NotMine, OldVersion, NewVersion, NotFound);

  TFirmwareQuery = record
    Model, Firmware: String;
    class operator Equal(const A, B: TFirmwareQuery): Boolean;
  end;

  TFirmwareQueryResult = record
    CurrentVersion: TFirmwareVersion;
    LatestVersion: String;
    FirmwarePath: String;
  end;

  TFirmwareGetter = class
  public
    constructor Create;
    destructor Destroy; override;
    function CheckFirmware(const Query: TFirmwareQuery): TFirmwareQueryResult;
  private
    type
      TCacheLine = record
        Query: TFirmwareQuery;
        Result: TFirmwareQueryResult;
      end;
      TFirmwareCache = class
      public
        constructor Create;
        destructor Destroy; override;
        function CheckFirmware(const Query: TFirmwareQuery):
          TFirmwareQueryResult;
        procedure ApplyQueryResult(const Query: TFirmwareQuery;
          const Result: TFirmwareQueryResult);
      private
        Cache: TList<TCacheLine>;
      end;
  private
    FirmwareCache: TFirmwareCache;
    HTTPWeb: THTTPWeb;
    function CheckFirmwareByWebAndApplyToCache(const Query: TFirmwareQuery):
      TFirmwareQueryResult;
    function SendQueryToServerAndGetResult(
      const Query: TFirmwareQuery): TFirmwareQueryResult;
    function BuildURIByQuery(const Query: TFirmwareQuery): String;
    function StringListToQueryResult(
      const StringList: TStringList): TFirmwareQueryResult;
    function TryToQueryFirmware(const Query: TFirmwareQuery):
      TStringList;
  end;

implementation

{ TFirmwareGetter }

constructor TFirmwareGetter.Create;
begin
  FirmwareCache := TFirmwareCache.Create;
  HTTPWeb := THTTPWeb.Create;
end;

destructor TFirmwareGetter.Destroy;
begin
  FreeAndNil(FirmwareCache);
  FreeAndNil(HTTPWeb);
  inherited;
end;

function TFirmwareGetter.BuildURIByQuery(const Query: TFirmwareQuery):
  String;
begin
  exit(
    'http://nstfirmware.naraeon.net/NSTFirmwareCheck.php?' +
    'Model=' + Query.Model + '&' +
    'Firmware=' + Query.Firmware);
end;

function TFirmwareGetter.StringListToQueryResult(const StringList: TStringList):
  TFirmwareQueryResult;
var
  CurrentVersionInInteger: Integer;
begin
  ZeroMemory(@result, SizeOf(result));
  if StringList.Count < 3 then
    exit;

  if not TryStrToInt(StringList[0], CurrentVersionInInteger) then
  begin
    result.CurrentVersion := TFirmwareVersion.NotMine;
    exit;
  end;

  result.CurrentVersion := TFirmwareVersion(CurrentVersionInInteger);
  if result.CurrentVersion <> TFirmwareVersion.NotMine then
  begin
    result.LatestVersion := StringList[1];
    result.FirmwarePath := StringList[2];
  end;
end;

function TFirmwareGetter.TryToQueryFirmware(const Query: TFirmwareQuery):
  TStringList;
begin
  try
    result := HTTPWeb.GetToStringList(BuildURIByQuery(Query));
  except
    result := TStringList.Create;
  end;
end;

function TFirmwareGetter.SendQueryToServerAndGetResult(
  const Query: TFirmwareQuery): TFirmwareQueryResult;
var
  ResultInStringList: TStringList;
begin
  ResultInStringList := TryToQueryFirmware(Query);
  try
    result := StringListToQueryResult(ResultInStringList);
  finally
    FreeAndNil(ResultInStringList);
  end;
end;

function TFirmwareGetter.CheckFirmwareByWebAndApplyToCache
  (const Query: TFirmwareQuery): TFirmwareQueryResult;
begin
  ZeroMemory(@result, SizeOf(result));
  result := SendQueryToServerAndGetResult(Query);
  if result.LatestVersion <> '' then
    FirmwareCache.ApplyQueryResult(Query, result);
end;

function TFirmwareGetter.CheckFirmware(const Query: TFirmwareQuery):
  TFirmwareQueryResult;
begin
  result := FirmwareCache.CheckFirmware(Query);
  if result.CurrentVersion <> TFirmwareVersion.NotMine then
    exit(result)
  else
    exit(CheckFirmwareByWebAndApplyToCache(Query));
end;

{ TFirmwareCache }

procedure TFirmwareGetter.TFirmwareCache.ApplyQueryResult(
  const Query: TFirmwareQuery; const Result: TFirmwareQueryResult);
var
  CacheLine: TCacheLine;
begin
  CacheLine.Query := Query;
  CacheLine.Result := Result;
  Cache.Add(CacheLine);
end;

function TFirmwareGetter.TFirmwareCache.CheckFirmware(
  const Query: TFirmwareQuery): TFirmwareQueryResult;
var
  CurrentCacheLine: TCacheLine;
begin
  result.CurrentVersion := TFirmwareVersion.NotMine;
  for CurrentCacheLine in Cache do
    if CurrentCacheLine.Query = Query then
      exit(CurrentCacheLine.Result);
end;

constructor TFirmwareGetter.TFirmwareCache.Create;
begin
  Cache := TList<TCacheLine>.Create;
end;

destructor TFirmwareGetter.TFirmwareCache.Destroy;
begin
  FreeAndNil(Cache);
  inherited;
end;

{ TFirmwareQuery }

class operator TFirmwareQuery.Equal(const A, B: TFirmwareQuery): Boolean;
begin
  result :=
    (A.Model = B.Model) and
    (A.Firmware = B.Firmware);
end;

end.
