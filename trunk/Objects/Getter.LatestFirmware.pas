unit Getter.LatestFirmware;

interface

uses
  Classes, Windows, SysUtils, Generics.Collections,
  Web.HTTP;

type
  TFirmwareVersion = (NotMine, OldVersion, NewVersion, NotFound);

  TFirmwareQuery = record
    Model, Firmware: String;
    class operator Equal(A, B: TFirmwareQuery): Boolean;
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
    function CheckFirmware(Query: TFirmwareQuery): TFirmwareQueryResult;

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

        function CheckFirmware(Query: TFirmwareQuery): TFirmwareQueryResult;
        procedure ApplyQueryResult(Query: TFirmwareQuery;
          Result: TFirmwareQueryResult);
      private
        Cache: TList<TCacheLine>;
      end;

  private
    FirmwareCache: TFirmwareCache;
    HTTPWeb: THTTPWeb;
    function CheckFirmwareByWebAndApplyToCache(Query: TFirmwareQuery):
      TFirmwareQueryResult;
    function SendQueryToServerAndGetResult(
      Query: TFirmwareQuery): TFirmwareQueryResult;
    function BuildURIByQuery(Query: TFirmwareQuery): String;
    function StringListToQueryResult(
      StringList: TStringList): TFirmwareQueryResult;
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

function TFirmwareGetter.BuildURIByQuery(Query: TFirmwareQuery):
  String;
begin
  exit(
    'http://nstfirmware.naraeon.net/NSTFirmwareCheck.php?' +
    'Model=' + Query.Model + '&' +
    'Firmware=' + Query.Firmware);
end;

function TFirmwareGetter.StringListToQueryResult(StringList: TStringList):
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

function TFirmwareGetter.SendQueryToServerAndGetResult(Query: TFirmwareQuery):
  TFirmwareQueryResult;
var
  ReturnedResultFromGet: TStringList;
begin
  try
    ReturnedResultFromGet := HTTPWeb.GetToStringList(BuildURIByQuery(Query));
    result := StringListToQueryResult(ReturnedResultFromGet);
  finally
    FreeAndNil(ReturnedResultFromGet);
  end;
end;

function TFirmwareGetter.CheckFirmwareByWebAndApplyToCache
  (Query: TFirmwareQuery): TFirmwareQueryResult;
begin
  ZeroMemory(@result, SizeOf(result));
  result := SendQueryToServerAndGetResult(Query);
  if result.LatestVersion <> '' then
    FirmwareCache.ApplyQueryResult(Query, result);
end;

function TFirmwareGetter.CheckFirmware(Query: TFirmwareQuery):
  TFirmwareQueryResult;
begin
  result := FirmwareCache.CheckFirmware(Query);
  if result.CurrentVersion <> TFirmwareVersion.NotMine then
    exit(result)
  else
    exit(CheckFirmwareByWebAndApplyToCache(Query));
end;

{ TFirmwareCache }

procedure TFirmwareGetter.TFirmwareCache.ApplyQueryResult(Query: TFirmwareQuery;
  Result: TFirmwareQueryResult);
var
  CacheLine: TCacheLine;
begin
  CacheLine.Query := Query;
  CacheLine.Result := Result;
  Cache.Add(CacheLine);
end;

function TFirmwareGetter.TFirmwareCache.CheckFirmware(Query: TFirmwareQuery):
  TFirmwareQueryResult;
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

class operator TFirmwareQuery.Equal(A, B: TFirmwareQuery): Boolean;
begin
  result :=
    (A.Model = B.Model) and
    (A.Firmware = B.Firmware);
end;

end.
