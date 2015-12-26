unit Web.HTTP;

interface

uses
  SysUtils, Classes, IdComponent, IdHttp, IdURI,
  Web;

type
  THTTPWeb = class(TWeb)
  private
    InnerConnector: TIdHttp;
    InnerEncodedURI: String;
    procedure SetEncodedURIByPath(PathToGet: String);
    function GetFromEncodedURIToStringList: TStringList;
    procedure SetRequestHeader;
    function HeadByEncodedURI: Boolean;
    function GetFromEncodedURIToStringStream: TStringStream;
  protected
    property Connector: TIdHttp read InnerConnector;
    property EncodedURI: String read InnerEncodedURI write SetEncodedURIByPath;
  public
    constructor Create;
    destructor Destroy; override;
    function GetToStringList(PathToGet: String): TStringList; override;
    function GetToStringStream(PathToGet: String): TStringStream; override;
    function Head(PathToGet: String): Boolean; virtual;
    procedure SetOnWorkHandler(OnWorkHandler: TWorkEvent);
  end;

implementation

{ THTTPWeb }

constructor THTTPWeb.Create;
begin
  InnerConnector := TIdHttp.Create(nil);
end;

destructor THTTPWeb.Destroy;
begin
  FreeAndNil(InnerConnector);
  inherited;
end;

procedure THTTPWeb.SetEncodedURIByPath(PathToGet: String);
begin
  InnerEncodedURI := TIdURI.URLEncode(PathToGet);
end;

procedure THTTPWeb.SetOnWorkHandler(OnWorkHandler: TWorkEvent);
begin
  Connector.OnWork := OnWorkHandler;
end;

function THTTPWeb.GetFromEncodedURIToStringList: TStringList;
begin
  result := TStringList.Create;
  result.Text := InnerConnector.Get(InnerEncodedURI);
end;

function THTTPWeb.GetFromEncodedURIToStringStream: TStringStream;
begin
  result := TStringStream.Create('', TEncoding.Unicode);;
  InnerConnector.Get(InnerEncodedURI, result);
end;

function THTTPWeb.HeadByEncodedURI: Boolean;
begin
  try
    InnerConnector.HandleRedirects := True;
    InnerConnector.Head(InnerEncodedURI);
    result := (InnerConnector.response.ResponseCode = 200);
  except
    result := false;
  end;
end;

function THTTPWeb.Head(PathToGet: String): Boolean;
begin
  if IsWebAccessible = false then
    exit(false);

  SetEncodedURIByPath(PathToGet);
  SetRequestHeader;
  result := HeadByEncodedURI;
end;

procedure THTTPWeb.SetRequestHeader;
begin
  InnerConnector.Request.UserAgent := UserAgent;
  InnerConnector.Request.CharSet := CharacterSet;
end;

function THTTPWeb.GetToStringList(PathToGet: String): TStringList;
begin
  if IsWebAccessible = false then
    exit(TStringList.Create);

  SetEncodedURIByPath(PathToGet);
  SetRequestHeader;
  result := GetFromEncodedURIToStringList;
end;

function THTTPWeb.GetToStringStream(PathToGet: String): TStringStream;
begin
  if IsWebAccessible = false then
    exit(TStringStream.Create);

  SetEncodedURIByPath(PathToGet);
  SetRequestHeader;
  result := GetFromEncodedURIToStringStream;
end;

end.
