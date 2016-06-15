unit Web.HTTP;

interface

uses
  SysUtils, Classes, IdComponent, IdExceptionCore, IdHttp, IdURI,
  IdStack, Web;

type
  THTTPWeb = class(TWeb)
  private
    InnerConnector: TIdHttp;
    InnerEncodedURI: String;
    procedure SetEncodedURIByPath(const PathToGet: String);
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
    function GetToStringList(const PathToGet: String): TStringList; override;
    function GetToStringStream(const PathToGet: String):
      TStringStream; override;
    function Head(const PathToGet: String): Boolean; virtual;
    procedure SetOnWorkHandler(const OnWorkHandler: TWorkEvent);
  end;

implementation

{ THTTPWeb }

constructor THTTPWeb.Create;
const
  Timeout = 1500;
begin
  inherited;
  InnerConnector := TIdHttp.Create(nil);
  InnerConnector.ReadTimeout := Timeout;
  InnerConnector.ConnectTimeout := Timeout;
end;

destructor THTTPWeb.Destroy;
begin
  FreeAndNil(InnerConnector);
  inherited;
end;

procedure THTTPWeb.SetEncodedURIByPath(const PathToGet: String);
begin
  InnerEncodedURI := TIdURI.URLEncode(PathToGet);
end;

procedure THTTPWeb.SetOnWorkHandler(const OnWorkHandler: TWorkEvent);
begin
  Connector.OnWork := OnWorkHandler;
end;

function THTTPWeb.GetFromEncodedURIToStringList: TStringList;
begin
  result := TStringList.Create;
  try
    result.Text := InnerConnector.Get(InnerEncodedURI);
  except
    on E: EIdReadTimeout do;
    on E: EIdConnectTimeout do;
    on E: EIdNotASocket do;
    else raise;
  end;
end;

function THTTPWeb.GetFromEncodedURIToStringStream: TStringStream;
begin
  result := TStringStream.Create('', TEncoding.Unicode);
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

function THTTPWeb.Head(const PathToGet: String): Boolean;
begin
  if not IsWebAccessible then
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

function THTTPWeb.GetToStringList(const PathToGet: String): TStringList;
begin
  if not IsWebAccessible then
    exit(TStringList.Create);

  SetEncodedURIByPath(PathToGet);
  SetRequestHeader;
  result := GetFromEncodedURIToStringList;
end;

function THTTPWeb.GetToStringStream(const PathToGet: String): TStringStream;
begin
  if not IsWebAccessible then
    exit(TStringStream.Create);

  SetEncodedURIByPath(PathToGet);
  SetRequestHeader;
  result := GetFromEncodedURIToStringStream;
end;

end.
