unit uHTTPSWeb;

interface

uses
  SysUtils, Classes, IdHttp, IdURI, IdSSLOpenSSL,
  uWeb;

type
  THTTPSWeb = class(TWeb)
  private
    SSLIoHandler: TIdSSLIOHandlerSocketOpenSSL;
    Connector: TIdHttp;
    EncodedURI: String;
    procedure SetEncodedURIByPath(PathToGet: String);
    function GetFromEncodedURI: TStringList;
    procedure SetRequestHeader;
    procedure SetSSLIoHandler;
  public
    constructor Create;
    destructor Destroy; override;
    function Get(PathToGet: String): TStringList; override;
  end;

implementation

{ THTTPWeb }

constructor THTTPSWeb.Create;
begin
  Connector := TIdHttp.Create(nil);
  SSLIoHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
end;

destructor THTTPSWeb.Destroy;
begin
  FreeAndNil(Connector);
  FreeAndNil(SSLIoHandler);
  inherited;
end;

procedure THTTPSWeb.SetEncodedURIByPath(PathToGet: String);
begin
  EncodedURI := TIdURI.URLEncode(PathToGet);
end;

function THTTPSWeb.GetFromEncodedURI: TStringList;
begin
  result := TStringList.Create;
  result.Text := Connector.Get(EncodedURI);
end;

procedure THTTPSWeb.SetRequestHeader;
begin
  Connector.Request.UserAgent := UserAgent;
  Connector.Request.CharSet := CharacterSet;
end;

procedure THTTPSWeb.SetSSLIoHandler;
begin
  Connector.IOHandler := SSLIoHandler;
end;

function THTTPSWeb.Get(PathToGet: String): TStringList;
begin
  SetSSLIoHandler;
  SetRequestHeader;

  SetEncodedURIByPath(PathToGet);
  result := GetFromEncodedURI;
end;

end.
