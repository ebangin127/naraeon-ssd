unit Web.HTTPS;

interface

uses
  SysUtils, Classes, IdHttp, IdURI, IdSSLOpenSSL,
  Web.HTTP;

type
  THTTPSWeb = class(THTTPWeb)
  private
    SSLIoHandler: TIdSSLIOHandlerSocketOpenSSL;
    procedure SetSSLIoHandler;
  public
    constructor Create;
    destructor Destroy; override;
    function GetToStringList(const PathToGet: String): TStringList; override;
    function GetToStringStream(const PathToGet: String): TStringStream; override;
  end;

implementation

{ THTTPWeb }

constructor THTTPSWeb.Create;
const
  Timeout = 1500;
begin
  SSLIoHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  SSLIoHandler.ConnectTimeout := Timeout;
  SSLIoHandler.ReadTimeout := Timeout;
end;

destructor THTTPSWeb.Destroy;
begin
  FreeAndNil(SSLIoHandler);
  inherited;
end;

procedure THTTPSWeb.SetSSLIoHandler;
begin
  Connector.IOHandler := SSLIoHandler;
end;

function THTTPSWeb.GetToStringList(const PathToGet: String): TStringList;
begin
  SetSSLIoHandler;
  result := inherited GetToStringList(PathToGet);
end;

function THTTPSWeb.GetToStringStream(const PathToGet: String): TStringStream;
begin
  SetSSLIoHandler;
  result := inherited GetToStringStream(PathToGet);
end;

end.
