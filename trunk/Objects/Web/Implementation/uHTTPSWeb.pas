unit uHTTPSWeb;

interface

uses
  SysUtils, Classes, IdHttp, IdURI, IdSSLOpenSSL,
  uHTTPWeb;

type
  THTTPSWeb = class(THTTPWeb)
  private
    SSLIoHandler: TIdSSLIOHandlerSocketOpenSSL;
    procedure SetSSLIoHandler;
  public
    constructor Create;
    destructor Destroy; override;
    function GetToStringList(PathToGet: String): TStringList; override;
    function GetToStringStream(PathToGet: String): TStringStream; override;
  end;

implementation

{ THTTPWeb }

constructor THTTPSWeb.Create;
begin
  SSLIoHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
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

function THTTPSWeb.GetToStringList(PathToGet: String): TStringList;
begin
  SetSSLIoHandler;
  result := inherited GetToStringList(PathToGet);
end;

function THTTPSWeb.GetToStringStream(PathToGet: String): TStringStream;
begin
  SetSSLIoHandler;
  result := inherited GetToStringStream(PathToGet);
end;

end.
