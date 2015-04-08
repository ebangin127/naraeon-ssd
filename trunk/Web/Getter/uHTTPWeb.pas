unit uHTTPWeb;

interface

uses
  SysUtils, Classes, IdHttp, IdURI,
  uWeb;

type
  THTTPWeb = class(TWeb)
  private
    Connector: TIdHttp;
    EncodedURI: String;
    procedure SetEncodedURIByPath(PathToGet: String);
    function GetFromEncodedURI: TStringList;
    procedure SetRequestHeader;
  public
    constructor Create;
    destructor Destroy; override;
    function Get(PathToGet: String): TStringList; override;
  end;

implementation

{ THTTPWeb }

constructor THTTPWeb.Create;
begin
  Connector := TIdHttp.Create(nil);
end;

destructor THTTPWeb.Destroy;
begin
  FreeAndNil(Connector);
  inherited;
end;

procedure THTTPWeb.SetEncodedURIByPath(PathToGet: String);
begin
  EncodedURI := TIdURI.URLEncode(PathToGet);
end;

function THTTPWeb.GetFromEncodedURI: TStringList;
begin
  result := TStringList.Create;
  result.Text := Connector.Get(EncodedURI);
end;

procedure THTTPWeb.SetRequestHeader;
begin
  Connector.Request.UserAgent := UserAgent;
  Connector.Request.CharSet := CharacterSet;
end;

function THTTPWeb.Get(PathToGet: String): TStringList;
begin
  SetEncodedURIByPath(PathToGet);
  SetRequestHeader;
  result := GetFromEncodedURI;
end;

end.
