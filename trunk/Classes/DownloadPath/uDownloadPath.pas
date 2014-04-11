unit uDownloadPath;

interface

uses
  SysUtils, IdHttp;

type
  TDownloadFileType = (dftPlain, dftGetFromWeb);
  TDownloadFile = record
    FFileAddress: String;
    FBaseAddress: String;
    FPostAddress: String;
    FType: TDownloadFileType;
  end;

function GetAddressFromWeb(const Address: String): String;
function GetDownloadPath(const Input: TDownloadFile): String;

implementation

function GetAddressFromWeb(const Address: String): String;
var
  AddrGetter: TIdHttp;
begin
  result := '';
  AddrGetter := TIdHttp.Create;
  AddrGetter.Request.UserAgent := 'Naraeon SSD Tools';
  try
    result := AddrGetter.Get(Address);
  finally
    FreeAndNil(AddrGetter);
  end;
end;

function GetDownloadPath(const Input: TDownloadFile): String;
begin
  result := Input.FBaseAddress;
  case Input.FType of
    dftPlain:
      begin
        result := result + Input.FFileAddress;
      end;
    dftGetFromWeb:
      begin
        result := result + GetAddressFromWeb(Input.FFileAddress);
      end;
  end;
  result := result + Input.FPostAddress;
end;

end.
