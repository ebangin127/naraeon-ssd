unit Web;

interface

uses
  Windows, Classes, WinInet;

type
  TWeb = class abstract
  private
    function IsWebAccessibleInConnectedState
      (GetFunctionResult: Boolean; InternetConnectedState: DWORD): Boolean;
    function IsWebAccessConfigured(InternetConnectedState: DWORD): Boolean;
  public
    function GetToStringList(PathToGet: String): TStringList; virtual; abstract;
    function GetToStringStream(PathToGet: String): TStringStream;
      virtual; abstract;
    function IsWebAccessible: Boolean;
  protected
    const
      UserAgent = 'Naraeon SSD Tools';
      CharacterSet = 'Unicode';
  end;

implementation

{ TWeb }

function TWeb.IsWebAccessConfigured
  (InternetConnectedState: DWORD): Boolean;
begin
  result :=
    ((InternetConnectedState and INTERNET_CONNECTION_OFFLINE) = 0) and
    (InternetConnectedState <> 0);
end;

function TWeb.IsWebAccessibleInConnectedState
  (GetFunctionResult: Boolean; InternetConnectedState: DWORD): Boolean;
begin
  result :=
    GetFunctionResult and IsWebAccessConfigured(InternetConnectedState);
end;

function TWeb.IsWebAccessible: Boolean;
var
  InternetConnectedState: DWORD;
  GetFunctionResult: Boolean;
begin
  GetFunctionResult := InternetGetConnectedState(@InternetConnectedState, 0);
  result :=
    IsWebAccessibleInConnectedState(GetFunctionResult, InternetConnectedState);
end;

end.
