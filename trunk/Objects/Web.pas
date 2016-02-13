unit Web;

interface

uses
  Windows, Classes, WinInet;

type
  TWeb = class abstract
  private
    function IsWebAccessibleInConnectedState(
      const GetFunctionResult: Boolean; const InternetConnectedState: DWORD):
      Boolean;
    function IsWebAccessConfigured(const InternetConnectedState: DWORD): Boolean;
  public
    function GetToStringList(const PathToGet: String):
      TStringList; virtual; abstract;
    function GetToStringStream(const PathToGet: String): TStringStream;
      virtual; abstract;
    function IsWebAccessible: Boolean;
  protected
    const
      UserAgent = 'Naraeon SSD Tools';
      CharacterSet = 'Unicode';
  end;

implementation

{ TWeb }

function TWeb.IsWebAccessConfigured(
  const InternetConnectedState: DWORD): Boolean;
begin
  result :=
    ((InternetConnectedState and INTERNET_CONNECTION_OFFLINE) = 0) and
    (InternetConnectedState <> 0);
end;

function TWeb.IsWebAccessibleInConnectedState(
  const GetFunctionResult: Boolean; const InternetConnectedState: DWORD):
  Boolean;
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
