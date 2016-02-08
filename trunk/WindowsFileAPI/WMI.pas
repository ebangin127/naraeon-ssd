unit WMI;

interface

uses
  Classes, Windows, ActiveX, ComObj;

type
  TWMIConnection = class
  private
    Owner: TThread;
    WMIObject: IDispatch;
    ContextToBindMoniker: IBindCtx;
    DefaultMoniker: IMoniker;
    function GetDefaultMonikerFromObjectPath(ObjectPath: String;
      BindableContext: IBindCtx): IMoniker;
    function GetMonikerBindableContext: IBindCtx;
    function GetLocalhostWMIRepositoryURI: String;
    function GetReferredObjectByMoniker(DefaultMoniker: IMoniker;
      BindableContext: IBindCtx): IDispatch;
    procedure SetMoniker;
  public
    function IsMyConnection: Boolean;
    function GetWMIConnection: IDispatch;
    constructor Create;
  end;

var
  WMIConnection: TWMIConnection;

implementation

{ TWMIConnection }


procedure TWMIConnection.SetMoniker;
begin
  ContextToBindMoniker := GetMonikerBindableContext;
  DefaultMoniker := GetDefaultMonikerFromObjectPath(
    GetLocalhostWMIRepositoryURI, ContextToBindMoniker);
end;
constructor TWMIConnection.Create;
begin
  try
    SetMoniker;
  except
    on EOleSysError do
    begin
      CoInitialize(nil);
      SetMoniker;
    end;
    else raise;
  end;
  WMIObject :=
    GetReferredObjectByMoniker(
      DefaultMoniker,
      ContextToBindMoniker);
  Owner := TThread.CurrentThread;
end;

function TWMIConnection.GetMonikerBindableContext: IBindCtx;
const
  ReservedAndMustBeZero = 0;
begin
  OleCheck(CreateBindCtx(ReservedAndMustBeZero, result));
end;

function TWMIConnection.GetDefaultMonikerFromObjectPath
  (ObjectPath: String; BindableContext: IBindCtx): IMoniker;
var
  LengthOfURISuccessfullyParsed: Integer;
begin
  OleCheck(
    MkParseDisplayName(BindableContext,
      PWideChar(ObjectPath),
      LengthOfURISuccessfullyParsed,
      result));
end;

function TWMIConnection.GetLocalhostWMIRepositoryURI: String;
const
  WMIService = 'winmgmts:\\';
  Localhost = 'localhost\';
  WMIRepositoryPrefix = 'root\cimv2';
begin
  result := WMIService + Localhost + WMIRepositoryPrefix;
end;

function TWMIConnection.GetReferredObjectByMoniker
  (DefaultMoniker: IMoniker; BindableContext: IBindCtx): IDispatch;
begin
  OleCheck(
    DefaultMoniker.BindToObject(BindableContext, nil, IUnknown, result));
end;

function TWMIConnection.GetWMIConnection: IDispatch;
begin
  result := WMIObject;
end;

function TWMIConnection.IsMyConnection: Boolean;
begin
  result := TThread.CurrentThread = Owner;
end;

initialization
  WMIConnection := TWMIConnection.Create;
finalization
  WMIConnection.Free;
end.
