unit WMI;

interface

uses
  Classes, Windows, ActiveX, ComObj;

const
  WMIService = 'winmgmts:\\';
  Localhost = 'localhost\';
  WMIRepositoryPrefix = 'root\cimv2';

type
  TWMIConnection = class
  private
    Owner: TThread;
    WMIObject: OleVariant;
    ContextToBindMoniker: IBindCtx;
    DefaultMoniker: IMoniker;
    function GetDefaultMonikerFromObjectPath(const ObjectPath: String;
      BindableContext: IBindCtx): IMoniker;
    function GetMonikerBindableContext: IBindCtx;
    function GetLocalhostWMIRepositoryURI: String;
    function GetReferredObjectByMoniker(DefaultMoniker: IMoniker;
      BindableContext: IBindCtx): IDispatch;
    procedure SetMoniker(const Path: string);
  public
    function IsMyConnection: Boolean;
    function GetWMIConnection: OleVariant;
    constructor Create;
    constructor CreateWithNewPath(const Path: String);
  end;

var
  WMIConnection: TWMIConnection;

implementation

{ TWMIConnection }


procedure TWMIConnection.SetMoniker(const Path: string);
begin
  ContextToBindMoniker := GetMonikerBindableContext;
  DefaultMoniker := GetDefaultMonikerFromObjectPath(Path, ContextToBindMoniker);
end;

constructor TWMIConnection.Create;
begin
  CreateWithNewPath(GetLocalhostWMIRepositoryURI);
end;

function TWMIConnection.GetMonikerBindableContext: IBindCtx;
const
  ReservedAndMustBeZero = 0;
begin
  OleCheck(CreateBindCtx(ReservedAndMustBeZero, result));
end;

constructor TWMIConnection.CreateWithNewPath(const Path: String);
begin
  try
    SetMoniker(Path);
  except
    on E: EOleSysError do
    begin
      CoInitialize(nil);
      SetMoniker(Path);
    end;
    else raise;
  end;
  WMIObject :=
    GetReferredObjectByMoniker(
      DefaultMoniker,
      ContextToBindMoniker);
  Owner := TThread.CurrentThread;
end;

function TWMIConnection.GetDefaultMonikerFromObjectPath
  (const ObjectPath: String; BindableContext: IBindCtx): IMoniker;
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
begin
  result := WMIService + Localhost + WMIRepositoryPrefix;
end;

function TWMIConnection.GetReferredObjectByMoniker
  (DefaultMoniker: IMoniker; BindableContext: IBindCtx): IDispatch;
begin
  OleCheck(
    DefaultMoniker.BindToObject(BindableContext, nil, IUnknown, result));
end;

function TWMIConnection.GetWMIConnection: OleVariant;
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
