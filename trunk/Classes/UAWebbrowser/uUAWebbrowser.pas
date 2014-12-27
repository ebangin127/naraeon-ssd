unit uUAWebbrowser;

interface

uses Classes, SHDocVw, ActiveX;

const
  DISPID_AMBIENT_USERAGENT = -5513;

type
  TUAWebBrowser = class(TWebbrowser, IDispatch)
  private
    FUserAgent: string;
    procedure SetUserAgent(const Value: string);
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
                    Flags: Word; var Params;
                    VarResult, ExcepInfo, ArgErr: Pointer): HRESULT; stdcall;
  public
    property UserAgent: string read FUserAgent write SetUserAgent;
    constructor Create(AOwner: TComponent); virtual;
  end;

implementation

constructor TUAWebBrowser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUserAgent := '';
  SetUserAgent('Naraeon SSD Tools');
end;

function TUAWebBrowser.Invoke(DispID: Integer; const IID: TGUID;
                              LocaleID: Integer; Flags: Word; var Params;
                              VarResult, ExcepInfo, ArgErr: Pointer): HRESULT;
begin
  //check if the DISPID_AMBIENT_USERAGENT flag is being processed and
  //if the User Agent to set is not empty
  if (FUserAgent <> '') and
     (Flags and DISPATCH_PROPERTYGET <> 0) and
     (DispId = DISPID_AMBIENT_USERAGENT) and
     (Assigned(VarResult)) then
  begin
    //set the user agent
    POleVariant(VarResult)^ := FUserAgent + #13#10;
    Result := S_OK; //return S_OK
  end
  else
    Result :=
      inherited Invoke(DispID, IID, LocaleID, Flags, Params,
        VarResult, ExcepInfo, ArgErr); //call the default Invoke method
end;

procedure TUAWebBrowser.SetUserAgent(const Value: string);
var
  Control: IOleControl;
begin
  FUserAgent := Value;
  //the current interface supports IOleControl?
  if DefaultInterface.QueryInterface(IOleControl, Control) = 0 then
    Control.OnAmbientPropertyChange(DISPID_AMBIENT_USERAGENT);
    //call the OnAmbientPropertyChange event
end;
end.
