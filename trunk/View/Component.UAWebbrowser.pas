unit Component.UAWebbrowser;

interface

uses Classes, SHDocVw, ActiveX;


type
  TUAWebBrowser = class(TWebbrowser, IDispatch)
  private
    const
      DISPID_AMBIENT_USERAGENT = -5513;
      
  private
    FUserAgent: string;
    BrowserAmbient: IOleControl;
    
    function QueryBrowserAmbientAndIfAvailableSetIt: Boolean;
    procedure SetUserAgent(const UserAgentToSet: string);
    
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
                    Flags: Word; var Params;
                    VarResult, ExcepInfo, ArgErr: Pointer): HRESULT; stdcall;
                    
  public
    property UserAgent: string read FUserAgent write SetUserAgent;
    constructor Create(AOwner: TComponent); override;
    
  end;

implementation

constructor TUAWebBrowser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  UserAgent := '';
  SetUserAgent('Naraeon SSD Tools');
end;

function TUAWebBrowser.Invoke(DispID: Integer; const IID: TGUID;
                              LocaleID: Integer; Flags: Word; var Params;
                              VarResult, ExcepInfo, ArgErr: Pointer): HRESULT;
begin
  if (UserAgent <> '') and
     (Flags and DISPATCH_PROPERTYGET <> 0) and
     (DispId = DISPID_AMBIENT_USERAGENT) and
     (Assigned(VarResult)) then
  begin
    POleVariant(VarResult)^ := UserAgent + #13#10;
    exit(S_OK);
  end;
  
  result :=
    inherited Invoke(DispID, IID, LocaleID, Flags, Params,
      VarResult, ExcepInfo, ArgErr);
end;

function TUAWebBrowser.QueryBrowserAmbientAndIfAvailableSetIt: Boolean;
begin
  result := DefaultInterface.QueryInterface(IOleControl, BrowserAmbient) = 0;
end;

procedure TUAWebBrowser.SetUserAgent(const UserAgentToSet: string);
begin
  FUserAgent := UserAgentToSet;
  if QueryBrowserAmbientAndIfAvailableSetIt then
    BrowserAmbient.OnAmbientPropertyChange(DISPID_AMBIENT_USERAGENT);
end;
end.
