unit uBrowser;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, uUAWebbrowser,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.OleCtrls, SHDocVw,
  uLanguageSettings;

procedure BrowserCreate(Sender: TForm);

const
  WM_AFTER_SHOW = WM_USER + 300;

type
  TfBrowser = class(TForm)
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure wbPluginNavigateComplete2(ASender: TObject;
      const pDisp: IDispatch; const URL: OleVariant);     
    procedure WmAfterShow(var Msg: TMessage); message WM_AFTER_SHOW;
    procedure FormShow(Sender: TObject);
  private
    wbPlugin: TUAWebBrowser;
  public
    { Public declarations }
  end;

var
  fBrowser: TfBrowser;

implementation

{$R *.dfm}

procedure BrowserCreate(Sender: TForm);
begin
  if fBrowser <> Nil then FreeAndNil(fBrowser);
  try
    fBrowser := TfBrowser.Create(Sender);
    fBrowser.ShowModal;
  finally
    FreeAndNil(fBrowser);
  end;
end;

procedure TfBrowser.FormCreate(Sender: TObject);
begin
  wbPlugin := TUAWebBrowser.Create(self);
  wbPlugin.SetParentComponent(self);
  wbPlugin.Navigate('about:blank');
  wbPlugin.OnNavigateComplete2 := wbPluginNavigateComplete2;
end;

procedure TfBrowser.FormResize(Sender: TObject);
begin
  wbPlugin.Width := ClientWidth;
  wbPlugin.Height := ClientHeight;
end;

procedure TfBrowser.FormShow(Sender: TObject);
begin
  PostMessage(Self.Handle, WM_AFTER_SHOW, 0, 0);
end;

procedure TfBrowser.wbPluginNavigateComplete2(ASender: TObject;
  const pDisp: IDispatch; const URL: OleVariant);
begin
  Caption := wbPlugin.OleObject.Document.Title;
end;

procedure TfBrowser.WmAfterShow(var Msg: TMessage);
begin
  wbPlugin.Navigate(AddrSecureErase[CurrLang]);
end;

end.
