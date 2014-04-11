unit uBrowser;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.OleCtrls, SHDocVw, uLanguageSettings;

procedure BrowserCreate(Sender: TForm);

type
  TfBrowser = class(TForm)
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure wbPluginNavigateComplete2(ASender: TObject;
      const pDisp: IDispatch; const URL: OleVariant);
  private
    wbPlugin: TWebBrowser;
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
  wbPlugin := TWebBrowser.Create(self);
  wbPlugin.SetParentComponent(self);

  wbPlugin.Navigate(AddrSecureErase[CurrLang]);
end;

procedure TfBrowser.FormResize(Sender: TObject);
begin
  wbPlugin.Width := ClientWidth;
  wbPlugin.Height := ClientHeight;
end;

procedure TfBrowser.wbPluginNavigateComplete2(ASender: TObject;
  const pDisp: IDispatch; const URL: OleVariant);
begin
  Caption := wbPlugin.OleObject.Document.Title;
end;

end.
