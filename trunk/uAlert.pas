unit uAlert;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ClipBrd, uLanguageSettings;

procedure AlertCreate(Sender: TForm; Msg: String);

type
  TfAlert = class(TForm)
    gBorder: TGroupBox;
    lMessage: TLabel;
    tTransparent: TTimer;
    iBG: TImage;
    procedure tTransparentTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lMessageClick(Sender: TObject);
  private
    Waittime: integer;
    alpha: integer;
  public
    { Public declarations }
  end;

var
  fAlert: TfAlert;

implementation

{$R *.dfm}

procedure AlertCreate(Sender: TForm; Msg: String);
begin
  if fAlert <> Nil then FreeAndNil(fAlert);
  try
    fAlert := TfAlert.Create(Sender);
    fAlert.lMessage.Caption := Msg;
    fAlert.ShowModal;
  finally
    FreeAndNil(fAlert);
  end;
end;

procedure TfAlert.FormCreate(Sender: TObject);
begin
  Waittime := 0;
  alpha := 0;
end;

procedure TfAlert.lMessageClick(Sender: TObject);
begin
  if lMessage.Caption[1] <> 'S' then Close;
end;

procedure TfAlert.tTransparentTimer(Sender: TObject);
begin
  if Waittime = 0 then
  begin
    alpha := alpha + 1;
    AlphaBlendValue := 255 - round(sqr(alpha) / (sqr(Length(lMessage.Caption)) / 80));
    if AlphaBlendValue < 150 then Close;
  end
  else Waittime := Waittime - 1;
end;

end.
