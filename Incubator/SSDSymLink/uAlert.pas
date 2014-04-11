unit uAlert;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ClipBrd;

procedure AlertCreate(Sender: TForm; Msg: String);

type
  TfAlert = class(TForm)
    GroupBox1: TGroupBox;
    lMessage: TLabel;
    tTransparent: TTimer;
    procedure tTransparentTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lMessageClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fAlert: TfAlert;
  Waittime: integer;  
  alpha: integer;

implementation

{$R *.dfm}

procedure AlertCreate(Sender: TForm; Msg: String);
begin
  if fAlert <> Nil then FreeAndNil(fAlert);
  try
    fAlert := TfAlert.Create(Sender);
    fAlert.lMessage.Caption := Msg;
    ClipBoard.AsText := Msg;
    fAlert.ShowModal;
  finally
    FreeAndNil(fAlert);
  end;
end;

procedure TfAlert.FormCreate(Sender: TObject);
begin
  waittime := 0;
  alpha := 0;
end;

procedure TfAlert.lMessageClick(Sender: TObject);
begin
  if lMessage.Caption[1] <> 'S' then Close;
end;

procedure TfAlert.tTransparentTimer(Sender: TObject);
begin
  if waittime = 0 then
  begin
    alpha := alpha + 1;
    AlphaBlendValue := 255 - round(sqr(alpha) / (sqr(Length(lMessage.Caption)) / 80));
    if AlphaBlendValue < 150 then Close;
  end
  else waittime := waittime - 1;
end;

end.
