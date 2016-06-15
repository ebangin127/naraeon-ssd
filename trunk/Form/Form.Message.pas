unit Form.Message;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Vcl.ExtCtrls;

procedure MsgboxCreate(Sender: TForm; const FileName: String);

type
  TfMessage = class(TForm)
    MMessage: TMemo;
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    LogDevice: String;
  end;

var
  fMessage: TfMessage;

implementation

{$R *.dfm}

procedure MsgboxCreate(Sender: TForm; const FileName: String);
begin
  fMessage := TfMessage.Create(Sender);
  if fMessage <> Nil then FreeAndNil(fMessage);
  try
    fMessage.MMessage.Lines.LoadFromFile(FileName);
    DeleteFile(FileName);
    fMessage.ShowModal;
  finally
    FreeAndNil(fMessage);
  end;
end;

procedure TfMessage.FormResize(Sender: TObject);
begin
  MMessage.Width := ClientWidth - 16;
  MMessage.Height := ClientHeight - 16;
end;

end.
